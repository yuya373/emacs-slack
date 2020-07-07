;;; slack-message-buffer.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'slack-room)
(require 'slack-util)
(require 'slack-room-buffer)
(require 'slack-buffer)
(require 'slack-request)
(require 'slack-action)
(require 'slack-message-sender)
(require 'slack-thread-message-buffer)
(require 'slack-room-message-compose-buffer)
(require 'slack-pinned-items-buffer)
(require 'slack-user-profile-buffer)
(require 'slack-mrkdwn)
(require 'slack-modeline)
(require 'slack-message-notification)

(defvar slack-completing-read-function)
(defvar slack-channel-button-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-message-display-room)
    (define-key keymap [mouse-1] #'slack-message-display-room)
    keymap))

(defvar slack-open-direct-message-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
      #'slack-user-profile-buffer-display-im)
    map))

(defface slack-new-message-marker-face
  '((t (:foreground "#d33682"
                    :weight bold
                    :height 0.8)))
  "Face used to New Message Marker."
  :group 'slack)

(define-derived-mode slack-message-buffer-mode slack-mode "Slack Message Buffer"
  (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
  (add-hook 'lui-pre-output-hook 'slack-add-face-lazy nil t)
  (add-hook 'lui-pre-output-hook 'slack-mrkdwn-add-face nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (add-hook 'lui-pre-output-hook 'slack-display-inline-action t t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-user-name nil t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-conversation-name nil t)
  ;; TODO move to `slack-room-buffer' ?
  (cursor-sensor-mode)
  (setq-local lui-max-buffer-size nil)
  )

(defclass slack-message-buffer (slack-room-buffer)
  ((oldest :initform nil :type (or null string))
   (latest :initform nil :type (or null string))
   (marker-overlay :initform nil)
   (update-mark-timer :initform '(nil . nil)) ;; (timestamp . timer)
   (cursor-event-prev-ts :initform nil :type (or null string))
   (cursor :initarg :cursor :initform "" :type string)
   ))

(cl-defmethod slack-buffer-key ((_class (subclass slack-message-buffer)) room)
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-message-buffer))
  (slack-buffer-key 'slack-message-buffer (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-message-buffer)))
  'slack-message-buffer)

(cl-defmethod slack-buffer-name ((this slack-message-buffer))
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (room-name (slack-room-name room team)))
      (format  "*Slack - %s : %s"
               (oref team name)
               room-name)))

(cl-defmethod slack-buffer-last-read ((this slack-message-buffer))
  (oref (slack-buffer-room this) last-read))

(cl-defmethod slack-buffer-update-mark ((this slack-message-buffer) &key (force nil))
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (update-mark-timer (oref this update-mark-timer))
         (ts (slack-get-ts))
         (timer-timeout-sec (or (and force 0) 3))
         (prev-mark (or (car update-mark-timer)
                        (slack-buffer-last-read this)))
         (prev-timer (cdr update-mark-timer)))
    (when (or force (or (string< prev-mark ts)
                        (string= prev-mark ts)))
      (slack-log (format "%s: update mark to %s" (slack-room-name room team) ts)
                 (slack-buffer-team this))
      (when (timerp prev-timer)
        (cancel-timer prev-timer))
      (cl-labels
          ((update-mark ()
                        (slack-buffer-update-mark-request this ts)))
        (oset this
              update-mark-timer
              (cons ts (run-at-time timer-timeout-sec nil #'update-mark)))))))

(cl-defmethod slack-buffer-update-mark-request ((this slack-message-buffer) ts &optional after-success)
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (when (slack-room-member-p room)
      (oset room last-read ts)
      (slack-buffer-update-marker-overlay this)

      (cl-labels ((on-update-mark (&key data &allow-other-keys)
                                  (slack-request-handle-error
                                   (data "slack-buffer-update-mark-request")
                                   (when (functionp after-success)
                                     (funcall after-success)))))
        (with-slots (id) room
          (slack-request
           (slack-request-create
            (slack-room-update-mark-url room)
            team
            :type "POST"
            :params (list (cons "channel"  id)
                          (cons "ts"  ts))
            :success #'on-update-mark)))))))

(cl-defmethod slack-buffer-send-message ((this slack-message-buffer) message)
  (slack-message-send-internal message
                               (slack-buffer-room this)
                               (slack-buffer-team this)))

(cl-defmethod slack-buffer-latest-ts ((this slack-message-buffer))
  (slack-room-latest (slack-buffer-room this) (slack-buffer-team this)))

(cl-defmethod slack-buffer-buffer ((this slack-message-buffer))
  (let ((buffer-already-exists-p (get-buffer (slack-buffer-name this)))
        (buffer (cl-call-next-method))
        (last-read (slack-buffer-last-read this)))
    (with-current-buffer buffer
      (if (slack-team-mark-as-read-immediatelyp (slack-buffer-team this))
          (progn
            (unless buffer-already-exists-p
              (goto-char (marker-position lui-input-marker)))
            (and (slack-buffer-latest-ts this)
                 (slack-buffer-update-mark-request this
                                                   (slack-buffer-latest-ts this))))


        (unless buffer-already-exists-p
          (when (or (string= "0" last-read)
                    (null (slack-buffer-goto last-read)))
            (goto-char (point-max))))

        (unless (string= "0" last-read)
          (slack-buffer-update-marker-overlay this))))

    buffer))

(cl-defmethod slack-buffer-visible-message-p ((this slack-message-buffer) message)
  (slack-message-visible-p message (slack-buffer-team this)))

(cl-defmethod slack-buffer-insert-messages ((this slack-message-buffer) messages
                                            &optional filter-by-oldest not-tracked-p)
  (with-slots (latest oldest) this
    (let* ((latest-message (car (last messages)))
           (oldest-message (car messages)))
      (cl-loop for m in messages
               with prev-message = nil
               do (when (and (if filter-by-oldest
                                 (or (null oldest)
                                     (string< (slack-ts m) oldest))
                               (or (null latest)
                                   (string< latest (slack-ts m))))
                             (slack-buffer-visible-message-p this m))
                    (slack-buffer-insert this m not-tracked-p prev-message)
                    (setq prev-message m)))
      (when latest-message
        (slack-buffer-update-lastest this (slack-ts latest-message)))
      (when oldest-message
        (slack-buffer-update-oldest this oldest-message)))))

(cl-defmethod slack-buffer-init-buffer ((this slack-message-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char (point-min))

      (slack-buffer-insert-load-more this)

      (let* ((room (slack-buffer-room this))
             (messages (slack-room-sorted-messages room)))
        (slack-buffer-insert-messages this messages nil t)))
    buf))

(cl-defmethod slack-buffer-update ((this slack-message-buffer) message &key replace)
  (let ((team (slack-buffer-team this))
        (buffer (slack-buffer-buffer this)))
    (when (and (slack-team-mark-as-read-immediatelyp team)
               (slack-buffer-in-current-frame buffer))
      (slack-buffer-update-mark-request this (slack-ts message)))

    (if replace (slack-buffer-replace this message)
      (with-current-buffer buffer
        (slack-buffer-insert-messages this (list message))))))

(cl-defmethod slack-buffer-display-message-compose-buffer ((this slack-message-buffer))
  (let ((buf (slack-create-room-message-compose-buffer (slack-buffer-room this)
                                                       (slack-buffer-team this))))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-update-lastest ((this slack-message-buffer) latest)
  (with-slots ((prev-latest latest)) this
    (if (or (null prev-latest)
            (string< prev-latest latest))
        (setq prev-latest latest))))

(cl-defmethod slack-create-message-buffer ((room slack-room) cursor team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer team room)))
      buffer
    (slack-message-buffer :room-id (oref room id) :team-id (oref team id) :cursor cursor)))


(cl-defmethod slack-buffer-update-oldest ((this slack-message-buffer) message)
  (when (and message (or (null (oref this oldest))
                         (string< (slack-ts message) (oref this oldest))))
    (oset this oldest (slack-ts message))))

(cl-defmethod slack-buffer-load-missing-messages ((this slack-message-buffer))
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (latest-message (car (last (slack-room-sorted-messages room))))
         (latest (and latest-message (slack-ts latest-message))))

    (cl-labels
        ((paginate (cursor)
                   (slack-conversations-view room team
                                             :oldest latest
                                             :cursor cursor
                                             :after-success #'after-success))
         (after-success (messages next-cursor)
                        (slack-room-set-messages room messages team)
                        (if (and next-cursor (< 0 (length next-cursor)))
                            (paginate next-cursor)
                          (write-messages)))
         (write-messages ()
                         (let* ((messages (slack-room-sorted-messages room)))
                           (with-current-buffer (slack-buffer-buffer this)
                             (let ((inhibit-read-only t))
                               (slack-buffer-delete-overlay this))
                             (slack-buffer-insert-messages this messages nil t)
                             (slack-buffer-goto (slack-buffer-last-read this))
                             (slack-buffer-update-marker-overlay this)))))
      (slack-conversations-view room team
                                :oldest latest
                                :after-success #'after-success))))

(cl-defmethod slack-buffer-load-more ((this slack-message-buffer))
  (let ((oldest (oref this oldest))
        (team (slack-buffer-team this))
        (room (slack-buffer-room this))
        (current-ts (let ((change (next-single-property-change (point) 'ts)))
                      (when change
                        (get-text-property change 'ts))))
        (cur-point (point)))
    (cl-labels
        ((update-buffer
          (messages)
          (with-current-buffer (slack-buffer-buffer this)
            (slack-buffer-widen
             (let ((inhibit-read-only t))
               (goto-char (point-min))

               (slack-if-let* ((loading-message-end
                                (slack-buffer-loading-message-end-point this)))
                   (progn
                     (slack-buffer-delete-overlay this)
                     (delete-region (point-min) loading-message-end))
                 (message "loading-message-end not found, oldest: %s" oldest))

               (set-marker lui-output-marker (point-min))
               (if (< 0 (length (oref this cursor)))
                   (slack-buffer-insert-load-more this)
                 (let ((lui-time-stamp-position nil))
                   (lui-insert "(no more messages)" t)))

               (slack-buffer-insert-messages this messages t t)
               (lui-recover-output-marker)
               (slack-buffer-update-marker-overlay this)
               ))
            (if current-ts
                (slack-buffer-goto current-ts)
              (goto-char cur-point))))
         (after-success (messages next-cursor)
                        (oset this cursor next-cursor)
                        (slack-room-set-messages room messages team)
                        (update-buffer (slack-room-sorted-messages room))))
      (slack-conversations-view room team
                                :cursor (oref this cursor)
                                :after-success #'after-success))))

(cl-defmethod slack-buffer-display-pins-list ((this slack-message-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (slack-pins-list
     room team
     #'(lambda (items)
         (let* ((buf (slack-create-pinned-items-buffer
                      room team items)))
           (slack-buffer-display buf))))))

(cl-defmethod slack-buffer-display-user-profile ((this slack-message-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (cl-labels
        ((success (members next-cursor)
                  (let ((candidates (cl-remove-if #'null
                                                  (cl-loop for member in members
                                                           collect (slack-if-let*
                                                                       ((user (slack-user--find member team))
                                                                        (not-hidden (not (slack-user-hidden-p user))))
                                                                       (cons (slack-user-label user team)
                                                                             user)))))
                        (selected nil))
                    (when (< 0 (length next-cursor))
                      (setq candidates
                            (append candidates
                                    (list (cons slack-next-page-token
                                                slack-next-page-token)))))
                    (setq selected (completing-read "Select user: " candidates nil t))
                    (if (equal slack-next-page-token selected)
                        (request next-cursor)
                      (slack-if-let* ((candidate (cl-assoc selected candidates :test #'string=))
                                      (user (cdr-safe candidate)))
                          (slack-buffer-display
                           (slack-create-user-profile-buffer
                            team
                            (plist-get user :id)))))))
         (request (cursor)
           (slack-conversations-members
            room team cursor #'success)))
      (request nil))))

(cl-defmethod slack-buffer-delete-overlay ((this slack-message-buffer))
  (when (oref this marker-overlay)
    (delete-overlay (oref this marker-overlay))))

(cl-defmethod slack-buffer-update-marker-overlay ((this slack-message-buffer))
  (let ((buf (get-buffer (slack-buffer-name this))))
    (and buf (with-current-buffer buf
               (let* ((last-read (slack-buffer-last-read this))
                      (beg (slack-buffer-ts-eq (point-min) (point-max) last-read))
                      (end (and beg
                                (<= (point-min) beg)
                                (next-single-property-change beg 'ts))))
                 (when (and beg end
                            (<= end (point-max)))
                   (let ((overlays (overlay-lists))
                         (overlay-props 'slack-new-message-marker-overlay)
                         (after-string (propertize "New Message"
                                                   'face
                                                   'slack-new-message-marker-face)))
                     (dolist (ov (append (car overlays)
                                         (cdr overlays)))
                       (when (overlay-get ov overlay-props)
                         (delete-overlay ov)))

                     (oset this marker-overlay (make-overlay beg end))
                     (overlay-put (oref this marker-overlay)
                                  overlay-props
                                  t)
                     (overlay-put (oref this marker-overlay)
                                  'after-string
                                  (format "%s\n" after-string)))))))))

(cl-defmethod slack-file-upload-params ((this slack-message-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (list (cons "channels"
                (mapconcat #'identity
                           (slack-file-select-sharing-channels
                            (slack-room-label room team)
                            team)
                           ",")))))

(cl-defmethod slack-buffer-next-message ((this slack-message-buffer) message)
  (let ((room (slack-buffer-room this))
        (ts (slack-ts message)))
    (cl-loop for m in (reverse (slack-room-sorted-messages room))
             with next = nil
             do (when (slack-buffer-visible-message-p this m)
                  (if (string= (slack-ts m) ts)
                      (cl-return next)
                    (setq next m))))))

(cl-defmethod slack-buffer-prev-message ((this slack-message-buffer) message)
  (let* ((room (slack-buffer-room this))
         (current-ts (slack-ts message)))
    (cl-loop for ts in (reverse (oref room message-ids))
             do (when (string< ts current-ts)
                  (slack-if-let* ((message (slack-room-find-message room ts))
                                  (visible-p (slack-buffer-visible-message-p this message)))
                      (cl-return message))))))

(cl-defmethod slack-buffer-merge-message-p ((this slack-message-buffer) message prev)
  (let ((team (slack-buffer-team this)))
    (and prev
         (and (not (slack-message-starred-p message))
              (not (slack-message-starred-p prev))
              (not (slack-reply-broadcast-message-p message))
              (null (oref message thread-ts))
              (null (oref prev thread-ts))
              (let ((prev-user (slack-user-find prev team))
                    (current-user (slack-user-find message team)))
                (and (not (null prev-user))
                     (not (null current-user))
                     (equal prev-user current-user)))
              (let ((prev-day (time-to-day-in-year (slack-message-time-stamp
                                                    prev)))
                    (current-day (time-to-day-in-year (slack-message-time-stamp
                                                       message))))
                (eq prev-day current-day))))))

(cl-defmethod slack-buffer-message-text ((this slack-message-buffer) message merge-message-p)
  (let* ((team (slack-buffer-team this))
         (text (slack-message-to-string message team)))
    (or (and merge-message-p
             (slack-if-let*
                 ((end (next-single-property-change
                        0 'slack-message-header text)))
                 (substring text (1+ end))))
        text)))

(cl-defmethod slack-buffer-insert ((this slack-message-buffer) message
                                   &optional not-tracked-p prev-message)
  (let* ((lui-time-stamp-time (slack-message-time-stamp message))
         (ts (slack-ts message))
         (prev (or prev-message (slack-buffer-prev-message this message)))
         (merge-message-p (slack-buffer-merge-message-p this message prev))
         (text (slack-buffer-message-text this message merge-message-p)))
    (when merge-message-p
      (save-excursion
        (goto-char lui-output-marker)
        (slack-if-let*
            ((inhibit-read-only t)
             (ts (slack-ts prev))
             (prev-message-end (cl-loop for i from (marker-position lui-output-marker) downto (point-min)
                                        if (string= (get-text-property i 'ts)
                                                    ts)
                                        return i)))
            (delete-region (1+ prev-message-end)
                           (marker-position lui-output-marker)))))
    (lui-insert-with-text-properties
     text
     'merged-message-p merge-message-p
     'not-tracked-p not-tracked-p
     'ts ts
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)))

(cl-defmethod slack-buffer-replace ((this slack-message-buffer) message)
  (with-current-buffer (slack-buffer-buffer this)
    (let* ((prev (slack-buffer-prev-message this message))
           (merge-message-p (slack-buffer-merge-message-p this message prev))
           (ts (slack-ts message))
           (text (slack-buffer-message-text this
                                            message
                                            merge-message-p)))
      (save-excursion
        (goto-char (point-max))
        (while (> (lui-backward-message) (point-min))
          (when (equal (get-text-property (point) 'ts) ts)
            (let ((merged-message-p
                   (get-text-property (point) 'merged-message-p)))
              (when (and (not merge-message-p) merged-message-p)
                (unwind-protect
                    (progn
                      (setq lui-output-marker (point-marker))
                      (let ((lui-time-stamp-position nil))
                        (lui-insert "" t)))
                  (lui-recover-output-marker))
                (goto-char (next-single-char-property-change
                            (point) 'lui-message-id)))

              (when (and (not merged-message-p) merge-message-p)
                (let ((beg (previous-single-property-change
                            (point) 'lui-message-id))
                      (inhibit-read-only t))
                  (when beg
                    (delete-region beg (point))))))

            (lui-replace-message text)
            (let ((inhibit-read-only t)
                  (end (next-single-property-change
                        (point) 'lui-message-id)))
              (when end
                (put-text-property (point) end
                                   'merged-message-p
                                   merge-message-p)))
            (slack-if-let*
                ((next (slack-buffer-next-message this
                                                  message))

                 (next-message-point
                  (slack-buffer-ts-eq (point) (point-max)
                                      (slack-ts next))))
                (let ((merged-message-p
                       (get-text-property next-message-point
                                          'merged-message-p))
                      (merge-message-p
                       (slack-buffer-merge-message-p this
                                                     next
                                                     message)))

                  (unless (eq merged-message-p merge-message-p)
                    (slack-buffer-replace this next)))))))))

  (slack-buffer-update-marker-overlay this))

(defun slack-message-buffer-detect-ts-changed ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (message-buffer-p (eq 'slack-message-buffer
                                        (eieio-object-class buffer)))
                  (current-ts (slack-get-ts))
                  (team (slack-buffer-team buffer)))
      (let ((prev-ts (oref buffer cursor-event-prev-ts)))
        (when (or (null prev-ts)
                  (not (string= prev-ts current-ts)))
          (oset buffer cursor-event-prev-ts current-ts)

          (when (slack-team-animate-image-p team)
            (slack-buffer-animate-image current-ts)
            (slack-buffer-cancel-animate-image prev-ts))

          (unless (slack-team-mark-as-read-immediatelyp team)
            (slack-buffer-update-mark buffer))))))

(defun slack-buffer-get-images (ts)
  (when ts
    (slack-if-let* ((room (slack-buffer-room slack-current-buffer))
                    (beg (slack-buffer-ts-eq (point-min) (point-max) ts))
                    (end (or (slack-buffer-next-point beg (point-max) ts)
                             (point-max))))

        (let ((images (make-hash-table :test 'equal))
              (current beg))
          (while (<= current end)
            (let ((prop (or (get-text-property current
                                               'emojify-display)
                            (get-text-property current
                                               'slack-image-display))))
              (when prop
                (let ((image (if (eq 'image (car prop))
                                 prop
                               (cl-find-if #'(lambda (e)
                                               (and (listp e)
                                                    (eq 'image (car e))))
                                           prop))))
                  (puthash (plist-get (cdr image) :file)
                           image
                           images))))
            (setq current (1+ current)))
          (hash-table-values images)))))

(defun slack-buffer-animate-image (ts)
  (when (display-graphic-p)
    (slack-if-let* ((images (slack-buffer-get-images ts)))
        (cl-loop for image in images
                 do (when (and image (image-multi-frame-p image))
                      (let* ((metadata (image-metadata image))
                             (count (plist-get metadata 'count)))
                        (if (< 200 count)
                            (slack-if-let* ((buffer slack-current-buffer)
                                            (team (slack-buffer-team buffer)))
                                (slack-log (format "Image too big to animate. metadata: %s"
                                                   metadata)
                                           team :level 'debug))
                          (image-animate image nil t))))))))

(defun slack-buffer-cancel-animate-image (ts)
  (when (and ts (display-graphic-p))
    (slack-if-let* ((images (slack-buffer-get-images ts)))
        (cl-loop for image in images
                 do (when (and image (image-multi-frame-p image))
                      (let ((timer (image-animate-timer image)))
                        (when (and timer (timerp timer))
                          (cancel-timer timer))))))))

(cl-defmethod slack-buffer--subscribe-cursor-event ((this slack-message-buffer)
                                                    _window
                                                    _prev-point
                                                    type)
  (cond
   ((eq type'entered)
    (add-hook 'post-command-hook
              #'slack-message-buffer-detect-ts-changed
              t t)
    (slack-message-buffer-detect-ts-changed))
   ((eq type 'left)
    (let ((prev-ts (oref this cursor-event-prev-ts)))
      (slack-buffer-cancel-animate-image prev-ts))
    (oset this cursor-event-prev-ts nil)
    (remove-hook 'post-command-hook
                 #'slack-message-buffer-detect-ts-changed
                 t))))

(defun slack-room-unread-threads ()
  (interactive)
  (error "Deprecated.  use `slack-all-threads instead'"))

(defvar slack-message-thread-status-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'slack-thread-show-or-create)
    (define-key map (kbd "RET") #'slack-thread-show-or-create)
    map))

(defun slack-room-pins-list ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-pins-list buf)))

(defun slack-room-user-select ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-user-profile buf)))

(defun slack-room-display (room team)
  (cl-labels
      ((open (buf)
             (slack-buffer-display buf)))
    (let ((buf (slack-buffer-find 'slack-message-buffer team room)))
      (if buf (open buf)
        (message "No Message in %s, fetching from server..." (slack-room-name room team))
        (slack-room-clear-messages room)
        (slack-conversations-view
         room team
         :after-success #'(lambda (messages cursor)
                            (slack-room-set-messages room messages team)
                            (open (slack-create-message-buffer room cursor team))))))))

(cl-defmethod slack-room-update-buffer ((this slack-room) team message replace)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer team this)))
      (slack-buffer-update buffer message :replace replace)
    (and slack-buffer-create-on-notify
         (slack-conversations-view
          this team
          :after-success #'(lambda (messages cursor)
                             (slack-room-set-messages this messages team)
                             (tracking-add-buffer
                              (slack-buffer-buffer
                               (slack-create-message-buffer this cursor team))
                              (slack-messages-tracking-faces messages this team)))))))

(defun slack-select-unread-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (cl-remove-if
                                 #'(lambda (room)
                                     (not (slack-room-has-unread-p room team)))
                                 (append (slack-team-ims team)
                                         (slack-team-groups team)
                                         (slack-team-channels team))))
                team)))
    (slack-room-display room team)))

(defun slack-select-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (append (slack-team-ims team)
                                        (slack-team-groups team)
                                        (slack-team-channels team)))
                team)))
    (slack-room-display room team)))

(defun slack-message-redisplay ()
  (interactive)
  (slack-if-let* ((ts (slack-get-ts))
                  (buf slack-current-buffer))
      (slack-buffer--replace buf ts)))

(defun slack-message-inspect ()
  (interactive)
  (slack-if-let* ((ts (slack-get-ts))
                  (buffer slack-current-buffer)
                  (team (slack-buffer-team buffer))
                  (room (slack-buffer-room buffer))
                  (message (slack-room-find-message room ts))
                  (text (slack-message--inspect message room team)))
      (message "%s" text)))

(cl-defmethod slack-message--inspect ((this slack-message) room team)
  (format "RAW: %s\nROOM: %s\nUSER: %s\nBOT: %S\nMESSAGE: %s\nATTACHMENTS: %s - %s\nFILES: %s - %s\nUSER_IDS: %s"
          (oref this text)
          (oref room id)
          (oref this user)
          (and (slot-exists-p this 'bot-id)
               (slot-boundp this 'bot-id)
               (oref this bot-id))
          (eieio-object-class this)
          (length (oref this attachments))
          (mapcar (lambda (e) (format "\n(CLASS: %s\nTITLE: %s\nPRETEXT: %s\nTEXT: %s\nIMAGE: %s\nTHUMBNAIL: %s\nFILES:%s)"
                                      (eieio-object-class e)
                                      (slack-unescape-channel
                                       (or (oref e title) "")
                                       team)
                                      (oref e pretext)
                                      (oref e text)
                                      (oref e image-url)
                                      (oref e thumb-url)
                                      (length (oref e files))))
                  (oref this attachments))
          (length (oref this files))
          (mapcar (lambda (e) (format "(TITLE: %s)"
                                      (oref e title)))
                  (oref this files))
          (slack-message-user-ids this)))

(defun slack-message-update-mark ()
  "Update Channel's last-read marker to this message."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-update-mark buffer :force t)))

(cl-defmethod slack-thread-message-update-buffer ((message slack-message) room team replace)
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer team room (slack-thread-ts message))))
      (slack-buffer-update buf message :replace replace)
    (and slack-buffer-create-on-notify
         (cl-labels ((after-success (_next-cursor has-more)
                                    (tracking-add-buffer (slack-buffer-buffer
                                                          (slack-create-thread-message-buffer
                                                           room team (slack-thread-ts message) has-more))
                                                         slack-message-tracking-faces)))
           (slack-thread-replies message room team
                                 :after-success #'after-success)))))

(cl-defmethod slack-message-update-buffer ((this slack-message) team)
  (slack-if-let* ((room (slack-room-find this team)))
      (progn
        (when (slack-message-visible-p this team)
          (slack-room-update-buffer room team this nil))
        (when (slack-thread-message-p this)
          (slack-thread-message-update-buffer this room team nil)))))

(cl-defmethod slack-message-replace-buffer ((this slack-message) team)
  (slack-if-let* ((room (slack-room-find this team)))
      (progn
        (slack-room-update-buffer room team this t)
        (when (slack-thread-message-p this)
          (slack-thread-message-update-buffer this room team t)))))

(cl-defmethod slack-message-replace-buffer ((this slack-file) team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer team this)))
      (progn
        (oset buffer file this)
        (slack-buffer-update buffer)))
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer team)))
      (slack-buffer-update buffer this :replace t)))

(defun slack-message-remove-star ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-remove-star buffer (slack-get-ts))))

(defun slack-message-add-star ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-add-star buffer (slack-get-ts))))

(defun slack-message-pins-add ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-pins-add buf (slack-get-ts))))

(defun slack-message-pins-remove ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-pins-remove buf (slack-get-ts))))

(defun slack-message-add-reaction ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (reaction (slack-message-reaction-input team)))
      (slack-buffer-add-reaction-to-message buf
                                            reaction
                                            (slack-get-ts))))

(defun slack-message-remove-reaction ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-remove-reaction-from-message buf
                                                 (slack-get-ts))))

(defun slack-message-display-room ()
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (slack-buffer-team buffer))
       (room-id (get-text-property (point) 'room-id))
       (room (slack-room-find room-id team)))
      (slack-room-display room team)))

(defun slack-im-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (candidates (cl-loop for team in (list team)
                              for ims = (cl-remove-if #'(lambda (im)
                                                          (not (oref im is-open)))
                                                      (slack-team-ims team))
                              nconc ims))
         (room (slack-room-select candidates team)))
    (slack-room-display room team)))

(defun slack-group-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for groups = (slack-team-groups team)
                         nconc groups)
                team)))
    (slack-room-display room team)))

(defun slack-channel-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for channels = (slack-team-channels team)
                         nconc channels)
                team)))
    (slack-room-display room team)))

(cl-defmethod slack-buffer-display-im ((this slack-user-profile-buffer))
  (let* ((user-id (oref this user-id))
         (team (slack-buffer-team this))
         (im (slack-im-find-by-user-id user-id team)))
    (slack-room-display im team)))

(defun slack-user-profile-buffer-display-im ()
  "Display im buffer from user profile buffer."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-im buf)))

(defun slack-message-share ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-share-message buf (slack-get-ts))))

(defun slack-message-write-another-buffer ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-message-compose-buffer buf)))

(defun slack-message-edit ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-edit-message-buffer buf (slack-get-ts))))

(defun slack-thread-show-or-create ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (if (slack-thread-message-buffer-p buf)
          (error "Already in thread")
        (slack-buffer-display-thread buf (slack-get-ts)))))

(cl-defmethod slack-buffer-display-thread ((this slack-message-buffer) ts)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-if-let* ((thread-ts (slack-thread-ts message)))
          (slack-thread-show-messages message room team)
        (slack-buffer-start-thread this message ts))))

(cl-defmethod slack-buffer-start-thread ((this slack-message-buffer) message ts)
  (when (slack-reply-broadcast-message-p message)
    (error "Can't start thread from broadcasted message"))
  (let ((buf (slack-create-thread-message-buffer (slack-buffer-room this)
                                                 (slack-buffer-team this)
                                                 ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-thread-show-messages ((this slack-message) room team)
  (cl-labels
      ((after-success (_next-cursor has-more)
                      (let ((buf (slack-create-thread-message-buffer
                                  room team (slack-thread-ts this) has-more)))
                        (slack-buffer-display buf))))
    (slack-thread-replies this room team
                          :after-success #'after-success)))

(defun slack-advice-delete-window (&optional window)
  (let ((buf (window-buffer window)))
    (with-current-buffer buf
      (slack-if-let* ((buffer slack-current-buffer))
          (slack-buffer--subscribe-cursor-event buffer
                                                nil
                                                nil
                                                'left)))))

(defun slack-advice-select-window (org-func window &optional norecord)
  (slack-if-let* ((win (selected-window))
                  (live-p (window-live-p win))
                  (buf (window-buffer win)))
      (with-current-buffer buf
        (slack-if-let* ((buffer slack-current-buffer))
            (slack-buffer--subscribe-cursor-event buffer
                                                  nil
                                                  nil
                                                  'left))))
  (prog1
      (funcall org-func window norecord)
    (slack-if-let* ((win (selected-window))
                    (live-p (window-live-p win))
                    (buf (window-buffer win)))
        (with-current-buffer buf
          (slack-if-let* ((buffer slack-current-buffer))
              (slack-buffer--subscribe-cursor-event buffer
                                                    nil
                                                    nil
                                                    'entered))))))

(advice-add 'select-window :around 'slack-advice-select-window)
(advice-add 'delete-window :before 'slack-advice-delete-window)

(provide 'slack-message-buffer)
;;; slack-message-buffer.el ends here
