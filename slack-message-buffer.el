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

(eval-when-compile
  (require 'cl))
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
  (add-hook 'lui-pre-output-hook 'slack-search-code-block nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (add-hook 'lui-pre-output-hook 'slack-display-inline-action t t)
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

(cl-defmethod slack-buffer-last-read ((this slack-message-buffer))
  (with-slots (room) this
    (oref room last-read)))

(cl-defmethod slack-buffer-update-mark ((this slack-message-buffer) &key (force nil))
  (with-slots (room update-mark-timer team) this
    (let* ((ts (slack-get-ts))
           (timer-timeout-sec (or (and force 0) 5))
           (prev-mark (or (car update-mark-timer)
                          (slack-buffer-last-read this)))
           (prev-timer (cdr update-mark-timer)))
      (when (or force (or (string< prev-mark ts)
                          (string= prev-mark ts)))
        (slack-log (format "%s: update mark to %s"
                           (slack-room-name room team)
                           ts)
                   (oref this team))
        (when (timerp prev-timer)
          (cancel-timer prev-timer))
        (cl-labels
            ((update-mark ()
                          (slack-buffer-update-mark-request this ts)))
          (setq update-mark-timer
                (cons ts (run-at-time timer-timeout-sec nil #'update-mark))))))))

(cl-defmethod slack-buffer-update-mark-request ((this slack-message-buffer) ts &optional after-success)
  (with-slots (room team) this
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
  (with-slots (room team) this
    (slack-message-send-internal message room team)))

(cl-defmethod slack-buffer-latest-ts ((this slack-message-buffer))
  (with-slots (room) this
    (slack-if-let* ((latest (oref room latest)))
        (slack-ts latest))))

(cl-defmethod slack-buffer-buffer ((this slack-message-buffer))
  (let ((buffer-already-exists-p (get-buffer (slack-buffer-name this)))
        (buffer (cl-call-next-method))
        (last-read (slack-buffer-last-read this)))
    (with-current-buffer buffer
      (if (slack-team-mark-as-read-immediatelyp (oref this team))
          (progn
            (unless buffer-already-exists-p
              (goto-char (marker-position lui-input-marker)))
            (and (slack-buffer-latest-ts this)
                 (slack-buffer-update-mark-request this
                                                   (slack-buffer-latest-ts this))))
        (unless (string= "0" last-read)
          (unless buffer-already-exists-p
            (slack-buffer-goto last-read))
          (slack-buffer-update-marker-overlay this))))

    buffer))

(cl-defmethod slack-thread-title ((thread slack-thread) team)
  (with-slots (root) thread
    (let ((room (slack-room-find (oref root channel) team))
          (body (slack-message-body root team)))
      (when room
        (format "%s - %s" (slack-room-name room team)
                (concat (substring body 0 (min 50 (length body))) "..."))))))

(cl-defmethod slack-buffer-display-unread-threads ((this slack-message-buffer))
  (with-slots (room team) this
    (let* ((threads (mapcar #'(lambda (m) (oref m thread))
                            (cl-remove-if
                             #'(lambda (m)
                                 (or (not (slack-message-thread-parentp m))
                                     (not (< 0 (oref (oref m thread) unread-count)))))
                             (oref room messages))))
           (alist (mapcar #'(lambda (thread)
                              (cons (slack-thread-title thread team) thread))
                          (cl-sort threads
                                   #'string>
                                   :key #'(lambda (thread) (oref thread thread-ts)))))
           (selected (slack-select-from-list (alist "Select Thread: "))))
      (slack-thread-show-messages selected room team))))

(cl-defmethod slack-buffer-start-thread ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (buf (slack-create-thread-message-buffer room team ts)))
      (when (slack-reply-broadcast-message-p message)
        (error "Can't start thread from broadcasted message"))
      (slack-buffer-display buf))))

(cl-defmethod slack-buffer-major-mode ((_this slack-message-buffer))
  'slack-message-buffer-mode)

(cl-defmethod slack-buffer-visible-message-p ((_this slack-message-buffer) message)
  (or (not (slack-thread-message-p message))
      (slack-reply-broadcast-message-p message)))

(cl-defmethod slack-buffer-insert-messages ((this slack-message-buffer) messages
                                            &optional filter-by-oldest)
  (with-slots (room team latest oldest) this
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
                    (slack-buffer-insert this m nil prev-message)
                    (setq prev-message m)))
      (when latest-message
        (slack-buffer-update-lastest this (slack-ts latest-message)))
      (when oldest-message
        (slack-buffer-update-oldest this oldest-message)))))

(cl-defmethod slack-buffer-init-buffer ((this slack-message-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (funcall (slack-buffer-major-mode this))
      (slack-buffer-set-current-buffer this)
      (goto-char (point-min))

      (slack-buffer-insert-load-more this)

      (with-slots (room) this
        (let ((messages (slack-room-sorted-messages room)))
          (slack-buffer-insert-messages this messages))))

    (with-slots (room team) this
      (let* ((class (eieio-object-class-name this)))
        (slack-buffer-push-new-3 class room team)))
    buf))


(cl-defmethod slack-buffer-update ((this slack-message-buffer) message &key replace)
  (with-slots (room team) this
    (let ((buffer (get-buffer (slack-buffer-name this))))
      (when (and (slack-team-mark-as-read-immediatelyp team)
                 (slack-buffer-in-current-frame buffer))
        (slack-buffer-update-mark-request this (slack-ts message)))

      (if replace (slack-buffer-replace this message)
        (with-current-buffer buffer
          (slack-buffer-insert-messages this (list message)))))))

(cl-defmethod slack-buffer-display-message-compose-buffer ((this slack-message-buffer))
  (with-slots (room team) this
    (let ((buf (slack-create-room-message-compose-buffer room team)))
      (slack-buffer-display buf))))

(cl-defmethod slack-buffer-update-lastest ((this slack-message-buffer) latest)
  (with-slots ((prev-latest latest)) this
    (if (or (null prev-latest)
            (string< prev-latest latest))
        (setq prev-latest latest))))

(cl-defmethod slack-buffer-display-thread ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((thread (slack-room-find-thread room ts)))
      (if thread (slack-thread-show-messages thread room team)
        (slack-thread-start)))))

(cl-defmethod slack-create-message-buffer ((room slack-room) cursor team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer
                                             room
                                             team)))
      buffer
    (slack-message-buffer :room room :team team :cursor cursor)))


(cl-defmethod slack-buffer-update-oldest ((this slack-message-buffer) message)
  (when (and message (or (null (oref this oldest))
                         (string< (slack-ts message) (oref this oldest))))
    (oset this oldest (slack-ts message))))

(cl-defmethod slack-buffer-load-missing-messages ((this slack-message-buffer))
  (with-slots (room team) this
    (let* ((latest-message (car (last (slack-room-sorted-messages room))))
           (latest (and latest-message (slack-ts latest-message))))

      (cl-labels
          ((paginate (cursor)
                     (slack-conversations-history room team
                                                  :oldest latest
                                                  :cursor cursor
                                                  :after-success #'after-success))
           (after-success (messages next-cursor)
                          (slack-room-append-messages room messages)
                          (if (and next-cursor (< 0 (length next-cursor)))
                              (paginate next-cursor)
                            (write-messages)))
           (write-messages ()
                           (let* ((messages (slack-room-sorted-messages room)))
                             (with-current-buffer (slack-buffer-buffer this)
                               (let ((inhibit-read-only t))
                                 (slack-buffer-delete-overlay this))
                               (slack-buffer-insert-messages this messages)
                               (slack-buffer-goto (slack-buffer-last-read this))
                               (slack-buffer-update-marker-overlay this)))))
        (slack-conversations-history room team
                                     :oldest latest
                                     :after-success #'after-success)))))

(cl-defmethod slack-buffer-load-more ((this slack-message-buffer))
  (with-slots (room team oldest) this
    (let ((current-ts (let ((change (next-single-property-change (point) 'ts)))
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

                 (slack-buffer-insert-messages this messages t)
                 (lui-recover-output-marker)
                 (slack-buffer-update-marker-overlay this)
                 ))
              (if current-ts
                  (slack-buffer-goto current-ts)
                (goto-char cur-point))))
           (after-success (messages next-cursor)
                          (oset this cursor next-cursor)
                          (slack-room-prepend-messages room messages)
                          (update-buffer (slack-room-sorted-messages room))))
        (slack-conversations-history room team
                                     :cursor (oref this cursor)
                                     :after-success #'after-success)))))

(cl-defmethod slack-buffer-display-pins-list ((this slack-message-buffer))
  (with-slots (room team) this
    (cl-labels
        ((on-pins-list (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-room-pins-list")
                        (let* ((buf (slack-create-pinned-items-buffer
                                     room team (plist-get data :items))))
                          (slack-buffer-display buf)))))
      (slack-request
       (slack-request-create
        slack-room-pins-list-url
        team
        :params (list (cons "channel" (oref room id)))
        :success #'on-pins-list)))))

(cl-defmethod slack-buffer-display-user-profile ((this slack-message-buffer))
  (with-slots (room team) this
    (let* ((members (cl-remove-if
                     #'(lambda (e)
                         (or (slack-user-self-p e team)
                             (slack-user-hidden-p
                              (slack-user--find e team))))
                     (slack-room-get-members room)))
           (user-alist (mapcar #'(lambda (u) (cons (slack-user-name u team) u))
                               members))
           (user-id (if (eq 1 (length members))
                        (car members)
                      (slack-select-from-list (user-alist "Select User: ")))))
      (let ((buf (slack-create-user-profile-buffer team user-id)))
        (slack-buffer-display buf)))))

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
                   (if (oref this marker-overlay)
                       (move-overlay (oref this marker-overlay)
                                     beg end)
                     (progn
                       (oset this marker-overlay (make-overlay beg end))
                       (let ((after-string
                              (propertize "New Message"
                                          'face
                                          'slack-new-message-marker-face)))
                         (overlay-put (oref this marker-overlay)
                                      'after-string
                                      (format "%s\n" after-string)))))))))))

(cl-defmethod slack-file-upload-params ((this slack-message-buffer))
  (with-slots (room team) this
    (list (cons "channels"
                (mapconcat #'identity
                           (slack-file-select-sharing-channels
                            (slack-room-label room team)
                            team)
                           ",")))))

(cl-defmethod slack-buffer-next-message ((this slack-message-buffer) message)
  (with-slots (room team) this
    (let ((ts (slack-ts message)))
      (cl-loop for m in (reverse (slack-room-sorted-messages room))
               with next = nil
               do (when (slack-buffer-visible-message-p this m)
                    (if (string= (slack-ts m) ts)
                        (return next)
                      (setq next m)))))))

(cl-defmethod slack-buffer-prev-message ((this slack-message-buffer) message)
  (with-slots (room team) this
    (let ((ts (slack-ts message)))
      (cl-loop for m in (reverse (slack-room-sorted-messages room))
               with prev = nil
               do (when (slack-buffer-visible-message-p this m)
                    (if prev (return m)
                      (when (string= (slack-ts m) ts)
                        (setq prev t))))))))

(cl-defmethod slack-buffer-merge-message-p ((this slack-message-buffer) message prev)
  (with-slots (team) this
    (and prev
         (and (not (slack-message-starred-p message))
              (not (slack-message-starred-p prev))
              (not (slack-reply-broadcast-message-p message))
              (null (oref message thread))
              (null (oref prev thread))
              (let ((prev-user (slack-user-find prev team))
                    (current-user (slack-user-find message team)))
                (equal prev-user current-user))
              (let ((prev-day (time-to-day-in-year (slack-message-time-stamp
                                                    prev)))
                    (current-day (time-to-day-in-year (slack-message-time-stamp
                                                       message))))
                (eq prev-day current-day))))))

(cl-defmethod slack-buffer-message-text ((this slack-message-buffer)
                                         message
                                         merge-message-p)
  (with-slots (team) this
    (let ((text (slack-message-to-string message team)))
      (or (and merge-message-p
               (let ((end (next-single-property-change
                           0 'slack-message-header text)))
                 (substring text (1+ end))))
          text))))

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
  (with-slots (team) this
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
                      (slack-buffer-replace this next))))))))))

  (slack-buffer-update-marker-overlay this))

(defun slack-message-buffer-detect-ts-changed ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (message-buffer-p (eq 'slack-message-buffer
                                        (eieio-object-class buffer)))
                  (prev-ts (oref buffer cursor-event-prev-ts))
                  (current-ts (slack-get-ts)))
      (when (not (string= prev-ts current-ts))
        (oset buffer cursor-event-prev-ts current-ts)
        (slack-buffer-update-mark buffer))))

(cl-defmethod slack-buffer--subscribe-cursor-event ((this slack-message-buffer)
                                                    _window
                                                    _prev-point
                                                    type)
  (cond
   ((eq type'entered)
    (with-slots (team) this
      (unless (slack-team-mark-as-read-immediatelyp team)
        (oset this cursor-event-prev-ts (slack-get-ts))
        (add-hook 'post-command-hook
                  #'slack-message-buffer-detect-ts-changed
                  t t)
        (slack-buffer-update-mark this))))
   ((eq type 'left)
    (remove-hook 'post-command-hook
                 #'slack-message-buffer-detect-ts-changed
                 t))))

(defun slack-thread-start ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-start-thread buf (slack-get-ts))))

(defun slack-room-unread-threads ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-unread-threads buf)))

(cl-defmethod slack-thread-show-messages ((thread slack-thread) room team)
  (cl-labels
      ((after-success (_next-cursor has-more)
                      (let ((buf (slack-create-thread-message-buffer
                                  room team (oref thread thread-ts) has-more)))
                        (slack-buffer-display buf))))
    (slack-thread-replies thread room team
                          :after-success #'after-success)))

(defun slack-thread-show-or-create ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (if (slack-thread-message-buffer-p buf)
          (error "Already in thread")
        (slack-buffer-display-thread buf (slack-get-ts)))))

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
    (let ((buf (slack-buffer-find 'slack-message-buffer
                                  room
                                  team)))
      (if buf (open buf)
        (message "No Message in %s, fetching from server..." (slack-room-name room team))
        (slack-conversations-history
         room team
         :after-success #'(lambda (messages cursor)
                            (slack-room-set-messages room messages)
                            (open (slack-create-message-buffer room cursor team))))))))

(cl-defmethod slack-room-update-buffer ((this slack-room) team message replace)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer this team)))
      (slack-buffer-update buffer message :replace replace)
    (and slack-buffer-create-on-notify
         (slack-conversations-history
          this team
          :after-success #'(lambda (messages cursor)
                             (slack-room-set-messages this messages)
                             (tracking-add-buffer
                              (slack-buffer-buffer
                               (slack-create-message-buffer this cursor team))))))))

(defun slack-select-unread-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (cl-remove-if
                                   #'(lambda (room)
                                       (not (slack-room-has-unread-p room)))
                                   (append ims groups channels))))
                team)))
    (slack-room-display room team)))

(defun slack-select-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels)))
                team)))
    (slack-room-display room team)))

(defun slack-message-inspect ()
  (interactive)
  (slack-if-let* ((ts (slack-get-ts))
                  (buffer slack-current-buffer))
      (with-slots (room team) buffer
        (slack-if-let* ((message (slack-room-find-message room ts))
                        (text (slack-message--inspect message room team)))
            (message "%s" text)))))

(defun slack-message-update-mark ()
  "Update Channel's last-read marker to this message."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-update-mark buffer :force t)))

(cl-defmethod slack-thread-message-update-buffer ((message slack-message)
                                                  room team replace old-message)
  (slack-if-let* ((parent (slack-room-find-thread-parent room message)))
      (progn
        (slack-room-update-buffer room team parent t)
        (when (slack-reply-broadcast-message-p message)
          (let* ((replace (if old-message
                              (slack-reply-broadcast-message-p old-message)
                            replace)))
            (slack-room-update-buffer room team message replace)))
        (slack-if-let* ((thread (slack-message-get-thread parent))
                        (buf (slack-buffer-find 'slack-thread-message-buffer
                                                room
                                                (oref thread thread-ts)
                                                team)))
            (slack-buffer-update buf message :replace replace)))))

(cl-defmethod slack-message-update ((message slack-message) team &optional replace no-notify old-message)
  (slack-if-let*
      ((room (slack-room-find (oref message channel) team))
       (ts (slack-ts message))
       (no-same-message (if replace t
                          (not (slack-room-find-message room ts)))))

      (progn
        (when (and (not replace)
                   (slack-message-mentioned-p message team))
          (cl-incf (oref room mention-count-display))
          (cl-incf (oref room mention-count)))

        (slack-room-push-message room message)
        (slack-room-update-latest room message)

        (if (or (slack-thread-message-p message)
                (slack-reply-broadcast-message-p message))
            (slack-thread-message-update-buffer message
                                                room
                                                team
                                                replace
                                                old-message)
          (slack-room-update-buffer room team message replace)
          (slack-room-inc-unread-count room))

        (unless no-notify
          (slack-message-notify message room team))
        (slack-update-modeline))))

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
                  (reaction (slack-message-reaction-input)))
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
       (team (oref buffer team))
       (room-id (get-text-property (point) 'room-id))
       (room (slack-room-find room-id team)))
      (slack-room-display room team)))

(defun slack-im-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (candidates (cl-loop for team in (list team)
                              for ims = (cl-remove-if #'(lambda (im)
                                                          (not (oref im is-open)))
                                                      (oref team ims))
                              nconc ims))
         (room (slack-room-select candidates team)))
    (slack-room-display room team)))

(defun slack-group-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for groups = (oref team groups)
                         nconc groups)
                team)))
    (slack-room-display room team)))

(defun slack-channel-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for channels = (oref team channels)
                         nconc channels)
                team)))
    (slack-room-display room team)))

(cl-defmethod slack-buffer-display-im ((this slack-user-profile-buffer))
  (with-slots (user-id team) this
    (let ((im (slack-im-find-by-user-id user-id team)))
      (slack-room-display im team))))

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

(provide 'slack-message-buffer)
;;; slack-message-buffer.el ends here
