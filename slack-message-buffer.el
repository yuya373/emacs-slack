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

(require 'eieio)
(require 'slack-room)
(require 'slack-util)
(require 'slack-room-buffer)
(require 'slack-buffer)
(require 'slack-request)
(require 'slack-action)

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
   ))

(defmethod slack-buffer-last-read ((this slack-message-buffer))
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

(defmethod slack-buffer-update-mark-request ((this slack-message-buffer) ts &optional after-success)
  (with-slots (room team) this
    (when (slack-room-member-p room)
      (cl-labels ((on-update-mark (&key data &allow-other-keys)
                                  (slack-request-handle-error
                                   (data "slack-buffer-update-mark-request")
                                   (oset room last-read ts)
                                   (slack-buffer-update-marker-overlay this)
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

(defmethod slack-buffer-send-message ((this slack-message-buffer) message)
  (with-slots (room team) this
    (slack-message-send-internal message (oref room id) team)))

(defmethod slack-buffer-latest-ts ((this slack-message-buffer))
  (with-slots (room) this
    (slack-if-let* ((latest (oref room latest)))
        (slack-ts latest))))

(defmethod slack-buffer-buffer ((this slack-message-buffer))
  (let ((buffer-already-exists-p (get-buffer (slack-buffer-name this)))
        (buffer (call-next-method))
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

(defmethod slack-buffer-display-unread-threads ((this slack-message-buffer))
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

(defmethod slack-buffer-start-thread ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (buf (slack-create-thread-message-buffer room team ts)))
      (when (slack-reply-broadcast-message-p message)
        (error "Can't start thread from broadcasted message"))
      (slack-buffer-display buf))))

(defmethod slack-buffer-major-mode ((this slack-message-buffer))
  'slack-message-buffer-mode)

(defmethod slack-buffer-init-buffer ((this slack-message-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (funcall (slack-buffer-major-mode this))
      (slack-buffer-set-current-buffer this)
      (goto-char (point-min))

      (slack-buffer-insert-load-more this)

      (with-slots (room team latest) this
        (let* ((messages (slack-room-sorted-messages room))
               (latest-message (car (last messages)))
               (oldest-message (car messages)))
          (cl-loop for m in messages
                   do (if (and (or (null latest)
                                   (string< latest (slack-ts m)))
                               (or (not (slack-thread-message-p m))
                                   (slack-reply-broadcast-message-p m)))
                          (slack-buffer-insert this m t)))
          (when latest-message
            (slack-buffer-update-lastest this (slack-ts latest-message)))
          (when oldest-message
            (slack-buffer-update-oldest this oldest-message))))
      )
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
        (slack-buffer-update-lastest this (slack-ts message))
        (with-current-buffer buffer
          (slack-buffer-insert this message))))))

(defmethod slack-buffer-display-message-compose-buffer ((this slack-message-buffer))
  (with-slots (room team) this
    (let ((buf (slack-create-room-message-compose-buffer room team)))
      (slack-buffer-display buf))))

(defmethod slack-buffer-update-lastest ((this slack-message-buffer) latest)
  (with-slots ((prev-latest latest)) this
    (if (or (null prev-latest)
            (string< prev-latest latest))
        (setq prev-latest latest))))

(defmethod slack-buffer-display-thread ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((thread (slack-room-find-thread room ts)))
      (if thread (slack-thread-show-messages thread room team)
        (slack-thread-start)))))

(defmethod slack-buffer-display-edit-message-buffer ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-edit-message-buffer room team ts)))
      (slack-buffer-display buf))))

(defmethod slack-create-message-buffer ((room slack-room) team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer
                                             room
                                             team)))
      buffer
    (slack-message-buffer :room room :team team)))


(defmethod slack-buffer-share-message ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-message-share-buffer room team ts)))
      (slack-buffer-display buf))))

(defmethod slack-buffer-add-reaction-to-message
  ((this slack-message-buffer) reaction ts)
  (with-slots (room team) this
    (slack-message-reaction-add reaction ts room team)))

(defmethod slack-buffer-remove-reaction-from-message
  ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (reactions (slack-message-reactions message))
           (reaction (slack-message-reaction-select reactions)))
      (slack-message-reaction-remove reaction ts room team))))

(defmethod slack-buffer-pins-remove ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (slack-message-pins-request slack-message-pins-remove-url
                                room team ts)))

(defmethod slack-buffer-pins-add ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (slack-message-pins-request slack-message-pins-add-url
                                room team ts)))
(defmethod slack-buffer-remove-star ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-remove-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(defmethod slack-buffer-add-star ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-add-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(defmethod slack-buffer-update-oldest ((this slack-message-buffer) message)
  (when (and message (or (null (oref this oldest))
                         (string< (slack-ts message) (oref this oldest))))
    (oset this oldest (slack-ts message))))

(defmethod slack-buffer-load-missing-messages ((this slack-message-buffer))
  (with-slots (room team) this
    (cl-labels
        ((request-messages (latest)
                           (slack-room-history-request room team
                                                       :latest latest
                                                       :count 100
                                                       :after-success #'after-success))
         (after-success (has-more)
                        (let* ((messages (slack-room-sorted-messages room))
                               (oldest-message (car messages))
                               (latest-message (car (last messages))))
                          (if has-more
                              (request-messages (slack-ts latest-message))
                            (progn
                              (with-current-buffer (slack-buffer-buffer this)
                                (let ((inhibit-read-only t))
                                  (slack-buffer-delete-overlay this)
                                  (delete-region (point-min)
                                                 (marker-position lui-output-marker)))
                                (slack-buffer-prepare-marker-for-history this)
                                (slack-buffer-insert-load-more this)
                                (cl-loop for m in messages
                                         do (slack-buffer-insert this m t))
                                (slack-buffer-goto (slack-buffer-last-read this))
                                (slack-buffer-update-marker-overlay this))
                              (when oldest-message
                                (slack-buffer-update-oldest this
                                                            oldest-message)))))))
      (oset room messages nil)
      (request-messages nil))))

(defmethod slack-buffer-load-more ((this slack-message-buffer))
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
                 (if (and messages (< 0 (length messages)))
                     (slack-buffer-insert-load-more this)
                   (let ((lui-time-stamp-position nil))
                     (lui-insert "(no more messages)\n")))

                 (cl-loop for m in messages
                          do (slack-buffer-insert this m t))
                 (lui-recover-output-marker)
                 (slack-buffer-update-marker-overlay this)
                 ))
              (if current-ts
                  (slack-buffer-goto current-ts)
                (goto-char cur-point))))
           (after-success (&rest _ignore)
                          (let ((messages (cl-remove-if #'(lambda (e)
                                                            (or (string< oldest e)
                                                                (string= oldest e)))
                                                        (slack-room-sorted-messages room)
                                                        :key #'slack-ts)))
                            (update-buffer messages)
                            (slack-buffer-update-oldest this (car messages)))))
        (slack-room-history-request room team
                                    :oldest oldest
                                    :after-success #'after-success)))))

(defmethod slack-buffer-display-pins-list ((this slack-message-buffer))
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

(defmethod slack-buffer-display-user-profile ((this slack-message-buffer))
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

(defmethod slack-buffer-delete-overlay ((this slack-message-buffer))
  (when (oref this marker-overlay)
    (delete-overlay (oref this marker-overlay))))

(defmethod slack-buffer-update-marker-overlay ((this slack-message-buffer))
  (let ((buf (get-buffer (slack-buffer-name this))))
    (and buf (with-current-buffer buf
               (let* ((last-read (slack-buffer-last-read this))
                      (beg (slack-buffer-ts-eq (point-min) (point-max) last-read))
                      (end (and beg (next-single-property-change beg 'ts))))
                 (when (and beg end)
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
                                      (format "\n%s" after-string)))))))))))

(defmethod slack-buffer-replace ((this slack-message-buffer) message)
  (call-next-method)
  (slack-buffer-update-marker-overlay this))

(defmethod slack-file-upload-params ((this slack-message-buffer))
  (with-slots (room team) this
    (list (cons "channels"
                (mapconcat #'identity
                           (slack-file-select-sharing-channels
                            (slack-room-label room team)
                            team)
                           ",")))))

(provide 'slack-message-buffer)
;;; slack-message-buffer.el ends here
