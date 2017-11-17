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
(require 'slack-room-buffer)

(define-derived-mode slack-message-buffer-mode slack-mode "Slack Message Buffer"
  (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
  (add-hook 'lui-pre-output-hook 'slack-add-face-lazy nil t)
  (add-hook 'lui-pre-output-hook 'slack-search-code-block nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (setq-local lui-max-buffer-size nil)
  )

(defclass slack-message-buffer (slack-room-buffer)
  ((oldest :initform nil :type (or null string))
   (last-read :initform "0" :type string)))


(defmethod slack-buffer-update-mark ((this slack-message-buffer) ts)
  (with-slots (room team last-read) this
    (if (or (string< last-read ts)
            (string= last-read ts))
        (cl-labels ((on-update-mark (&key data &allow-other-keys)
                                    (slack-request-handle-error
                                     (data "slack-buffer-update-mark"))))
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


(defmethod slack-buffer-buffer ((this slack-message-buffer))
  (let ((has-buffer (get-buffer (slack-buffer-name this)))
        (buffer (call-next-method)))
    (with-current-buffer buffer
      (if has-buffer
          (slack-buffer-update-mark this (oref this last-read))
        (goto-char (marker-position lui-input-marker))))
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
      (when (slack-file-comment-message-p message)
        (error "Can't start thread from file comment message"))
      (when (slack-file-share-message-p message)
        (error "Can't start thread from file share message"))
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

      (with-slots (room team last-read) this
        (let* ((messages (slack-room-sorted-messages room))
               (latest-message (car (last messages)))
               (oldest-message (car messages)))
          (cl-loop for m in messages
                   do (if (and (or (null last-read)
                                   (string< last-read (oref m ts)))
                               (not (slack-thread-message-p m)))
                          (slack-buffer-insert this m t)))
          (when latest-message
            (slack-buffer-update-mark this (oref latest-message ts))
            (slack-buffer-update-last-read this latest-message))
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
      (slack-buffer-update-last-read this message)
      (if (slack-buffer-in-current-frame buffer)
          (slack-buffer-update-mark this (oref message ts))
        (slack-room-inc-unread-count room))

      (if replace (slack-buffer-replace this message)
        (with-current-buffer buffer
          (slack-buffer-insert this message))))))

(defmethod slack-buffer-display-message-compose-buffer ((this slack-message-buffer))
  (with-slots (room team) this
    (let ((buf (slack-create-room-message-compose-buffer room team)))
      (slack-buffer-display buf))))

(defmethod slack-buffer-update-last-read ((this slack-message-buffer) message)
  (with-slots (last-read) this
    (if (or (null last-read)
            (string< last-read (oref message ts)))
        (setq last-read (oref message ts)))))

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

(defmethod slack-buffer-add-reaction-to-file-comment
  ((this slack-message-buffer) reaction id)
  (with-slots (team) this
    (slack-file-comment-add-reaction id reaction team)))

(defmethod slack-buffer-remove-reaction-from-message
  ((this slack-message-buffer) ts &optional file-comment-id)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           ;; TODO
           (reactions (if file-comment-id
                          (slack-message-reactions
                           (oref (oref message file) initial-comment))
                        (slack-message-reactions message)))
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
                         (string< (oref message ts) (oref this oldest))))
    (oset this oldest (oref message ts))))

(defmethod slack-buffer-load-missing-messages ((this slack-message-buffer))
  (with-slots (room team last-read) this
    (cl-labels
        ((request-messages (latest)
                           (slack-room-history-request room team
                                                       :latest latest
                                                       :count 1
                                                       :after-success #'after-success))
         (after-success (has-more)
                        (if has-more
                            (let* ((messages (slack-room-sorted-messages room))
                                   (latest-message (car (last messages))))
                              (request-messages (oref latest-message ts)))
                          (let* ((messages (slack-room-sorted-messages room))
                                 (latest-message (car (last messages)))
                                 (buffer (slack-buffer-buffer this)))
                            (with-current-buffer buffer
                              (cl-loop for m in messages
                                       do (if (string< last-read (oref m ts))
                                              (slack-buffer-insert this m t))))
                            (when latest-message
                              (slack-buffer-update-mark this (oref latest-message ts))
                              (slack-buffer-update-last-read this latest-message))))))
      (let ((latest (oref (oref room latest) ts)))
        (request-messages latest)))))

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
                     (delete-region (point-min) loading-message-end)
                   (message "loading-message-end not found, oldest: %s" oldest))

                 (set-marker lui-output-marker (point-min))
                 (if (and messages (< 0 (length messages)))
                     (slack-buffer-insert-load-more this)
                   (let ((lui-time-stamp-position nil))
                     (lui-insert "(no more messages)\n")))

                 (cl-loop for m in messages
                          do (slack-buffer-insert this m t))
                 (lui-recover-output-marker)))
              (if current-ts
                  (slack-buffer-goto current-ts)
                (goto-char cur-point))))
           (after-success (&rest _ignore)
                          (let ((messages (cl-remove-if #'(lambda (e)
                                                            (or (string< oldest e)
                                                                (string= oldest e)))
                                                        (slack-room-sorted-messages room)
                                                        :key #'(lambda (e) (oref e ts)))))
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

(defmethod slack-buffer-execute-slash-command ((this slack-message-buffer) command)
  (with-slots (team) this
    (slack-slash-commands-execute command team)))

(provide 'slack-message-buffer)
;;; slack-message-buffer.el ends here
