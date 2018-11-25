;;; slack-thread-message-buffer.el ---               -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-message-sender)
(require 'slack-message-reaction)
(require 'slack-message-edit-buffer)
(require 'slack-message-share-buffer)

(define-derived-mode slack-thread-message-buffer-mode
  slack-buffer-mode
  "Slack Thread Message"
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-thread-message--send))

(defclass slack-thread-message-buffer (slack-room-buffer)
  ((thread-ts :initarg :thread-ts :type string)
   (has-more :initarg :has-more :type boolean)
   (last-read :initform nil :type (or null string))))

(cl-defmethod slack-buffer-find ((class (subclass slack-thread-message-buffer)) room ts team)
  (slack-buffer-find-4 class room ts team))

(defun slack-create-thread-message-buffer (room team thread-ts &optional has-more)
  "Create thread message buffer according to ROOM, TEAM, THREAD-TS."
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer
                                          room thread-ts team)))
      buf
    (slack-thread-message-buffer :room room
                                 :team team
                                 :has-more has-more
                                 :thread-ts thread-ts)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-thread-message-buffer)) room ts team)
  (format "*Slack - %s : %s Thread - %s"
          (oref team name)
          (slack-room-name room team)
          ts))

(cl-defmethod slack-buffer-name ((this slack-thread-message-buffer))
  (with-slots (room thread-ts team) this
    (slack-buffer-name 'slack-thread-message-buffer
                       room thread-ts team)))

(cl-defmethod slack-buffer-update-last-read ((this slack-thread-message-buffer) message)
  (when message
    (oset this last-read (slack-ts message))))

(cl-defmethod slack-buffer-init-buffer ((this slack-thread-message-buffer))
  (let* ((buf (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buf
      (slack-thread-message-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char lui-input-marker)
      (with-slots (room thread-ts team) this
        (slack-if-let* ((message (slack-room-find-message room thread-ts)))
            (progn
              (slack-buffer-insert this message t)
              (let ((lui-time-stamp-position nil))
                (lui-insert (format "%s\n" (make-string lui-fill-column ?=)) t))
              (slack-if-let* ((thread (slack-message-thread message room))
                              (messages (oref thread messages)))
                  (progn
                    (cl-loop for m in messages
                             do (slack-buffer-insert this m))
                    (slack-buffer-update-last-read this (car (last messages)))))
              (when (slack-buffer-has-next-page-p this)
                (slack-buffer-insert-load-more this))))))

    (with-slots (room thread-ts team) this
      (slack-buffer-push-new-4 (eieio-object-class-name this)
                               room
                               thread-ts
                               team))

    buf))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-thread-message-buffer))
  (oref this has-more))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-thread-message-buffer))
  (slack-if-let*
      ((beg (next-single-property-change (point-min)
                                         'loading-message))
       (end (next-single-property-change beg
                                         'loading-message)))
      (delete-region beg end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-thread-message-buffer)))

(cl-defmethod slack-buffer-insert--history ((this slack-thread-message-buffer))
  (slack-buffer-insert-history this)
  (when (slack-buffer-has-next-page-p this)
    (slack-buffer-insert-load-more this)))

(cl-defmethod slack-buffer-request-history ((this slack-thread-message-buffer) after-success)
  (with-slots (team thread-ts room last-read) this
    (slack-if-let* ((message (slack-room-find-message room thread-ts))
                    (thread (slack-message-thread message room)))
        (cl-labels
            ((success (_next-cursor has-more)
                      (oset this has-more has-more)
                      (funcall after-success)))
          (slack-thread-replies thread room team
                                :after-success #'success
                                :oldest last-read)))))

(cl-defmethod slack-buffer-insert-history ((this slack-thread-message-buffer))
  (with-slots (team thread-ts last-read room) this
    (slack-if-let* ((message (slack-room-find-message room thread-ts))
                    (thread (slack-message-thread message room))
                    (messages (oref thread messages)))
        (progn
          (cl-loop for m in messages
                   do (when (string< last-read (slack-ts m))
                        (slack-buffer-insert this m)))
          (slack-buffer-update-last-read this (car (last messages)))))))

(cl-defmethod slack-buffer-send-message ((this slack-thread-message-buffer) message)
  (with-slots (room team thread-ts) this
    (slack-thread-send-message room team message thread-ts)))

(defun slack-thread-send-message (room team message thread-ts)
  (let ((message (slack-message-prepare-links
                  (slack-escape-message message)
                  team))
        (broadcast (if (eq slack-thread-also-send-to-room 'ask)
                       (y-or-n-p (format "Also send to %s ? "
                                         (slack-room-name room team)))
                     slack-thread-also-send-to-room)))
    (progn
      (slack-team-inc-message-id team)
      (with-slots (message-id sent-message self-id) team
        (let* ((payload (list :id message-id
                              :channel (oref room id)
                              :reply_broadcast broadcast
                              :thread_ts thread-ts
                              :type "message"
                              :user self-id
                              :text message))
               (obj (slack-message-create payload team :room room)))
          (slack-team-send-message team payload)
          (puthash message-id obj sent-message))))))

(defun slack-thread-message--send (message)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-send-message buf message)))

(cl-defmethod slack-buffer-add-reaction-to-message
  ((this slack-thread-message-buffer) reaction ts)
  (with-slots (room team) this
    (slack-message-reaction-add reaction ts room team)))

(cl-defmethod slack-buffer-remove-reaction-from-message
  ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (reaction (slack-message-reaction-select
                      (slack-message-reactions message))))
      (slack-message-reaction-remove reaction ts room team))))

(cl-defmethod slack-buffer-add-star ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-add-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(cl-defmethod slack-buffer-remove-star ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-remove-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(cl-defmethod slack-buffer-update ((this slack-thread-message-buffer) message &key replace)
  (if replace (slack-buffer-replace this message)
    (with-current-buffer (slack-buffer-buffer this)
      (slack-buffer-insert this message))))

(cl-defmethod slack-buffer-display-edit-message-buffer ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-edit-message-buffer room team ts)))
      (slack-buffer-display buf))))

(cl-defmethod slack-buffer-share-message ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-message-share-buffer room team ts)))
      (slack-buffer-display buf))))

(cl-defmethod slack-file-upload-params ((this slack-thread-message-buffer))
  (list (cons "thread_ts" (oref this thread-ts))
        (cons "channels" (oref (oref this room) id))))

(provide 'slack-thread-message-buffer)
;;; slack-thread-message-buffer.el ends here
