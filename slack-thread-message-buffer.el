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
(require 'slack-room-buffer)

(define-derived-mode slack-thread-message-buffer-mode
  slack-buffer-mode
  "Slack Thread Message"
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-thread-message--send))

(defclass slack-thread-message-buffer (slack-room-buffer)
  ((thread-ts :initarg :thread-ts :type string)))

(defmethod slack-buffer-find :static ((class slack-thread-message-buffer) room ts team)
  (slack-buffer-find-4 class room ts team))

(defun slack-create-thread-message-buffer (room team thread-ts)
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer
                                          room thread-ts team)))
      buf
    (slack-thread-message-buffer :room room
                                 :team team
                                 :thread-ts thread-ts)))

(defmethod slack-buffer-name :static ((class slack-thread-message-buffer) room ts team)
  (format "*Slack - %s : %s Thread - %s"
          (oref team name)
          (slack-room-name room team)
          ts))

(defmethod slack-buffer-name ((this slack-thread-message-buffer))
  (with-slots (room thread-ts team) this
    (slack-buffer-name 'slack-thread-message-buffer
                       room thread-ts team)))

(defmethod slack-buffer-init-buffer ((this slack-thread-message-buffer))
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
              (let ((thread (slack-message-thread message room)))
                (and thread
                     (cl-loop for m in (oref thread messages)
                              do (slack-buffer-insert this m))))))))

    (with-slots (room thread-ts team) this
      (slack-buffer-push-new-4 (eieio-object-class-name this)
                               room
                               thread-ts
                               team))

    buf))

(defmethod slack-buffer-display-message-compose-buffer
  ((this slack-thread-message-buffer))
  (with-slots (room team thread-ts) this
    (let ((buf (slack-create-thread-message-compose-buffer
                room thread-ts team)))
      (slack-buffer-display buf))))


(defmethod slack-buffer-send-message ((this slack-thread-message-buffer) message)
  (with-slots (room team thread-ts) this
    (slack-thread-send-message room team message thread-ts)))

(defmethod slack-buffer-add-reaction-to-message
  ((this slack-thread-message-buffer) reaction ts)
  (with-slots (room team) this
    (slack-message-reaction-add reaction ts room team)))

(defmethod slack-buffer-remove-reaction-from-message
  ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (reaction (slack-message-reaction-select
                      (slack-message-reactions message))))
      (slack-message-reaction-remove reaction ts room team))))

(defmethod slack-buffer-add-star ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-add-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(defmethod slack-buffer-remove-star ((this slack-thread-message-buffer) ts)
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

(defmethod slack-buffer-display-edit-message-buffer ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-edit-message-buffer room team ts)))
      (slack-buffer-display buf))))

(defmethod slack-buffer-share-message ((this slack-thread-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-message-share-buffer room team ts)))
      (slack-buffer-display buf))))

(defmethod slack-file-upload-params ((this slack-thread-message-buffer))
  (list (cons "thread_ts" (oref this thread-ts))
        (cons "channels" (oref (oref this room) id))))

(provide 'slack-thread-message-buffer)
;;; slack-thread-message-buffer.el ends here
