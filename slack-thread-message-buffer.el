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
(require 'slack-room-buffer)

(defclass slack-thread-message-buffer (slack-room-buffer)
  ((thread-ts :initarg :thread-ts :type string)))

(defmethod slack-buffer-find :static ((class slack-thread-message-buffer) room ts team)
  (slack-buffer-find-4 class room ts team))

(defun slack-create-thread-message-buffer (room team thread-ts)
  (if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer
                                      room thread-ts team)))
      buf
    (slack-thread-message-buffer :room room
                                 :team team
                                 :thread-ts thread-ts)))

(defmethod slack-buffer-name :static ((class slack-thread-message-buffer) room ts team)
  (format "*Slack - %s : %s Thread - %s"
          (oref team name)
          (slack-room-name room)
          ts))

(defmethod slack-buffer-name ((this slack-thread-message-buffer))
  (with-slots (room thread-ts team) this
    (slack-buffer-name 'slack-thread-message-buffer
                       room thread-ts team)))

(defmethod slack-buffer-init-buffer ((this slack-thread-message-buffer))
  (let* ((buf (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buf
      (slack-thread-mode)
      (goto-char lui-input-marker)
      (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
      (with-slots (room thread-ts team) this
        (if-let* ((message (slack-room-find-message room thread-ts))
                  (thread (slack-message-thread message room)))
            (progn
              (slack-buffer-insert message team t)
              (let ((lui-time-stamp-position nil))
                (lui-insert (format "%s\n" (make-string lui-fill-column ?=)) t))
              (cl-loop for m in (oref thread messages)
                       do (slack-buffer-insert m team))
              ))))

    (with-slots (room thread-ts team) this
      (slack-buffer-push-new-4 (eieio-object-class-name this)
                               room
                               thread-ts
                               team))

    buf))

(defmethod slack-buffer-display-message-compose-buffer
  ((this slack-thread-message-buffer))
  (with-slots (room team thread-ts) this
    (let ((buf (slack-thread-message-compose-buffer :room room
                                                    :team team
                                                    :ts thread-ts)))
      (slack-buffer-display buf))))


(defmethod slack-buffer-send-message ((this slack-thread-message-buffer) message)
  (with-slots (room team thread-ts) this
    (slack-thread-send-message room team message thread-ts)))

(defmethod slack-buffer-add-reaction-to-message
  ((this slack-thread-message-buffer) reaction ts)
  (with-slots (room team) this
    (slack-message-reaction-add reaction ts room team)))

(defmethod slack-buffer-remove-reaction-from-message
  ((this slack-thread-message-buffer) ts &optional _file-comment-id)
  (with-slots (room team) this
    (let* ((message (or (slack-room-find-message room ts)
                        (slack-room-find-thread-message room ts)))
           (reaction (slack-message-reaction-select
                      (slack-message-reactions message))))
      (slack-message-reaction-remove reaction ts room team))))


(provide 'slack-thread-message-buffer)
;;; slack-thread-message-buffer.el ends here
