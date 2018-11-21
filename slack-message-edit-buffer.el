;;; slack-message-edit-buffer.el ---                 -*- lexical-binding: t; -*-

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
(require 'slack-buffer)
(require 'slack-message-compose-buffer)
(require 'slack-message-editor)
;; (require 'slack-message-buffer)

(define-derived-mode slack-message-edit-buffer-mode
  slack-edit-message-mode
  "Slack Edit Message")

(defclass slack-message-edit-buffer (slack-message-compose-buffer)
  ((room :initarg :room :type slack-room)
   (ts :initarg :ts :type string)))

(defun slack-create-edit-message-buffer (room team ts)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-edit-buffer
                                             room ts team)))
      buffer
    (slack-message-edit-buffer :room room :team team :ts ts)))

(cl-defmethod slack-buffer-find ((class (subclass slack-message-edit-buffer)) room ts team)
  (slack-buffer-find-4 class room ts team))

(cl-defmethod slack-buffer-name ((_class (subclass slack-message-edit-buffer)) room  ts team)
  (format "*Slack - %s : %s Edit Message %s"
          (oref team name)
          (slack-room-name room team)
          ts))

(cl-defmethod slack-buffer-name ((this slack-message-edit-buffer))
  (with-slots (room team ts) this
    (slack-buffer-name (eieio-object-class-name this)
                       room ts team)))

(cl-defmethod slack-buffer-init-buffer ((this slack-message-edit-buffer))
  (with-slots (room team ts) this
    (let* ((buf (cl-call-next-method))
           (message (slack-room-find-message room ts)))
      (with-current-buffer buf
        (slack-message-edit-buffer-mode)
        (slack-buffer-set-current-buffer this)
        (insert (slack-message-unescape-string
                 (slack-message-get-text message)
                 team)))
      (with-slots (room ts team) this
        (slack-buffer-push-new-4 (eieio-object-class-name this)
                                 room
                                 ts
                                 team))
      buf)))

(cl-defmethod slack-buffer-send-message ((this slack-message-edit-buffer) message)
  (with-slots (room team ts) this
    (slack-if-let* ((m (slack-room-find-message room ts)))
        (progn
          (slack-message--edit (oref room id) team ts message)
          (cl-call-next-method)))))

(provide 'slack-message-edit-buffer)
;;; slack-message-edit-buffer.el ends here
