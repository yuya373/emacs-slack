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
(require 'slack-room-buffer)

(defclass slack-message-edit-buffer (slack-room-buffer)
  ((ts :initarg :ts :type string)))

(defmethod slack-buffer-name ((_this slack-message-edit-buffer))
  (format "%s%s" (call-next-method) " Edit Message"))

(defmethod slack-buffer-init-buffer ((this slack-message-edit-buffer))
  (with-slots (room team ts) this
    (let* ((buf (call-next-method))
           (message (slack-room-find-message room ts)))
      (with-current-buffer buf
        (slack-edit-message-mode)
        (insert (slack-message-get-text message)))
      buf)))

(defmethod slack-buffer-send-message ((this slack-message-edit-buffer) message)
  (with-slots (room team ts) this
    (if-let* ((m (slack-room-find-message room ts)))
        (progn
          (if (object-of-class-p m 'slack-file-comment-message)
              (slack-file-comment--edit (oref room id) (oref team id) ts message)
            (slack-message--edit (oref room id) team ts message))
          (call-next-method)))))

(defun slack-create-edit-message-buffer (room team ts)
  (slack-message-edit-buffer :room room
                             :team team
                             :ts ts))


(provide 'slack-message-edit-buffer)
;;; slack-message-edit-buffer.el ends here
