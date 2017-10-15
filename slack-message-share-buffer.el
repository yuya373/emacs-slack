;;; slack-message-share-buffer.el ---                -*- lexical-binding: t; -*-

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
(require 'slack-message-edit-buffer)

(defclass slack-message-share-buffer (slack-buffer)
  ((ts :initarg :ts :type string)))

(defmethod slack-buffer-name ((_this slack-message-share-buffer))
  (format "%s%s" (call-next-method) " Share Message"))

(defmethod slack-buffer-init-buffer ((this slack-message-share-buffer))
  (let* ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-edit-message-mode)
      (setq slack-current-buffer this))
    buf))

(defmethod slack-buffer-send-message ((this slack-message-share-buffer) message)
  (with-slots (room team ts) this
    (slack-message-share--send team room ts message)
    (call-next-method)))

(defun slack-create-message-share-buffer (room team ts)
  (slack-message-share-buffer :room room :team team :ts ts))

(provide 'slack-message-share-buffer)
;;; slack-message-share-buffer.el ends here
