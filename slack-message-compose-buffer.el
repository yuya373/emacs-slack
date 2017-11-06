;;; slack-message-compose-buffer.el ---              -*- lexical-binding: t; -*-

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
(require 'slack-buffer)

(define-derived-mode slack-message-compose-buffer-mode
  slack-edit-message-mode
  "Slack Compose Message")

(defclass slack-message-compose-buffer (slack-buffer) ())

(defmethod slack-buffer-send-message ((this slack-message-compose-buffer) _message)
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (kill-buffer)
      (if (> (count-windows) 1) (delete-window)))))

(defmethod slack-buffer-init-buffer ((this slack-message-compose-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-message-compose-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))


(provide 'slack-message-compose-buffer)
;;; slack-message-compose-buffer.el ends here
