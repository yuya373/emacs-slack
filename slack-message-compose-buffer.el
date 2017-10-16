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
(require 'slack-room-buffer)

(defclass slack-message-compose-buffer (slack-room-buffer) ())

(defmethod slack-buffer-name ((_this slack-message-compose-buffer))
  (format "%s Compose Message" (call-next-method)))

(defmethod slack-buffer-init-buffer ((this slack-message-compose-buffer))
  (let* ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-edit-message-mode)
      (setq slack-current-buffer this)
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "C-c C-c to send edited msg"))
    buf))

(defmethod slack-buffer-send-message ((this slack-message-compose-buffer) message)
  (with-slots (room team buffer) this
    (slack-message-send-internal message (oref room id) team)
    (call-next-method)))


(provide 'slack-message-compose-buffer)
;;; slack-message-compose-buffer.el ends here
