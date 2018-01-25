;;; slack-room-message-compose-buffer.el ---              -*- lexical-binding: t; -*-

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
(require 'slack-message-compose-buffer)

(defclass slack-room-message-compose-buffer (slack-message-compose-buffer)
  ((room :initarg :room type slack-room)))

(defun slack-create-room-message-compose-buffer (room team)
  (slack-if-let* ((buf (slack-buffer-find 'slack-room-message-compose-buffer
                                    room team)))
      buf
    (slack-room-message-compose-buffer :room room :team team)))

(defmethod slack-buffer-name :static ((class slack-room-message-compose-buffer) room team)
  (format "*Slack - %s : %s Compose Message"
          (oref team name)
          (slack-room-name room)))

(defmethod slack-buffer-name ((this slack-room-message-compose-buffer))
  (with-slots (room team) this
    (slack-buffer-name (eieio-object-class-name this)
                       room team)))

(defmethod slack-buffer-init-buffer ((this slack-room-message-compose-buffer))
  (let* ((buf (call-next-method)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "C-c C-c to send edited msg"))
    (with-slots (room team) this
      (slack-buffer-push-new-3 'slack-room-message-compose-buffer
                               room team))
    buf))

(defmethod slack-buffer-send-message ((this slack-room-message-compose-buffer) message)
  (let ((buffer (slack-buffer-buffer this)))
    (with-slots (room team) this
      (slack-message-send-internal message (oref room id) team)
      (call-next-method))))


(provide 'slack-room-message-compose-buffer)
;;; slack-room-message-compose-buffer.el ends here
