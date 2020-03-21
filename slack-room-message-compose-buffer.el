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
(require 'slack-message-sender)

(defclass slack-room-message-compose-buffer (slack-message-compose-buffer) ())

(defun slack-create-room-message-compose-buffer (room team)
  (slack-if-let* ((buf (slack-buffer-find 'slack-room-message-compose-buffer team room)))
      buf
    (slack-room-message-compose-buffer :room-id (oref room id) :team-id (oref team id))))

(cl-defmethod slack-buffer-name ((this slack-room-message-compose-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (format "*Slack - %s : %s Compose Message"
            (slack-team-name team)
            (slack-room-name room team))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-room-message-compose-buffer)) room)
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-room-message-compose-buffer))
  (slack-buffer-key 'slack-room-message-compose-buffer (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-room-message-compose-buffer)))
  'slack-room-message-compose-buffer)

(cl-defmethod slack-buffer-init-buffer ((_this slack-room-message-compose-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer))
    buf))

(cl-defmethod slack-buffer-send-message ((this slack-room-message-compose-buffer) message)
  (slack-message-send-internal message
                               (slack-buffer-room this)
                               (slack-buffer-team this)
                               :files (slack-buffer-attachments this))
  (cl-call-next-method))


(provide 'slack-room-message-compose-buffer)
;;; slack-room-message-compose-buffer.el ends here
