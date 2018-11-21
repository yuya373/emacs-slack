;;; slack-thread-message-compose-buffer.el ---       -*- lexical-binding: t; -*-

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
(require 'slack-thread-message-buffer)

(defclass slack-thread-message-compose-buffer (slack-message-compose-buffer)
  ((room :initarg :room :type slack-room)
   (thread-ts :initarg :thread-ts :type string)))

(cl-defmethod slack-buffer-find ((class (subclass slack-thread-message-compose-buffer)) room ts team)
  (slack-buffer-find-4 class room ts team))

(cl-defmethod slack-buffer-name ((_class (subclass slack-thread-message-compose-buffer)) room ts team)
  (format "*Slack - %s : %s Compose Thread Message - %s*"
          (oref team name)
          (slack-room-name room team)
          ts))

(cl-defmethod slack-buffer-name ((this slack-thread-message-compose-buffer))
  (with-slots (room thread-ts team) this
    (slack-buffer-name 'slack-thread-message-compose-buffer
                       room thread-ts team)))

(cl-defmethod slack-buffer-init-buffer ((this slack-thread-message-compose-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-compose-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-slots (room thread-ts team) this
      (slack-buffer-push-new-4 'slack-thread-message-compose-buffer
                               room thread-ts team))
    buf))

(defun slack-create-thread-message-compose-buffer (room ts team)
  "Create thread message compose buffer, according to ROOM, TS, TEAM."
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-compose-buffer
                                          room ts team)))
      buf
    (slack-thread-message-compose-buffer :room room
                                         :team team
                                         :thread-ts ts)))

(cl-defmethod slack-buffer-send-message
  ((this slack-thread-message-compose-buffer) message)
  (with-slots (room team thread-ts) this
    (slack-thread-send-message room team message thread-ts))
  (cl-call-next-method))

(cl-defmethod slack-buffer-display-message-compose-buffer
  ((this slack-thread-message-buffer))
  (with-slots (room team thread-ts) this
    (let ((buf (slack-create-thread-message-compose-buffer
                room thread-ts team)))
      (slack-buffer-display buf))))

(provide 'slack-thread-message-compose-buffer)
;;; slack-thread-message-compose-buffer.el ends here
