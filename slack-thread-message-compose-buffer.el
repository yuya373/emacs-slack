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
(require 'slack-room-buffer)
(require 'slack-thread-message-buffer)

(defclass slack-thread-message-compose-buffer (slack-message-compose-buffer)
  ((thread-ts :initarg :thread-ts :type string)))

(cl-defmethod slack-buffer-name ((this slack-thread-message-compose-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this))
        (ts (oref this thread-ts)))
    (format "*Slack - %s : %s Compose Thread Message - %s*"
            (slack-team-name team)
            (slack-room-name room team)
            ts)))

(cl-defmethod slack-buffer-key ((_class (subclass slack-thread-message-compose-buffer)) _room ts)
  ts)

(cl-defmethod slack-buffer-key ((this slack-thread-message-compose-buffer))
  (slack-buffer-key 'slack-thread-message-compose-buffer
                    (slack-buffer-room this)
                    (oref this thread-ts)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-thread-message-compose-buffer)))
  'slack-thread-message-compose-buffer)

(cl-defmethod slack-buffer-init-buffer ((this slack-thread-message-compose-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-compose-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (setq buffer-read-only nil)
      (erase-buffer))
    buf))

(defun slack-create-thread-message-compose-buffer (room ts team)
  "Create thread message compose buffer, according to ROOM, TS, TEAM."
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-compose-buffer team room ts)))
      buf
    (slack-thread-message-compose-buffer :room-id (oref room id)
                                         :team-id (oref team id)
                                         :thread-ts ts)))

(cl-defmethod slack-buffer-send-message ((this slack-thread-message-compose-buffer) message)
  (with-slots (thread-ts) this
    (slack-thread-send-message (slack-buffer-room this)
                               (slack-buffer-team this)
                               message
                               thread-ts
                               (slack-buffer-attachments this)))
  (cl-call-next-method))

(cl-defmethod slack-buffer-display-message-compose-buffer ((this slack-thread-message-buffer))
  (with-slots (thread-ts) this
    (let ((buf (slack-create-thread-message-compose-buffer
                (slack-buffer-room this)
                thread-ts
                (slack-buffer-team this))))
      (slack-buffer-display buf))))

(provide 'slack-thread-message-compose-buffer)
;;; slack-thread-message-compose-buffer.el ends here
