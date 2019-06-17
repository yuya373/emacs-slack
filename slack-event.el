;;; slack-event.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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
(require 'slack-room)

(defun slack-event-thread-message-p (payload)
  (let ((v (plist-get payload :thread_ts)))
    (and (not (null v))
         (stringp v)
         (not (slack-string-blankp v)))))

(defclass slack-event ()
  ((type :initarg :type :type string)
   (payload :initarg :payload)))

(cl-defmethod slack-event-update-buffer ((_this slack-event) _message _team))
(cl-defmethod slack-event-notify ((_this slack-event) _message _team))
(cl-defmethod slack-event-update-ui ((this slack-event) message team)
  (slack-event-update-buffer this message team)
  (slack-event-notify this message team))

(defclass slack-message-event-processable () () :abstract t)
(cl-defmethod slack-event-find-message ((_this slack-message-event-processable) _team))
(cl-defmethod slack-event-save-message ((_this slack-message-event-processable) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-room-push-message room message team)))

(cl-defmethod slack-event-update ((this slack-message-event-processable) team)
  (let ((message (slack-event-find-message this team)))
    (when message
      (slack-event-save-message this message team)
      (slack-event-update-ui this message team))))

(defclass slack-room-event-processable () () :abstract t)
(cl-defmethod slack-event-find-room ((_this slack-room-event-processable) _team))
(cl-defmethod slack-event-save-room ((_this slack-room-event-processable) _room _team))
(cl-defmethod slack-event-update ((this slack-room-event-processable) team)
  (let ((room (slack-event-find-room this team)))
    (when room
      (slack-event-save-room this room team)
      (slack-event-update-ui this room team))))

(provide 'slack-event)
;;; slack-event.el ends here
