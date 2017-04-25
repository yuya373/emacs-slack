;;; slack-message-update.el --- impl for message update  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-message)
(require 'slack-buffer)
(require 'slack-thread)
(require 'slack-message-notification)

(defclass _slack-message-update ()
  ((message :initarg :message)
   (room :initarg :room)
   (team :initarg :team)
   (replace :initarg :replace :initform nil)
   (notify :initarg :notify :initform nil)))

(defclass _slack-thread-message-update (_slack-message-update) ())

(defmethod slack-message-update--buffer ((this _slack-message-update))
  (with-slots (room message team replace) this
    (slack-buffer-update room message team :replace replace)))

(defmethod slack-message-update--buffer ((this _slack-thread-message-update))
  (with-slots (room team replace message) this
    (let* ((parent (slack-room-find-thread-parent room message))
           (thread (and parent (slack-message-get-thread parent team))))
      (when parent
        (slack-buffer-update room parent team :replace t)
        (when thread
          (slack-thread-add-message thread message)
          (slack-thread-update-buffer thread message room team :replace replace))))))

(defmethod slack-message-update--notify ((this _slack-message-update))
  (with-slots (notify message room team) this
    (and notify (slack-message-notify message room team))))

(defmethod slack-message-update--notify ((this _slack-thread-message-update))
  (with-slots (message room team) this
    (slack-message-notify message room team)))

(defmethod slack-message-update--room ((this _slack-message-update))
  (with-slots (message room) this
    (when room
      (slack-room-push-message room message)
      (slack-room-update-latest room message))))

(defmethod slack-message-update ((m slack-message) team &optional replace no-notify)
  (let ((room (slack-room-find (oref m channel) team))
        (class (or (and (slack-message-thread-messagep m) '_slack-thread-message-update)
                   '_slack-message-update)))
    (when room
      (let ((update (make-instance class
                                   :message m
                                   :room room
                                   :team team
                                   :replace replace
                                   :notify (not no-notify))))
        (slack-message-update--room update)
        (slack-message-update--buffer update)
        (slack-message-update--notify update)
        (slack-update-modeline)))))

(provide 'slack-message-update)
;;; slack-message-update.el ends here
