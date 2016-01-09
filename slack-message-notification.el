;;; slack-message-notification.el --- message notification  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
(require 'slack-room)
(require 'slack-message)
(require 'slack-message-formatter)
(require 'slack-buffer)
(require 'slack-im)
(require 'alert)

(defconst slack-message-notification-buffer-name "*Slack - notification*")
(defvar slack-message-notification-subscription '())
(defvar slack-message-tips '())
(defvar alert-default-style)
(defvar slack-my-user-id)

(defmethod slack-message-notify-buffer ((m slack-message) room)
  (if (not (slack-message-minep m))
      (let ((room-name (concat (slack-room-name room) ": "))
            (message (slack-message-to-string m))
            (buf-name slack-message-notification-buffer-name))
        (slack-message-put-header-property room-name)
        (slack-buffer-update-notification buf-name
                                          (concat room-name message "\n")))))

(defun slack-message-notify-alert (message room)
  (if (or (and (slack-im-p room) (not (slack-message-minep message)))
          (and (slack-group-p room) (slack-mpim-p room)
               (not (slack-message-minep message)))
          (and (slack-room-subscribedp room) (not (slack-message-minep message))))
      (let ((room-name (slack-room-name room))
            (text (slack-message-to-alert message))
            (user-name (slack-message-sender-name message)))
        (if (and (eq alert-default-style 'notifier)
                 (or (eq (aref text 0) ?\[)
                     (eq (aref text 0) ?\{)
                     (eq (aref text 0) ?\<)
                     (eq (aref text 0) ?\()))
            (setq text (concat "\\" text)))
        (alert text
               :title (concat room-name ": @" user-name)
               :category 'slack))))

(defmethod slack-message-sender-equalp ((_m slack-message) _sender-id)
  nil)

(defmethod slack-message-minep ((m slack-message))
  (slack-message-sender-equalp m slack-my-user-id))

(provide 'slack-message-notification)
;;; slack-message-notification.el ends here
