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

(defvar alert-default-style)

(defcustom slack-message-custom-notifier nil
  "Custom notification function.\ntake 3 Arguments.\n(lambda (MESSAGE ROOM TEAM) ...)."
  :group 'slack)

(defun slack-message-notify (message room team)
  (if slack-message-custom-notifier
      (funcall slack-message-custom-notifier message room team)
    (slack-message-notify-alert message room team)))

(defun slack-message-notify-alert (message room team)
  (if (and (not (slack-message-minep message team))
           (or (slack-im-p room)
               (and (slack-group-p room) (slack-mpim-p room))
               (slack-room-subscribedp room team)
               (string-match (format "@%s" (plist-get (oref team self) :name))
                             (or (slack-message-body message team) ""))))
      (let ((team-name (oref team name))
            (room-name (slack-room-name room))
            (text (slack-message-to-alert message team))
            (user-name (slack-message-sender-name message team)))
        (if (and (eq alert-default-style 'notifier)
                 (slack-im-p room)
                 (or (eq (aref text 0) ?\[)
                     (eq (aref text 0) ?\{)
                     (eq (aref text 0) ?\<)
                     (eq (aref text 0) ?\()))
            (setq text (concat "\\" text)))
        (alert (if (slack-im-p room) text (format "%s: %s" user-name text))
               :title (if (slack-im-p room)
                          (format "%s - %s" team-name room-name)
                        (format "%s - #%s" team-name room-name))
               :category 'slack))))

(defmethod slack-message-sender-equalp ((_m slack-message) _sender-id)
  nil)

(defmethod slack-message-minep ((m slack-message) team)
  (if team
      (with-slots (self-id) team
        (slack-message-sender-equalp m self-id))
    (slack-message-sender-equalp m (oref team self-id))))

(provide 'slack-message-notification)
;;; slack-message-notification.el ends here
