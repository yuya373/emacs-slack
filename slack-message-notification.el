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

(defun slack-message-notify-alert (message room team)
  (if (or (and (slack-im-p room)
               (not (slack-message-minep message team)))
          (and (slack-group-p room)
               (slack-mpim-p room)
               (not (slack-message-minep message team)))
          (and (slack-room-subscribedp room team)
               (not (slack-message-minep message team)))
          (and (not (slack-message-minep message team))
               (string-match (format "@%s" (plist-get (oref team self) :name))
                             (slack-message-body message team))))
      (let ((room-name (slack-room-name-with-team-name room))
            (text (slack-message-to-alert message team))
            (user-name (slack-message-sender-name message team)))
        (if (and (eq alert-default-style 'notifier)
                 (or (eq (aref text 0) ?\[)
                     (eq (aref text 0) ?\{)
                     (eq (aref text 0) ?\<)
                     (eq (aref text 0) ?\()))
            (setq text (concat "\\" text)))
        (alert text
               :title (format "\\[%s] from %s" room-name user-name)
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
