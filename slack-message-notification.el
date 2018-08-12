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

(defcustom slack-message-im-notification-title-format-function
  #'(lambda (team-name room-name thread-messagep)
      (format "%s - %s" team-name (if thread-messagep
                                      (format "Thread in %s" room-name)
                                    room-name)))
  "Function to format notification title for IM message.\ntake 3 Arguments.\n(lambda (TEAM-NAME ROOM-NAME THREAD-MESSAGEP) ...)."
  :type 'function
  :group 'slack)

(defcustom slack-message-notification-title-format-function
  #'(lambda (team-name room-name thread-messagep)
      (format "%s - %s" team-name (if thread-messagep
                                      (format "Thread in #%s" room-name)
                                    (format "#%s" room-name))))
  "Function to format notification title for non-IM message.\ntake 3 Arguments.\n(lambda (TEAM-NAME ROOM-NAME THREAD-MESSAGEP) ...)."
  :type 'function
  :group 'slack)

(defcustom slack-alert-icon nil
  "String passed as the :icon argument to `alert'."
  :type '(choice file (const :tag "Stock alert icon" nil))
  :group 'slack)

(defvar slack-modeline nil)

(defcustom slack-modeline-formatter #'slack-default-modeline-formatter
  "Format modeline with Arg '((team-name . unread-count))."
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
            (room-name (slack-room-name room team))
            (text (with-temp-buffer
                    (goto-char (point-min))
                    (insert (slack-message-to-alert message team))
                    (slack-buffer-buttonize-link)
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
            (user-name (slack-message-sender-name message team)))
        (if (and (eq alert-default-style 'notifier)
                 (slack-im-p room)
                 (or (eq (aref text 0) ?\[)
                     (eq (aref text 0) ?\{)
                     (eq (aref text 0) ?\<)
                     (eq (aref text 0) ?\()))
            (setq text (concat "\\" text)))
        (alert (if (slack-im-p room) text (format "%s: %s" user-name text))
               :icon slack-alert-icon
               :title (if (slack-im-p room)
                          (funcall slack-message-im-notification-title-format-function
                                   team-name room-name (slack-thread-messagep message))
                        (funcall slack-message-notification-title-format-function
                                 team-name room-name (slack-thread-messagep message)))
               :category 'slack))))

(defmethod slack-message-sender-equalp ((_m slack-message) _sender-id)
  nil)

(defmethod slack-message-minep ((m slack-message) team)
  (if team
      (with-slots (self-id) team
        (slack-message-sender-equalp m self-id))
    (slack-message-sender-equalp m (oref team self-id))))

(defun slack-default-modeline-formatter (alist)
  "Arg is alist of '((team-name . unread-count))"
  (if (= 1 (length alist))
      (format "[ %s: %s ]" (caar alist) (cdar alist))
    (mapconcat #'(lambda (e) (format "[ %s: %s ]" (car e) (cdr e)))
               alist " ")))

(defun slack-enable-modeline ()
  (add-to-list 'global-mode-string '(:eval slack-modeline) t))

(defun slack-update-modeline ()
  (let ((teams (cl-remove-if-not #'slack-team-modeline-enabledp slack-teams)))
    (when (< 0 (length teams))
      (setq slack-modeline
            (funcall slack-modeline-formatter
                     (mapcar #'(lambda (e) (cons (or (oref e modeline-name) (slack-team-name e))
                                                 (slack-team-get-unread-messages e)))
                             teams)))
      (force-mode-line-update))))

(defun slack-message-test-notification ()
  "Debug notification.
Execute this function when cursor is on some message."
  (interactive)
  (let ((ts (slack-get-ts)))
    (with-slots (room team) slack-current-buffer
      (let ((message (slack-room-find-message room ts)))
        (slack-message-notify message room team)))))

(provide 'slack-message-notification)
;;; slack-message-notification.el ends here
