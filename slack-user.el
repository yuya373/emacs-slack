;;; slack-user.el ---slack user interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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

(require 'slack-request)
(require 'slack-room)

(defconst slack-user-profile-set-url "https://slack.com/api/users.profile.set")
(defconst slack-bot-info-url "https://slack.com/api/bots.info")
(defconst slack-bot-list-url "https://slack.com/api/bots.list")

(defun slack-user-find (id team)
  (with-slots (users) team
    (cl-find-if (lambda (user)
                  (string= id (plist-get user :id)))
                users)))

(defun slack-user-find-by-name (name team)
  (with-slots (users) team
    (cl-find-if (lambda (user)
                  (string= name (plist-get user :name)))
                users)))

(defun slack-user-get-id (name team)
  (let ((user (slack-user-find-by-name name team)))
    (if user
        (plist-get user :id))))

(defun slack-user-name (id team)
  (let ((user (slack-user-find id team)))
    (if user
        (plist-get user :name))))

(defun slack-user-status (id team)
  (let* ((user (slack-user-find id team))
         (profile (and user (plist-get user :profile)))
         (emoji (and profile (plist-get profile :status_emoji)))
         (text (and profile (plist-get profile :status_text))))
    (when (and emoji text)
      (format "%s %s" emoji text))))

(defun slack-user-names (team)
  (with-slots (users) team
    (mapcar (lambda (u) (cons (plist-get u :name) u))
            users)))

(defun slack-user-presence-to-string (user)
  (if (string= (plist-get user :presence) "active")
      "* "
    "  "))

(defun slack-user-set-status ()
  (interactive)
  (let ((team (slack-team-select))
        (emoji (slack-select-emoji))
        (text (read-from-minibuffer "Text: ")))
    (slack-user-set-status-request  team emoji text)))

(defun slack-user-set-status-request (team emoji text)
  (cl-labels ((on-success
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-user-set-status-request"))))
    (slack-request
     slack-user-profile-set-url
     team
     :type "POST"
     :data (list (cons "id" (oref team self-id))
                 (cons "profile"
                       (json-encode (list (cons "status_text" text)
                                          (cons "status_emoji" emoji)))))
     :success #'on-success)))

(defun slack-bot-info-request (bot_id team &optional after-success)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-bot-info-request")
                    (push (plist-get data :bot) (oref team bots))
                    (if after-success
                        (funcall after-success team)))))
    (slack-request
     slack-bot-info-url
     team
     :params (list (cons "bot" bot_id))
     :success #'on-success
     :sync nil)))

(defun slack-bot-list-update (&optional team)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-bot-list-update")
           (oset team bots (append (plist-get data :bots) nil)))))
      (slack-request
       slack-bot-list-url
       team
       :success #'on-success
       :sync nil))))

(provide 'slack-user)
;;; slack-user.el ends here
