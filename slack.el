;;; slack.el --- slack client for emacs              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords: tools
;; Version: 0.0.1
;; Package-Requires: ((websocket "1.5") (request "0.2.0") (oauth2 "0.10") (circe "2.1") (alert "1.2") (emojify "0.2"))
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

(require 'cl-lib)
(require 'oauth2)

(require 'slack-team)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-file)
(require 'slack-message-notification)
(require 'slack-message-sender)
(require 'slack-message-editor)
(require 'slack-message-reaction)
(require 'slack-user-message)
(require 'slack-bot-message)

(require 'slack-websocket)
(require 'slack-request)

(defgroup slack nil
  "Emacs Slack Client"
  :prefix "slack-"
  :group 'tools)

(defcustom slack-redirect-url "http://localhost:8080"
  "Redirect url registered for Slack.")
(defcustom slack-buffer-function #'switch-to-buffer-other-window
  "Function to print buffer.")

(defconst slack-oauth2-authorize "https://slack.com/oauth/authorize")
(defconst slack-oauth2-access "https://slack.com/api/oauth.access")
(defconst slack-authorize-url "https://slack.com/api/rtm.start")

(defun slack-authorize (team &optional error-callback)
  (slack-request
   slack-authorize-url
   team
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (slack-on-authorize data team)))
   :sync nil
   :error error-callback))

(defun slack-update-team (data team)
  (cl-labels
      ((create-room (data func)
                    (unless (plist-get data :last_read)
                      (plist-put data :last_read "0"))
                    (if (plist-get data :latest)
                        (plist-put data :latest
                                   (slack-message-create
                                    (plist-get data  :latest))))
                    (funcall func data))
       (create-rooms (datum team func)
                     (mapcar #'(lambda (data)
                                 (plist-put data :team-id (oref team id))
                                 (create-room data func))
                             (append datum nil))))
    (let ((self (plist-get data :self))
          (team-data (plist-get data :team)))
      (oset team id (plist-get team-data :id))
      (oset team name (plist-get team-data :name))
      (oset team channels (create-rooms (plist-get data :channels)
                                        team
                                        #'slack-channel-create))
      (oset team groups (create-rooms (plist-get data :groups)
                                      team
                                      #'slack-group-create))
      (oset team ims (create-rooms (plist-get data :ims)
                                   team
                                   #'slack-im-create))
      (oset team self self)
      (oset team self-id (plist-get self :id))
      (oset team self-name (plist-get self :name))
      (oset team users (append (plist-get data :users) nil))
      (oset team bots (append (plist-get data :bots) nil))
      (oset team ws-url (plist-get data :url))
      (oset team connected t)
      team)))

(cl-defun slack-on-authorize (data team)
  (slack-request-handle-error
   (data "slack-authorize")
   (message "Slack Authorization Finished.")
   (let ((team (slack-update-team data team)))
     (with-slots (groups ims channels) team
       (mapc #'(lambda (room)
                 (if (slack-room-subscribedp room team)
                     (slack-room-history room
                                         team
                                         nil
                                         nil
                                         t)))
             (append groups ims channels)))
     (slack-ws-open team))))

(defun slack-on-authorize-e
    (&key error-thrown &allow-other-keys &rest_)
  (error "slack-authorize: %s" error-thrown))

(defun slack-oauth2-auth (team)
  (with-slots (client-id client-secret) team
    (oauth2-auth
     slack-oauth2-authorize
     slack-oauth2-access
     client-id
     client-secret
     "client"
     nil
     slack-redirect-url)))

(defun slack-request-token (team)
  (with-slots (token) team
    (setq token
          (oauth2-token-access-token
           (slack-oauth2-auth team)))))

;;;###autoload
(defun slack-start (&optional team)
  (interactive)
  (cl-labels ((start
               (team)
               (with-slots (ws-conn token) team
                 (if ws-conn
                     (slack-ws-close team))
                 (unless token
                   (slack-request-token team)))
               (slack-authorize team)
               (sleep-for 0.5)))
    (if team
        (start team)
      (mapcan #'start slack-teams))))

(provide 'slack)
;;; slack.el ends here
