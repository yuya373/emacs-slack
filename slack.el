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

(require 'slack-channel)
(require 'slack-im)
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

(defcustom slack-client-id nil
  "Client ID provided by Slack.")
(defcustom slack-client-secret nil
  "Client Secret Provided by Slack.")
(defcustom slack-redirect-url "http://localhost:8080"
  "Redirect url registered for Slack.")
(defcustom slack-buffer-function #'switch-to-buffer-other-window
  "Function to print buffer.")
(defcustom slack-token nil
  "Slack token provided by Slack.
set this to save request to Slack if already have.")

(defvar slack-oauth2-token)
(defvar slack-ims)
(defvar slack-groups)
(defvar slack-users)
(defvar slack-self)
(defvar slack-my-user-id)
(defvar slack-my-user-name)
(defvar slack-team)
(defvar slack-channels)
(defvar slack-bots)
(defconst slack-oauth2-authorize "https://slack.com/oauth/authorize")
(defconst slack-oauth2-access "https://slack.com/api/oauth.access")
(defconst slack-authorize-url "https://slack.com/api/rtm.start")

(defun slack-authorize ()
  (slack-request
   slack-authorize-url
   :params (list (cons "token" slack-token))
   :success #'slack-on-authorize))

(cl-defun slack-on-authorize (&key data &allow-other-keys)
  (slack-request-handle-error
   (data "slack-authorize")
   (setq slack-self        (plist-get data :self))
   (setq slack-my-user-id (plist-get slack-self :id))
   (setq slack-my-user-name (plist-get slack-self :name))
   (setq slack-team        (plist-get data :team))
   (cl-labels
       ((create-room (data func)
                     (plist-put data :last_read "0")
                     (funcall func data))
        (create-rooms (datum func)
                      (mapcar #'(lambda (data)
                                  (create-room data func))
                              (append datum nil))))
     (setq slack-channels (create-rooms (plist-get data :channels)
                                        #'slack-channel-create))
     (setq slack-groups (create-rooms (plist-get data :groups)
                                      #'slack-group-create))
     (setq slack-ims (create-rooms (plist-get data :ims)
                                   #'slack-im-create)))
   (setq slack-users       (append (plist-get data :users) nil))
   (setq slack-bots        (plist-get data :bots))
   (setq slack-ws-url      (plist-get data :url))
   (message "Slack Authorization Finished.")
   (slack-ws-open)))

(defun slack-on-authorize-e
    (&key error-thrown &allow-other-keys &rest_)
  (error "slack-authorize: %s" error-thrown))

(defun slack-oauth2-auth ()
  (oauth2-auth
   slack-oauth2-authorize
   slack-oauth2-access
   slack-client-id
   slack-client-secret
   "client"
   nil
   slack-redirect-url))

(defun slack-request-token ()
  (let ((token (slack-oauth2-auth)))
    (setq slack-oauth2-token token)
    (setq slack-token
          (oauth2-token-access-token slack-oauth2-token))))

;;;###autoload
(defun slack-start ()
  (interactive)
  (if slack-ws
    (slack-ws-close))
  (unless slack-token
    (slack-request-token))
  (slack-authorize))

(provide 'slack)
;;; slack.el ends here
