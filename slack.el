;;; slack.el --- Emacs Slack Client
;;; Commentary:
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
;; Keywords: tools,convenience,hypermedia,
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
  (request
   slack-authorize-url
   :params (list (cons "token" slack-token))
   :parser #'slack-parse-to-plist
   :success #'slack-on-authorize
   :error #'slack-on-authorize-e))

(defun slack-on-authorize (&key data &allow-other-keys &rest _)
  (let ((status (plist-get data :ok)))
    (unless status
      (error "slack-on-authorize: %s" data)))
  (setq slack-self        (plist-get data :self))
  (setq slack-my-user-id (plist-get slack-self :id))
  (setq slack-my-user-name (plist-get slack-self :name))
  (setq slack-team        (plist-get data :team))
  (setq slack-channels    (mapcar #'slack-channel-create
                                  (plist-get data :channels)))
  (setq slack-groups      (mapcar #'slack-group-create
                                  (plist-get data :groups)))
  (setq slack-ims         (mapcar #'slack-im-create
                                  (plist-get data :ims)))
  (setq slack-users       (plist-get data :users))
  (setq slack-bots        (plist-get data :bots))
  (setq slack-ws-url      (plist-get data :url))
  (message "Slack Authorization Finished.")
  (slack-ws-open))

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
;;; slack ends here
