;;; slack.el --- Emacs Slack Client
;;; Commentary:
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
;; Keywords: tools,convenience,hypermedia,
;;; Code:

(require 'cl)
(require 'websocket)
(require 'json)
(require 'request)
(require 'oauth2)
(require 'popup)

(require 'slack-buffer)
(require 'slack-group)
(require 'slack-im)
(require 'slack-message)
(require 'slack-request)
(require 'slack-user)
(require 'slack-websocket)

(defvar slack-oauth2-authorize "https://slack.com/oauth/authorize")
(defvar slack-oauth2-access "https://slack.com/api/oauth.access")
(defvar slack-authorize-url "https://slack.com/api/rtm.start")
(defvar client-id nil)
(defvar client-secret nil)
(defvar slack-token nil)
(defvar slack-oauth2-token)
(defvar slack-self)
(defvar slack-team)
(defvar slack-channels)
(defvar slack-bots)

;; (defvar slack-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     map))

;; (define-minor-mode slack-mode ()
;;   "Enable Slack"
;;   :group 'slack
;;   :init-value nil
;;   :global nil
;;   :keymap slack-mode-map
;;   :lighter " Slack")

(defun slack-authorize ()
  (request
   slack-authorize-url
   :type "GET"
   :params (list (cons "token" slack-token))
   :parser #'slack-parse-to-hash
   :success #'slack-on-authorize
   :error #'slack-on-authorize-e))

(defun slack-on-authorize (&key data &allow-other-keys &rest _)
  (let ((status (gethash "ok" data)))
    (unless status
      (error "slack-on-authorize: %s" data)))
  (setq slack-self (gethash "self" data))
  (setq slack-team (gethash "team" data))
  (setq slack-channels (gethash "channels" data))
  (setq slack-groups (gethash "groups" data))
  (setq slack-ims (append (gethash "ims"  data) nil))
  (setq slack-users (gethash "users" data))
  (setq slack-bots (gethash "bots" data))
  (setq slack-ws-url (gethash "url" data))
  (slack-ws-open slack-ws-url))

(defun slack-on-authorize-e
    (&key error-thrown &allow-other-keys &rest_)
  (error "slack-authorize: %s" e))

(defun slack-oauth2-auth ()
  (oauth2-auth
   slack-oauth2-authorize
   slack-oauth2-access
   client-id
   client-secret
   "client"
   nil
   "http://localhost:8080"))

(defun slack-request-token ()
  (let ((token (slack-oauth2-auth)))
    (setq slack-oauth2-token token)
    (setq slack-token
          (oauth2-token-access-token slack-oauth2-token))))

(defun slack-start ()
  (interactive)
  (if slack-ws
    (slack-ws-close))
  (unless slack-token
    (slack-request-token))
  (slack-authorize))

(defun slack-find-bot (id)
  (find-if (lambda (bot)
             (string= id (gethash "id" bot)))
           slack-bots))

(defun channel-names ()
  (mapcar (lambda (channel) (gethash "name" channel))
          slack-channels))

(defun slack-bot-name (id)
  (let ((bot (slack-find-bot id)))
    (if bot
        (gethash "name" bot))))

(provide 'slack)
;;; slack ends here

