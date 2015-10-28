;;; slack.el --- Emacs Slack Client
;;; Commentary:
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
;; Keywords: tools,convenience,hypermedia,
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'websocket)
(require 'oauth2)
(require 'popup)

(require 'slack-group)
(require 'slack-im)
(require 'slack-buffer)
(require 'slack-user)
(require 'slack-request)
(require 'slack-websocket)
(require 'slack-message)
(require 'slack-message-formatter)
(require 'slack-message-notification)
(require 'slack-message-sender)
(require 'slack-user-message)
(require 'slack-bot-message)
(require 'slack-reply)

(defgroup slack nil
  "Emacs Slack Client"
  :prefix "slack-"
  :group 'tools)

(defvar slack-ws nil)
(defvar slack-ws-url nil)
(defvar slack-groups)
(defvar slack-ims)
(defvar slack-users)
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

(define-derived-mode slack-mode fundamental-mode "Slack")

(defun slack-authorize ()
  (request
   slack-authorize-url
   :type "GET"
   :params (list (cons "token" slack-token))
   :parser #'slack-parse-to-plist
   :success #'slack-on-authorize
   :error #'slack-on-authorize-e))

(defun slack-on-authorize (&key data &allow-other-keys &rest _)
  (let ((status (plist-get data :ok)))
    (unless status
      (error "slack-on-authorize: %s" data)))
  (setq slack-self        (plist-get data :self))
  (setq slack-team        (plist-get data :team))
  (setq slack-channels    (plist-get data :channels))
  (setq slack-groups      (mapcar #'slack-group-create
                                  (plist-get data :groups)))
  (setq slack-ims         (mapcar #'slack-im-create
                                  (plist-get data :ims)))
  (setq slack-users       (plist-get data :users))
  (setq slack-bots        (plist-get data :bots))
  (setq slack-ws-url      (plist-get data :url))
  (message "Slack Authorization Finished.")
  (slack-ws-open)
  )

(defun slack-on-authorize-e
    (&key error-thrown &allow-other-keys &rest_)
  (error "slack-authorize: %s" error-thrown))

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
  (cl-find-if (lambda (bot)
             (string= id (plist-get bot :id)))
           slack-bots))

(defun slack-bot-name (id)
  (let ((bot (slack-find-bot id)))
    (if bot
        (plist-get bot :name))))

(defun slack-class-have-slot-p (class slot)
  (and (symbolp slot)
       (let* ((stripped (substring (symbol-name slot) 1))
              (replaced (replace-regexp-in-string "_" "-"
                                                  stripped))
              (symbolized (intern replaced)))
         (slot-exists-p class symbolized))))

(defun slack-collect-slots (class m)
  (cl-mapcan #'(lambda (property)
              (if (slack-class-have-slot-p class property)
                  (list property (plist-get m property))))
          m))

(provide 'slack)
;;; slack ends here

