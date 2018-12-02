;;; slack.el --- slack client for emacs              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords: tools
;; Version: 0.0.2
;; Package-Requires: ((websocket "1.8") (request "0.2.0") (oauth2 "0.10") (circe "2.3") (alert "1.2") (emojify "0.4") (emacs "24.4"))
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
(require 'subr-x)
(require 'color)

(require 'slack-util)
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
(require 'slack-search)
(require 'slack-reminder)
(require 'slack-thread)
(require 'slack-attachment)
(require 'slack-emoji)
(require 'slack-star)

(require 'slack-buffer)
(require 'slack-message-buffer)
(require 'slack-message-edit-buffer)
(require 'slack-message-share-buffer)
(require 'slack-thread-message-buffer)
(require 'slack-all-threads-buffer)
(require 'slack-room-message-compose-buffer)
(require 'slack-pinned-items-buffer)
(require 'slack-user-profile-buffer)
(require 'slack-file-list-buffer)
(require 'slack-file-info-buffer)
(require 'slack-thread-message-compose-buffer)
(require 'slack-search-result-buffer)
(require 'slack-stars-buffer)
(require 'slack-dialog-buffer)
(require 'slack-dialog-edit-element-buffer)

(require 'slack-websocket)
(require 'slack-request)
(require 'slack-usergroup)
(require 'slack-unread)

(require 'slack-oauth2)
(require 'slack-authorize)

(when (featurep 'helm)
  (require 'helm-slack))

(require 'slack-company)

(defgroup slack nil
  "Emacs Slack Client"
  :prefix "slack-"
  :group 'tools)

(defcustom slack-buffer-function #'switch-to-buffer-other-window
  "Function to print buffer."
  :type 'function
  :group 'slack)

(defvar slack-use-register-team-string
  "use `slack-register-team' instead.")

(defcustom slack-client-id nil
  "Client ID provided by Slack."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-client-id slack-use-register-team-string
 "0.0.2")

(defcustom slack-client-secret nil
  "Client Secret Provided by Slack."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-client-secret slack-use-register-team-string
 "0.0.2")

(defcustom slack-token nil
  "Slack token provided by Slack.
set this to save request to Slack if already have."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-token slack-use-register-team-string
 "0.0.2")

(defcustom slack-room-subscription '()
  "Group or Channel list to subscribe notification."
  :type '(repeat string)
  :group 'slack)
(make-obsolete-variable
 'slack-room-subscription slack-use-register-team-string
 "0.0.2")

(defcustom slack-typing-visibility 'frame
  "When to display typing indicator.
When `frame', typing slack buffer is in the current frame.
When `buffer', typing slack buffer is the current buffer.
When `never', never display typing indicator."
  :type '(choice (const frame)
                 (const buffer)
                 (const never))
  :group 'slack)

(defcustom slack-display-team-name t
  "If nil, only display channel, im, group name."
  :type 'boolean
  :group 'slack)

(defcustom slack-completing-read-function #'completing-read
  "Require same argument with `completing-read'."
  :type 'function
  :group 'slack)


;;;###autoload
(defun slack-start (&optional team)
  (interactive)
  (cl-labels ((start
               (team)
               (slack-team-kill-buffers team)
               (when (slot-boundp team 'ws)
                 (slack-ws--close (oref team ws) team))
               (when (slack-team-need-token-p team)
                 (let ((token (slack-oauth2-get-token team)))
                   (oset team token token)
                   (kill-new token))
                 (message "Your Token is added to kill ring."))
               (slack-authorize team)))
    (if team
        (start team)
      (if slack-teams
          (cl-loop for team in slack-teams
                   do (start team))
        (slack-start (call-interactively #'slack-register-team))))
    (slack-enable-modeline)))

(provide 'slack)
;;; slack.el ends here
