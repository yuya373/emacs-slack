;;; slack.el --- slack client              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; URL: https://github.com/yuya373/emacs-slack
;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords: tools
;; Version: 0.0.2
;; Package-Requires: ((websocket "1.12") (request "0.3.2") (circe "2.11") (alert "1.2") (emojify "1.2.1") (emacs "25.1"))
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

;; Slack client in Emacs.

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
(require 'slack-message-faces)
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
(require 'slack-room-info-buffer)
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
(require 'slack-modeline)
(require 'slack-create-message)

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

(defcustom slack-enable-wysiwyg nil
  "If t, enable live markup in message compose or edit buffer."
  :type 'boolean
  :group 'slack)

;;;###autoload
(defun slack-start (&optional team)
  (interactive)
  (cl-labels ((start
               (team)
               (slack-team-kill-buffers team)
               (slack-if-let* ((ws (and (slot-boundp team 'ws)
                                        (oref team ws))))
                   (progn
                     (when (oref ws conn)
                       (slack-ws--close ws team))
                     (oset ws inhibit-reconnection nil)))
               (slack-authorize team)))
    (if team
        (start team)
      (if (hash-table-empty-p slack-teams-by-token)
          (slack-start (call-interactively #'slack-register-team))
        (cl-loop for team in (hash-table-values slack-teams-by-token)
                 do (start team))))
    (slack-enable-modeline)))

;;;###autoload
(defun slack-register-team (&rest plist)
  "PLIST must contain :name and :token.
Available options (property name, type, default value)
:subscribed-channels [ list symbol ] '()
  notified when new message arrived in these channels.
:default [boolean] nil
  if `slack-prefer-current-team' is t,
  some functions use this team without asking.
:full-and-display-names [boolean] nil
  if t, use full name to display user name.
:mark-as-read-immediately [boolean] these
  if t, mark messages as read when open channel.
  if nil, mark messages as read when cursor hovered.
:modeline-enabled [boolean] nil
  if t, display mention count and has unread in modeline.
:modeline-name [or nil string] nil
  use this value in modeline.
  if nil, use team name.
:visible-threads [boolean] nil
  if t, thread replies are also displayed in channel buffer.
:websocket-event-log-enabled [boolean] nil
  if t, websocket event is logged.
  use `slack-log-open-event-buffer' to open the buffer.
:animate-image [boolean] nil
  if t, animate gif images."
  (interactive
   (let* ((name (read-from-minibuffer "Team Name: "))
          (token (read-from-minibuffer "Token: "))
          (cookie (when (string= "xoxc" (substring token 0 4))
                    (read-from-minibuffer "Cookie: "))))
     (list :name name :token token :cookie cookie)))
  (cl-labels ((has-token-p (plist)
                           (let ((token (plist-get plist :token)))
                             (and token (< 0 (length token)))))
              (register (team)
                        (let ((same-team (slack-team-find-by-token (oref team token))))
                          (if same-team
                              (progn
                                (slack-team-disconnect same-team)
                                (slack-team-connect team))))
                        (puthash (oref team token) team slack-teams-by-token)
                        (if (plist-get plist :default)
                            (setq slack-current-team team))))

    (if (has-token-p plist)
        (let ((team (slack-create-team plist)))
          (register team))
      (error ":token is required"))))

(cl-defmethod slack-team-connect ((team slack-team))
  (unless (slack-team-connectedp team)
    (slack-start team)))

(defun slack-change-current-team ()
  (interactive)
  (let* ((alist (mapcar #'(lambda (team) (cons (slack-team-name team)
                                               (oref team token)))
                        (hash-table-values slack-teams-by-token)))
         (selected (funcall slack-completing-read-function "Select Team: " alist))
         (team (slack-team-find-by-token
                (cdr-safe (cl-assoc selected alist :test #'string=)))))
    (setq slack-current-team team)
    (message "Set slack-current-team to %s" (or (and team (oref team name))
                                                "nil"))
    (if team
        (slack-team-connect team))))

(provide 'slack)
;;; slack.el ends here
