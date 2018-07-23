;;; slack-slash-commands.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(defvar slack-slash-commands-map
  '(("active" . slack-slash-commands-active)
    ("away" . slack-slash-commands-away)
    ("dnd" . slack-slash-commands-dnd)
    ("leave" . slack-slash-commands-leave)
    ("join" . slack-slash-commands-join)
    ("remind" . slack-slash-commands-remind)
    ("shrug" . slack-slash-commands-shrug)
    ("status" . slack-slash-commands-status)
    ("who" . slack-slash-commands-who)
    ("dm" . slack-slash-commands-dm)))

(defvar slack-slash-commands-available
  (mapcar #'car slack-slash-commands-map))

(defun slack-slash-commands-parse (text)
  (if (string-prefix-p "/" text)
      (let* ((maybe-command (car (split-string (substring text 1) " ")))
             (command (cl-find maybe-command slack-slash-commands-available
                               :test #'string=)))
        (when command
          (cons command (cdr (split-string text " ")))))))

(defun slack-slash-commands-find (command)
  (cdr (cl-assoc command slack-slash-commands-map :test #'string=)))

(defun slack-slash-commands-execute (command team)
  (let ((command (car command))
        (args (cdr command)))
    (funcall (slack-slash-commands-find command) team args)))

(defun slack-slash-commands-active (team _args)
  ;; https://api.slack.com/docs/presence-and-status#user_presence
  ;; Setting presence back to auto indicates that the automatic status should be used instead. There's no way to force a user status to active.
  (slack-request-set-presence team "auto"))

(defun slack-slash-commands-away (team _args)
  (slack-request-set-presence team))

(defun slack-slash-commands-leave (team _args)
  (slack-channel-leave team t))

(defun slack-slash-commands-join (team _args)
  (slack-channel-join team t))

(defun slack-slash-commands-remind (team _args)
  (slack-reminder-add team))

(defun slack-slash-commands-dnd (team args)
  "[some description of time, see `slack-parse-time-string']"
  (let ((time (car args))
        (user (slack-user--find (oref team self-id) team)))
    (if (or (not (slack-user-dnd-in-range-p user))
            time)
        (slack-request-dnd-set-snooze team time)
      (slack-request-dnd-end-dnd team))))
;; ¯\_(ツ)_/¯
(defun slack-slash-commands-shrug (_team messages)
  "[your message]"
  (slack-message--send (format "%s ¯\\_(ツ)_/¯"
                               (mapconcat #'identity messages " "))))

(defun slack-slash-commands-status (team args)
  "[clear] or [:your_new_status_emoji:] [your new status message]"
  (let ((emoji (car args))
        (text (cdr args)))
    (if (string= emoji "clear")
        (slack-user-set-status-request team "" "")
      (if (string-prefix-p ":" emoji)
          (slack-user-set-status-request team emoji (mapconcat #'identity text " "))
        (slack-user-set-status-request team "" (mapconcat #'identity args " "))))))

(defun slack-slash-commands-who (_team _args)
  (slack-room-user-select))

(defun slack-slash-commands-dm (team args)
  "@user [your message]"
  (let* ((user-name (substring (car args) 1))
         (text (mapconcat #'identity (cdr args) " "))
         (user (slack-user-find-by-name user-name team)))
    (unless user
      (error "Invalid user name: %s" (car args)))
    (cl-labels
        ((send-message
          ()
          (slack-message-send-internal
           text
           (oref (slack-im-find-by-user-id (plist-get user :id)
                                           team)
                 id)
           team)))
      (if (slack-im-find-by-user-id (plist-get user :id) team)
          (send-message)
        (slack-im-open user #'send-message)))))

(defclass slack-command ()
  ((name :initarg :name :type string)
   (type :initarg :type :type string)
   (usage :initarg :usage :type string :initform "")
   (desc :initarg :desc :type string :initform "")
   (alias-of :initarg :alias_of :type string)))

(defclass slack-core-command (slack-command)
  ((canonical-name :initarg :canonical_name :type string)))

(defclass slack-app-command (slack-command)
  ((app :initarg :app :type string)))

(defclass slack-service-command (slack-command)
  ((service-name :initarg :service_name :type string)))

(defun slack-command-create (command)
  (cl-labels
      ((slack-core-command-create
        (payload)
        (apply #'make-instance 'slack-core-command
               (slack-collect-slots 'slack-core-command payload)))
       (slack-app-command-create
        (payload)
        (apply #'make-instance 'slack-app-command
               (slack-collect-slots 'slack-app-command payload)))
       (slack-service-command-create
        (payload)
        (apply #'make-instance 'slack-service-command
               (slack-collect-slots 'slack-service-command payload))))
    (let ((type (plist-get command :type)))
      (cond
       ((string= type "core")
        (slack-core-command-create command))
       ((string= type "app")
        (slack-app-command-create command))
       ((string= type "service")
        (slack-service-command-create command))
       (t (apply #'make-instance 'slack-command command))))))

(defun slack-command-list-update (&optional team)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-commands-list-request")
           (let ((commands (mapcar #'(lambda (command) (slack-command-create command))
                                   (cl-remove-if-not #'listp
                                                     (plist-get data :commands)))))
             (oset team commands commands)
             (slack-log "Slack Command List Updated" team :level 'info)))))
      (slack-request
       (slack-request-create
        "https://slack.com/api/commands.list"
        team
        :type "POST"
        :success #'on-success)))))

(defun slack-command-find (name team)
  (let ((commands (oref team commands)))
    (cl-find-if #'(lambda (command) (string= name
                                             (oref command name)))
                commands)))

(defmethod slack-command-company-doc-string ((this slack-command) team)
  (if (slot-boundp this 'alias-of)
      (let ((command (slack-command-find (oref this alias-of)
                                         team)))
        (when command
          (slack-command-company-doc-string command team)))
    (with-slots (usage desc) this
      (format "%s%s"
              (or (and (< 0 (length usage))
                       (format "%s\n" usage))
                  "")
              desc))))
(provide 'slack-slash-commands)
;;; slack-slash-commands.el ends here
