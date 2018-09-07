;;; slack-conversations.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-request)
(require 'slack-room)

(defconst slack-conversations-archive-url
  "https://slack.com/api/conversations.archive")
(defconst slack-conversations-invite-url
  "https://slack.com/api/conversations.invite")
(defconst slack-conversations-join-url
  "https://slack.com/api/conversations.join")
(defconst slack-conversations-leave-url
  "https://slack.com/api/conversations.leave")
(defconst slack-conversations-rename-url
  "https://slack.com/api/conversations.rename")
(defconst slack-conversations-set-purpose-url
  "https://slack.com/api/conversations.setPurpose")
(defconst slack-conversations-set-topic-url
  "https://slack.com/api/conversations.setTopic")

(cl-defun slack-conversations-success-handler (team &optional errors-handler)
  (cl-function
   (lambda (&key data &allow-other-keys)
     (cl-labels
         ((replace-underscore-with-space (s)
                                         (replace-regexp-in-string "_"
                                                                   " "
                                                                   s))
          (log-error
           (_)
           (slack-if-let*
               ((err (plist-get data :error))
                (message (format "%s"
                                 (replace-underscore-with-space
                                  err))))
               (slack-log message team :level 'error))
           (slack-if-let*
               ((errors (plist-get data :errors))
                (has-handler (functionp errors-handler)))
               (funcall errors-handler errors))))
       (slack-request-handle-error
        (data "conversations" #'log-error)
        (slack-if-let* ((warning (plist-get data :warning)))
            (slack-log (format "%s" (replace-underscore-with-space
                                     warning))
                       team
                       :level 'warn)))))))

(defun slack-conversations-archive (room team)
  (let ((id (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-archive-url
      team
      :type "POST"
      :params (list (cons "channel" id))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-invite (room team)
  (let* ((channel (oref room id))
         (user-names (slack-user-names team))
         (users nil))
    (cl-labels
        ((already-selected-p
          (user-name)
          (cl-find-if #'(lambda (e)
                          (string= e
                                   (plist-get (cdr user-name)
                                              :id)))
                      users))
         (filter-selected (user-names)
                          (cl-remove-if #'already-selected-p
                                        user-names)))
      (cl-loop for i from 1 upto 30
               as candidates = (filter-selected user-names)
               as selected = (slack-select-from-list
                                 (candidates "Select User: "))
               while selected
               do (push (plist-get selected :id) users)))
    (setq users (mapconcat #'identity users ","))

    (cl-labels
        ((errors-handler
          (errors)
          (let ((message
                 (mapconcat #'(lambda (err)
                                (let ((msg (plist-get err :error))
                                      (user (plist-get err :user)))
                                  (format "%s%s"
                                          (replace-regexp-in-string "_" " " msg)
                                          (or (and user (format ": %s" user))
                                              ""))))
                            errors
                            ", ")))
            (slack-log message team :level 'error))))
      (slack-request
       (slack-request-create
        slack-conversations-invite-url
        team
        :type "POST"
        :params (list (cons "channel" channel)
                      (cons "users" users))
        :success (slack-conversations-success-handler team #'errors-handler))))))

(defun slack-conversations-join (room team)
  (let ((channel (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-join-url
      team
      :type "POST"
      :params (list (cons "channel" channel))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-leave (room team)
  (let ((channel (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-leave-url
      team
      :type "POST"
      :params (list (cons "channel" channel))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-rename (room team)
  (let ((channel (oref room id))
        (name (read-from-minibuffer "Name: ")))
    (slack-request
     (slack-request-create
      slack-conversations-rename-url
      team
      :type "POST"
      :params (list (cons "channel" channel)
                    (cons "name" name))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-set-purpose (room team)
  (let ((channel (oref room id))
        (purpose (read-from-minibuffer "Purpose: ")))
    (slack-request
     (slack-request-create
      slack-conversations-set-purpose-url
      team
      :type "POST"
      :params (list (cons "channel" channel)
                    (cons "purpose" purpose))
      :success (slack-conversations-success-handler team)))))

(defun slack-conversations-set-topic (room team)
  (let ((channel (oref room id))
        (topic (read-from-minibuffer "Topic: ")))
    (slack-request
     (slack-request-create
      slack-conversations-set-topic-url
      team
      :type "POST"
      :params (list (cons "channel" channel)
                    (cons "topic" topic))
      :success (slack-conversations-success-handler team)))))

(provide 'slack-conversations)
;;; slack-conversations.el ends here
