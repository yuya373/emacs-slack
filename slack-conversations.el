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

;; TODO
;; https://slack.com/api/conversations.setPurpose
;; https://slack.com/api/conversations.setTopic
(defconst slack-conversations-set-topic-url
  "https://slack.com/api/conversations.setTopic")

(cl-defun slack-conversations-success-handler (team)
  (cl-function
   (lambda (&key data &allow-other-keys)
     (cl-labels
         ((log-error (e) (slack-log (format "%s" (replace-regexp-in-string "_" " " e))
                                    team :level 'error)))
       (slack-request-handle-error
        (data "conversations" #'log-error)
        (slack-if-let* ((warning (plist-get data :warning)))
            (slack-log (format "%s" warning) team
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
  (let ((channel (oref room id))
        (users (plist-get (slack-select-from-list
                              ((slack-user-names team)
                               "Select User: ")) :id)))
    (slack-request
     (slack-request-create
      slack-conversations-invite-url
      team
      :type "POST"
      :params (list (cons "channel" channel)
                    (cons "users" users))
      :success (slack-conversations-success-handler team)
      ))))

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

(provide 'slack-conversations)
;;; slack-conversations.el ends here
