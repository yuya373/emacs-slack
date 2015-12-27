;;; slack-group.el ---slack private group interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuya Minami

;; Author: Yuya Minami
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

(require 'eieio)
(require 'slack-room)
(require 'slack-util)
(require 'slack-buffer)

(defcustom slack-room-subscription '()
  "Group or Channel list to subscribe notification."
  :group 'slack)

(defconst slack--group-open-url "https://slack.com/api/groups.open")
(defconst slack-group-history-url "https://slack.com/api/groups.history")
(defconst slack-group-buffer-name "*Slack - Private Group*")
(defconst slack-group-list-url "https://slack.com/api/groups.list")
(defconst slack-group-update-mark-url "https://slack.com/api/groups.mark")
(defconst slack-create-group-url "https://slack.com/api/groups.create")
(defconst slack-group-rename-url "https://slack.com/api/groups.rename")
(defconst slack-group-invite-url "https://slack.com/api/groups.invite")

(defvar slack-groups)
(defvar slack-token)
(defvar slack-buffer-function)

(defclass slack-group (slack-room)
  ((name :initarg :name :type string)
   (is-group :initarg :is_group)
   (creator :initarg :creator)
   (is-archived :initarg :is_archived)
   (is-mpim :initarg :is_mpim)
   (members :initarg :members :type list)
   (topic :initarg :topic)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (purpose :initarg :purpose)))

(defun slack-group-create (payload)
  (plist-put payload :members (append (plist-get payload :members) nil))
  (apply #'slack-group "group"
         (slack-collect-slots 'slack-group payload)))

(defun slack-group-names ()
  (mapcar (lambda (group)
            (cons (oref group name) group))
          slack-groups))

(defmethod slack-room-subscribedp ((room slack-group))
  (with-slots (name) room
    (and name
         (memq (intern name) slack-room-subscription))))

(defmethod slack-room-buffer-name ((room slack-group))
  (concat slack-group-buffer-name " : " (slack-room-name room)))

(defmethod slack-room-buffer-header ((room slack-group))
  (concat "Private Group: " (slack-room-name room) "\n"))

(defmethod slack-room-history ((room slack-group) &optional oldest)
  (slack-room-request-update room
                             slack-group-history-url
                             oldest))

(defun slack-group-select ()
  (interactive)
  (slack-room-select slack-groups))

(defun slack-group-list-update ()
  (interactive)
  (cl-labels ((on-list-update
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-group-list-update")
                (setq slack-groups (mapcar #'slack-group-create
                                           (plist-get data :groups)))
                (message "Slack Group List Updated"))))
    (slack-room-list-update slack-group-list-url
                            #'on-list-update
                            :sync nil)))


(defmethod slack-room-update-mark-url ((_room slack-group))
  slack-group-update-mark-url)

(defun slack-create-group ()
  (interactive)
  (cl-labels
      ((on-create-group (&key data &allow-other-keys)
                        (slack-request-handle-error
                         (data "slack-create-group")
                         (let ((group (slack-group-create
                                       (plist-get data :group))))
                           (push group slack-groups)
                           (message "group: %s created!"
                                    (slack-room-name group))))))
    (slack-create-room slack-create-group-url
                       #'on-create-group)))

(defun slack-group-rename ()
  (interactive)
  (slack-room-rename slack-group-rename-url
                     (slack-group-names)))

(defun slack-group-invite ()
  (interactive)
  (slack-room-invite slack-group-invite-url
                     #'slack-group-names))

(provide 'slack-group)
;;; slack-group.el ends here
