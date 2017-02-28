;;; slack-group.el --- slack private group interface  -*- lexical-binding: t; -*-

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

(defconst slack--group-open-url "https://slack.com/api/groups.open")
(defconst slack-group-buffer-name "*Slack - Private Group*")
(defconst slack-group-list-url "https://slack.com/api/groups.list")
(defconst slack-group-update-mark-url "https://slack.com/api/groups.mark")
(defconst slack-create-group-url "https://slack.com/api/groups.create")
(defconst slack-group-rename-url "https://slack.com/api/groups.rename")
(defconst slack-group-invite-url "https://slack.com/api/groups.invite")
(defconst slack-group-leave-url "https://slack.com/api/groups.leave")
(defconst slack-group-archive-url "https://slack.com/api/groups.archive")
(defconst slack-group-unarchive-url "https://slack.com/api/groups.unarchive")
(defconst slack-mpim-close-url "https://slack.com/api/mpim.close")
(defconst slack-mpim-open-url "https://slack.com/api/mpim.open")

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

(defun slack-group-names (team &optional filter)
  (with-slots (groups) team
    (slack-room-names groups filter)))

(defmethod slack-room-subscribedp ((room slack-group) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room)))
      (and name
           (memq (intern name) subscribed-channels)))))

(defmethod slack-room-buffer-name ((room slack-group))
  (concat slack-group-buffer-name
          " : "
          (slack-room-name-with-team-name room)))

(defun slack-group-select ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-room-select
     (cl-loop for team in (list team)
              for groups = (oref team groups)
              nconc groups))))

(defun slack-group-list-update ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels ((on-list-update
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-group-list-update")
                  (with-slots (groups) team
                    (setq groups
                          (mapcar #'(lambda (g)
                                      (slack-room-create g team 'slack-group))
                                  (plist-get data :groups))))
                  (message "Slack Group List Updated"))))
      (slack-room-list-update slack-group-list-url
                              #'on-list-update
                              team
                              :sync nil))))


(defmethod slack-room-update-mark-url ((_room slack-group))
  slack-group-update-mark-url)

(defun slack-create-group ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels
        ((on-create-group (&key data &allow-other-keys)
                          (slack-request-handle-error
                           (data "slack-create-group"))))
      (slack-create-room slack-create-group-url
                         team
                         #'on-create-group))))

(defun slack-group-rename ()
  (interactive)
  (slack-room-rename slack-group-rename-url
                     #'slack-group-names))

(defun slack-group-invite ()
  (interactive)
  (slack-room-invite slack-group-invite-url
                     #'slack-group-names))

(defun slack-group-leave ()
  (interactive)
  (let* ((team (slack-team-select))
         (group (slack-current-room-or-select
                 #'(lambda ()
                     (slack-group-names team)))))
    (cl-labels
        ((on-group-leave
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-group-leave")
           (with-slots (groups) team
             (setq groups
                   (cl-delete-if #'(lambda (g)
                                     (slack-room-equal-p group g))
                                 groups)))
           (message "Left Group: %s"
                    (slack-room-name-with-team-name group)))))
      (slack-room-request-with-id slack-group-leave-url
                                  (oref group id)
                                  team
                                  #'on-group-leave))))

(defmethod slack-room-archived-p ((room slack-group))
  (oref room is-archived))

(defun slack-group-archive ()
  (interactive)
  (let* ((team (slack-team-select))
         (group (slack-current-room-or-select
                 #'(lambda ()
                     (slack-group-names
                      team
                      #'(lambda (groups)
                          (cl-remove-if #'slack-room-archived-p
                                        groups)))))))
    (cl-labels
        ((on-group-archive (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-group-archive"))))
      (slack-room-request-with-id slack-group-archive-url
                                  (oref group id)
                                  team
                                  #'on-group-archive))))

(defun slack-group-unarchive ()
  (interactive)
  (let* ((team (slack-team-select))
         (group (slack-current-room-or-select
                 #'(lambda ()
                     (slack-group-names
                      team
                      #'(lambda (groups)
                          (cl-remove-if-not #'slack-room-archived-p
                                            groups)))))))
    (cl-labels
        ((on-group-unarchive (&key _data &allow-other-keys)
                             (data "slack-group-unarchive")))
      (slack-room-request-with-id slack-group-unarchive-url
                                  (oref group id)
                                  team
                                  #'on-group-unarchive))))


(defun slack-group-members-s (group)
  (with-slots (members team-id) group
    (mapconcat #'(lambda (user) (slack-user-name user
                                                 (slack-team-find team-id)))
               members ", ")))


(defun slack-group-mpim-open ()
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-names team)))
    (cl-labels
        ((select-users (users acc)
                       (let ((selected (completing-read "Select User: "
                                                        users nil t)))
                         (if (< 0 (length selected))
                             (select-users users
                                           (push (cdr (cl-assoc selected users :test #'string=)) acc))
                           acc)))
         (on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-group-mpim-open")
           (if (plist-get data :already_open)
               (message "Direct Message Channel with %s Already Open"
                        (slack-group-members-s (slack-room-find (oref selected id) team)))
             (oset team groups
                   (cons (slack-room-create (plist-get data :group) team 'slack-group)
                         (oref team groups)))))))
      (slack-request
       slack-mpim-open-url
       team
       :type "POST"
       :params (list (cons "users" (mapconcat (lambda (u) (plist-get u :id)) (select-users users '()) ",")))
       :success #'on-success
       :sync nil))))

(defun slack-group-mpim-close ()
  (interactive)
  (let* ((team (slack-team-select)))
    (slack-select-from-list
     ((slack-group-names team #'(lambda (groups)
                                  (cl-remove-if-not #'slack-mpim-p
                                        groups)))
      "Select MPIM: ")
     (cl-labels
         ((on-success
           (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-group-mpim-close")
            (let ((group (slack-room-find (oref selected id) team)))
              (with-slots (groups) team
                (setq groups
                      (cl-delete-if #'(lambda (g)
                                        (slack-room-equal-p group g))
                                    groups)))
              (if (plist-get data :already_closed)
                  (message "Direct Message Channel with %s Already Closed"
                           (slack-group-members-s group)))))))
       (slack-request
        slack-mpim-close-url
        team
        :type "POST"
        :params (list (cons "channel" (oref selected id)))
        :success #'on-success
        :sync nil)))))


(defmethod slack-mpim-p ((room slack-group))
  (oref room is-mpim))

(provide 'slack-group)
;;; slack-group.el ends here
