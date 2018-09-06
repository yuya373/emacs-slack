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
(require 'slack-request)

(defconst slack-group-history-url "https://slack.com/api/groups.history")
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
(defconst slack-group-info-url "https://slack.com/api/groups.info")

(defvar slack-buffer-function)

(defclass slack-group (slack-room)
  ((is-group :initarg :is_group :initform nil)
   (creator :initarg :creator :initform "")
   (is-archived :initarg :is_archived :initform nil)
   (is-mpim :initarg :is_mpim :initform nil)
   (topic :initarg :topic :initform nil)
   (purpose :initarg :purpose :initform nil)))

(defmethod slack-merge ((this slack-group) other)
  (call-next-method)
  (with-slots (is-group creator is-archived is-mpim members topic purpose) this
    (setq is-group (oref other is-group))
    (setq creator (oref other creator))
    (setq is-archived (oref other is-archived))
    (setq is-mpim (oref other is-mpim))
    (setq members (oref other members))
    (setq topic (oref other topic))
    (setq purpose (oref other purpose))))

(defun slack-group-names (team &optional filter)
  (with-slots (groups) team
    (slack-room-names groups team filter)))

(defmethod slack-room-subscribedp ((room slack-group) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room team)))
      (and name
           (memq (intern name) subscribed-channels)))))

(defmethod slack-room-buffer-name ((room slack-group) team)
  (concat slack-group-buffer-name
          " : "
          (slack-room-display-name room team)))

(defun slack-group-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for groups = (oref team groups)
                         nconc groups)
                team)))
    (slack-room-display room team)))

(defun slack-group-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels ((on-list-update
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-group-list-update")
                  (slack-merge-list (oref team groups)
                                    (mapcar #'(lambda (g)
                                                (slack-room-create
                                                 g team 'slack-group))
                                            (plist-get data :groups)))
                  (if after-success
                      (funcall after-success team))
                  (mapc #'(lambda (room)
                            (slack-room-info-request room team))
                        (oref team groups))
                  (slack-log "Slack Group List Updated" team :level 'info))))
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
                    (slack-room-display-name group team)))))
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


(defun slack-group-members-s (group team)
  (with-slots (members) group
    (mapconcat #'(lambda (user)
                   (slack-user-name user
                                    team))
               members ", ")))

(defun slack-group-mpim-open ()
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-names team)))
    (cl-labels
        ((select-users (users acc)
                       (let ((selected (funcall slack-completing-read-function "Select User: "
                                                users nil t)))
                         (if (< 0 (length selected))
                             (select-users users
                                           (push (cdr (cl-assoc selected users :test #'string=)) acc))
                           acc)))
         (on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-group-mpim-open"))))
      (slack-request
       (slack-request-create
        slack-mpim-open-url
        team
        :type "POST"
        :params (list (cons "users" (mapconcat (lambda (u) (plist-get u :id)) (select-users users '()) ",")))
        :success #'on-success)))))

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
                              (slack-group-members-s group team)))))))
          (slack-request
           (slack-request-create
            slack-mpim-close-url
            team
            :type "POST"
            :params (list (cons "channel" (oref selected id)))
            :success #'on-success))))))


(defmethod slack-mpim-p ((room slack-group))
  (oref room is-mpim))

(defmethod slack-room-get-info-url ((_room slack-group))
  slack-group-info-url)

(defmethod slack-room-update-info ((room slack-group) data team)
  (let ((new-room (slack-room-create (plist-get data :group)
                                     team
                                     'slack-group)))
    (slack-merge room new-room)))

(defmethod slack-room-history-url ((_room slack-group))
  slack-group-history-url)

(defmethod slack-room-replies-url ((_room slack-group))
  "https://slack.com/api/groups.replies")

(provide 'slack-group)
;;; slack-group.el ends here
