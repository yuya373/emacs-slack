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
(require 'slack-request)
(require 'slack-buffer)
(require 'slack-conversations)

(defconst slack--group-open-url "https://slack.com/api/groups.open")
(defconst slack-group-buffer-name "*Slack - Private Group*")
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
(defvar slack-completing-read-function)

(defclass slack-group (slack-room)
  ((is-group :initarg :is_group :initform nil)
   (creator :initarg :creator :initform "")
   (is-archived :initarg :is_archived :initform nil)
   (is-mpim :initarg :is_mpim :initform nil)
   (topic :initarg :topic :initform nil)
   (purpose :initarg :purpose :initform nil)))

(cl-defmethod slack-merge ((this slack-group) other)
  (cl-call-next-method)
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

(cl-defmethod slack-room-subscribedp ((room slack-group) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room team)))
      (and name
           (memq (intern name) subscribed-channels)))))

(cl-defmethod slack-room-buffer-name ((room slack-group) team)
  (concat slack-group-buffer-name
          " : "
          (slack-room-display-name room team)))

(defun slack-group-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (_channels groups _ims)
                  (slack-merge-list (oref team groups)
                                    groups)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (mapc #'(lambda (room)
                            (slack-request-worker-push
                             (slack-conversations-info-request room team)))
                        (oref team groups))
                  (slack-log "Slack Group List Updated"
                             team :level 'info)))
      (slack-conversations-list team #'success (list "private_channel" "mpim")))))


(cl-defmethod slack-room-update-mark-url ((_room slack-group))
  slack-group-update-mark-url)

(defun slack-create-group ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-conversations-create team "true")))

(defun slack-group-rename ()
  (interactive)
  (slack-room-rename slack-group-rename-url
                     #'slack-group-names))

(defun slack-group-invite ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-current-room-or-select
                (slack-group-names team
                                   #'(lambda (rooms)
                                       (cl-remove-if #'slack-room-archived-p
                                                     rooms))))))
    (slack-conversations-invite room team)))

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

(cl-defmethod slack-room-archived-p ((room slack-group))
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
    (slack-conversations-archive group team)))

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
    (slack-conversations-unarchive group team)))


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
        ((prompt (loop-count)
                 (if (< 0 loop-count)
                     "Select another user (or leave empty): "
                   "Select user: "))
         (user-ids ()
                   (mapcar #'(lambda (user) (plist-get user :id))
                           (slack-select-multiple #'prompt users)))
         (on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-group-mpim-open"))))
      (slack-request
       (slack-request-create
        slack-mpim-open-url
        team
        :type "POST"
        :params (list (cons "users" (mapconcat #'identity (user-ids) ",")))
        :success #'on-success)))))

(defun slack-group-mpim-close ()
  "Close mpim."
  (interactive)
  (let* ((team (slack-team-select))
         (mpim (slack-current-room-or-select
                #'(lambda ()
                    (slack-group-names
                     team
                     #'(lambda (groups)
                         (cl-remove-if-not #'slack-mpim-p
                                           groups)))))))
    (cl-labels
        ((success (_data)
                  (oset team groups
                        (cl-remove-if #'(lambda (e)
                                          (slack-equalp e mpim))
                                      (oref team groups)))
                  (slack-log (format "%s closed"
                                     (slack-room-name mpim team))
                             team :level 'info)))
      (slack-conversations-close mpim team #'success))))


(cl-defmethod slack-mpim-p ((room slack-group))
  (oref room is-mpim))

(cl-defmethod slack-room-replies-url ((_room slack-group))
  "https://slack.com/api/groups.replies")

(provide 'slack-group)
;;; slack-group.el ends here
