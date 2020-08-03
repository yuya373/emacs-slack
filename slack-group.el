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
(require 'slack-conversations)

(defconst slack-group-update-mark-url "https://slack.com/api/groups.mark")
(defconst slack-mpim-open-url "https://slack.com/api/mpim.open")

(defvar slack-buffer-function)
(defvar slack-completing-read-function)

(defclass slack-group (slack-room)
  ((name :initarg :name :type string :initform "")
   (name-normalized :initarg :name_normalized :type string :initform "")
   (num-members :initarg :num_members :initform 0)
   (creator :initarg :creator :type (or null string) :initform nil)
   (is-archived :initarg :is_archived :initform nil :type boolean)
   (is-channel :initarg :is_channel :initform nil :type boolean)
   (is-ext-shared :initarg :is_ext_shared :initform nil :type boolean)
   (is-pending-ext-shared :initarg :is_pending_ext_shared :initform nil :type boolean)
   (is-general :initarg :is_general :initform nil :type boolean)
   (is-group :initarg :is_group :initform nil :type boolean)
   (is-im :initarg :is_im :initform nil :type boolean)
   (is-member :initarg :is_member :initform t :type boolean)
   (is-mpim :initarg :is_mpim :initform nil :type boolean)
   (is-org-shared :initarg :is_org_shared :initform nil :type boolean)
   (is-private :initarg :is_private :initform nil :type boolean)
   (is-read-only :initarg :is_read_only :initform nil :type boolean)
   (is-shared :initarg :is_shared :initform nil :type boolean)
   (parent-conversation :initarg :parent_conversation :initform nil)
   (pending-shared :initarg :pending_shared :initform '() :type list)
   (previous-names :initarg :previous_names :initform '() :type list)
   (purpose :initarg :purpose :initform nil)
   (shared-team-ids :initarg :shared_team_ids :initform '() :type list)
   (topic :initarg :topic :initform nil)
   (members :initarg :members :type list :initform '())
   (members-loaded-p :type boolean :initform nil)))

(cl-defmethod slack-merge ((this slack-group) other)
  (cl-call-next-method)
  (oset this name (oref other name))
  (oset this name-normalized (oref other name-normalized))
  (oset this num-members (oref other num-members))
  (oset this creator (oref other creator))
  (oset this is-archived (oref other is-archived))
  (oset this is-channel (oref other is-channel))
  (oset this is-ext-shared (oref other is-ext-shared))
  (oset this is-pending-ext-shared (oref other is-pending-ext-shared))
  (oset this is-general (oref other is-general))
  (oset this is-group (oref other is-group))
  (oset this is-im (oref other is-im))
  (oset this is-member (oref other is-member))
  (oset this is-mpim (oref other is-mpim))
  (oset this is-org-shared (oref other is-org-shared))
  (oset this is-private (oref other is-private))
  (oset this is-read-only (oref other is-read-only))
  (oset this is-shared (oref other is-shared))
  (oset this parent-conversation (oref other parent-conversation))
  (oset this pending-shared (oref other pending-shared))
  (oset this previous-names (oref other previous-names))
  (oset this purpose (oref other purpose))
  (oset this shared-team-ids (oref other shared-team-ids))
  (oset this topic (oref other topic)))

(defun slack-group-names (team &optional filter)
  (slack-room-names (slack-team-groups team) team filter))

(cl-defmethod slack-room-subscribedp ((room slack-group) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room team)))
      (and name
           (memq (intern name) subscribed-channels)))))

(defun slack-group-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (_channels groups _ims)
                  (slack-team-set-groups team groups)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (slack-log "Slack Group List Updated"
                             team :level 'info)))
      (slack-conversations-list team #'success (list "private_channel" "mpim")))))


(defun slack-create-group ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-conversations-create team "true")))

(cl-defmethod slack-room-archived-p ((room slack-group))
  (oref room is-archived))

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
                           (slack-select-multiple #'prompt users))))
      (slack-conversations-open team
                                :user-ids (user-ids)))))

(cl-defmethod slack-mpim-p ((room slack-group))
  (oref room is-mpim))

(cl-defmethod slack-room--has-unread-p ((this slack-group) counts)
  (if (slack-mpim-p this)
      (slack-counts-mpim-unread-p counts this))
  (slack-counts-channel-unread-p counts this))

(cl-defmethod slack-room-mention-count ((this slack-group) team)
  (with-slots (counts) team
    (if counts
        (if (slack-mpim-p this)
            (slack-counts-mpim-mention-count counts this)
          (slack-counts-channel-mention-count counts this))
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-group) count team)
  (slack-if-let* ((counts (oref team counts)))
      (if (slack-mpim-p this)
          (slack-counts-mpim-set-mention-count counts this count)
        (slack-counts-channel-set-mention-count counts this count))))

(cl-defmethod slack-room-set-has-unreads ((this slack-group) value team)
  (slack-if-let* ((counts (oref team counts)))
      (if (slack-mpim-p this)
          (slack-counts-mpim-set-has-unreads counts this value)
        (slack-counts-channel-set-has-unreads counts this value))))

(cl-defmethod slack-room--update-latest ((this slack-group) counts ts)
  (if (slack-mpim-p this)
      (slack-counts-mpim-update-latest counts this ts)
    (slack-counts-channel-update-latest counts this ts)))

(cl-defmethod slack-room--latest ((this slack-group) counts)
  (if (slack-mpim-p this)
      (slack-counts-mpim-latest counts this)
    (slack-counts-channel-latest counts this)))

(cl-defmethod slack-room-members ((this slack-group))
  (oref this members))

(cl-defmethod slack-room-set-members ((this slack-group) members)
  (oset this members
        (cl-remove-duplicates (append (oref this members) members)
                              :test #'string=)))

(cl-defmethod slack-room-members-loaded-p ((this slack-group))
  (oref this members-loaded-p))

(cl-defmethod slack-room-members-loaded ((this slack-group))
  (oset this members-loaded-p t))

(cl-defmethod slack-room-hidden-p ((this slack-group))
  (not (slack-room-member-p this)))

(cl-defmethod slack-room-member-p ((room slack-group))
  (oref room is-member))

(provide 'slack-group)
;;; slack-group.el ends here
