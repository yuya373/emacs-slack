;;; slack-channel.el ---slack channel implement      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
(require 'slack-group)
(require 'slack-buffer)
(require 'slack-util)
(require 'slack-request)

(defvar slack-buffer-function)

(defconst slack-channel-history-url "https://slack.com/api/channels.history")
(defconst slack-channel-list-url "https://slack.com/api/channels.list")
(defconst slack-channel-buffer-name "*Slack - Channel*")
(defconst slack-channel-update-mark-url "https://slack.com/api/channels.mark")
(defconst slack-create-channel-url "https://slack.com/api/channels.create")
(defconst slack-channel-rename-url "https://slack.com/api/channels.rename")
(defconst slack-channel-invite-url "https://slack.com/api/channels.invite")
(defconst slack-channel-leave-url "https://slack.com/api/channels.leave")
(defconst slack-channel-join-url "https://slack.com/api/channels.join")
(defconst slack-channel-info-url "https://slack.com/api/channels.info")
(defconst slack-channel-archive-url "https://slack.com/api/channels.archive")
(defconst slack-channel-unarchive-url "https://slack.com/api/channels.unarchive")
(defconst slack-channel-info-url "https://slack.com/api/channels.info")

(defclass slack-channel (slack-group)
  ((is-member :initarg :is_member :initform nil)
   (num-members :initarg :num_members :initform 0)))

(defmethod slack-merge ((this slack-channel) other)
  (call-next-method)
  (with-slots (is-member num-members) this
    (setq is-member (oref other is-member))
    (setq num-members (oref other num-members))))

(defmethod slack-room-buffer-name ((room slack-channel))
  (concat slack-channel-buffer-name
          " : "
          (slack-room-display-name room)))

(defun slack-channel-names (team &optional filter)
  (with-slots (channels) team
    (slack-room-names channels filter)))

(defmethod slack-room-member-p ((room slack-channel))
  (oref room is-member))

(defun slack-channel-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for channels = (oref team channels)
                         nconc channels))))
    (slack-room-display room team)))

(defun slack-channel-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels ((on-list-update
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-channel-list-update")
                  (slack-merge-list (oref team channels)
                                    (mapcar #'(lambda (d)
                                                (slack-room-create d
                                                                   team
                                                                   'slack-channel))
                                            (plist-get data :channels)))

                  (if after-success
                      (funcall after-success team))
                  (mapc #'(lambda (room)
                            (slack-request-worker-push
                             (slack-room-create-info-request room team)))
                        (oref team channels))
                  (slack-log "Slack Channel List Updated" team :level 'info))))
      (slack-room-list-update slack-channel-list-url
                              #'on-list-update
                              team
                              :sync nil))))

(defmethod slack-room-update-mark-url ((_room slack-channel))
  slack-channel-update-mark-url)

(defun slack-create-channel ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels
        ((on-create-channel (&key data &allow-other-keys)
                            (slack-request-handle-error
                             (data "slack-channel-create"))))
      (slack-create-room slack-create-channel-url
                         team
                         #'on-create-channel))))

(defun slack-channel-rename ()
  (interactive)
  (slack-room-rename slack-channel-rename-url
                     #'slack-channel-names))

(defun slack-channel-invite ()
  (interactive)
  (slack-room-invite slack-channel-invite-url
                     #'slack-channel-names))

(defun slack-channel-leave (&optional team select)
  (interactive)
  (let* ((team (or team (slack-team-select)))
         (channel (slack-current-room-or-select
                   #'(lambda ()
                       (slack-channel-names
                        team
                        #'(lambda (channels)
                            (cl-remove-if-not #'slack-room-member-p
                                              channels))))
                   select)))
    (slack-channel-request-leave channel team)))

(defun slack-channel-request-leave (channel team)
  (cl-labels
      ((on-channel-leave (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-channel-leave")
                          (oset channel is-member nil)
                          (message "Left Channel: %s"
                                   (slack-room-name channel)))))
    (slack-room-request-with-id slack-channel-leave-url
                                (oref channel id)
                                team
                                #'on-channel-leave)))

(defun slack-channel-join (&optional team select)
  (interactive)
  (cl-labels
      ((filter-channel (channels)
                       (cl-remove-if
                        #'(lambda (c)
                            (or (slack-room-member-p c)
                                (slack-room-archived-p c)))
                        channels)))
    (let* ((team (or team (slack-team-select)))
           (channel (slack-current-room-or-select
                     #'(lambda ()
                         (slack-channel-names team
                                              #'filter-channel))
                     select)))
      (slack-channel-request-join channel team))))

(defun slack-channel-request-join (channel team)
  (cl-labels
      ((on-channel-join (&key data &allow-other-keys)
                        (slack-request-handle-error
                         (data "slack-channel-join"))))
    (slack-request
     (slack-request-create
      slack-channel-join-url
      team
      :params (list (cons "name" (slack-room-name channel)))
      :success #'on-channel-join))))

(defun slack-channel-create-from-info (id team)
  (cl-labels
      ((on-create-from-info
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-channel-create-from-info")
         (let* ((c-data (plist-get data :channel)))
           (if (plist-get c-data :is_channel)
               (let ((channel (slack-room-create c-data team 'slack-channel)))
                 (with-slots (channels) team (push channel channels))
                 (message "Channel: %s created"
                          (slack-room-display-name channel))))))))
    (slack-channel-fetch-info id team #'on-create-from-info)))

(defun slack-channel-fetch-info (id team success)
  (slack-request
   (slack-request-create
    slack-channel-info-url
    team
    :params (list (cons "channel" id))
    :success success)))

(defun slack-channel-archive ()
  (interactive)
  (let* ((team (slack-team-select))
         (channel (slack-current-room-or-select
                   #'(lambda ()
                       (slack-channel-names
                        team
                        #'(lambda (channels)
                            (cl-remove-if #'slack-room-archived-p
                                          channels)))))))
    (cl-labels
        ((on-channel-archive (&key data &allow-other-keys)
                             (slack-request-handle-error
                              (data "slack-channel-archive"))))
      (slack-room-request-with-id slack-channel-archive-url
                                  (oref channel id)
                                  team
                                  #'on-channel-archive))))

(defun slack-channel-unarchive ()
  (interactive)
  (let* ((team (slack-team-select))
         (channel (slack-current-room-or-select
                   #'(lambda ()
                       (slack-channel-names
                        team
                        #'(lambda (channels)
                            (cl-remove-if-not #'slack-room-archived-p
                                              channels)))))))
    (cl-labels
        ((on-channel-unarchive (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-channel-unarchive"))))
      (slack-room-request-with-id slack-channel-unarchive-url
                                  (oref channel id)
                                  team
                                  #'on-channel-unarchive))))

(defmethod slack-room-subscribedp ((room slack-channel) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room)))
      (and name
           (memq (intern name) subscribed-channels)))))

(defmethod slack-room-get-info-url ((_room slack-channel))
  slack-channel-info-url)

(defmethod slack-room-update-info ((room slack-channel) data team)
  (let ((new-room (slack-room-create (plist-get data :channel)
                                     team
                                     'slack-channel)))

    (slack-merge room new-room)))

(defmethod slack-room-history-url ((_room slack-channel))
  slack-channel-history-url)

(defmethod slack-room-replies-url ((_room slack-channel))
  "https://slack.com/api/channels.replies")

(defmethod slack-room-hidden-p ((room slack-channel))
  (slack-room-archived-p room))

(defmethod slack-room-member-p ((this slack-channel))
  (oref this is-member))

(provide 'slack-channel)
;;; slack-channel.el ends here
