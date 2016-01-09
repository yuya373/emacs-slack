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
(require 'slack-group)
(require 'slack-buffer)
(require 'slack-util)

(defvar slack-token)
(defvar slack-channels)
(defvar slack-buffer-function)
(defvar slack-groups)

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

(defclass slack-channel (slack-group)
  ((is-member :initarg :is_member)
   (num-members :initarg :num_members)))

(defun slack-channel-create (payload)
  (plist-put payload :members (append (plist-get payload :members) nil))
  (apply #'slack-channel "channel"
         (slack-collect-slots 'slack-channel payload)))

(defmethod slack-room-buffer-name ((room slack-channel))
  (concat slack-channel-buffer-name " : " (slack-room-name room)))

(defmethod slack-room-buffer-header ((room slack-channel))
  (concat "Channel: " (slack-room-name room ) "\n"))

(defun slack-channel-names (&optional filter)
  (slack-room-names slack-channels filter))

(defmethod slack-room-member-p ((room slack-channel))
  (if (eq (oref room is-member) :json-false)
      nil
    t))

(defun slack-channel-select ()
  (interactive)
  (slack-room-select slack-channels))

(defun slack-channel-list-update ()
  (interactive)
  (cl-labels ((on-list-update
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-channel-list-update")
                (setq slack-channels
                      (mapcar #'slack-channel-create
                              (plist-get data :channels)))
                (message "Slack Channel List Updated"))))
    (slack-room-list-update slack-channel-list-url
                            #'on-list-update
                            :sync nil)))

(defmethod slack-room-update-mark-url ((_room slack-channel))
  slack-channel-update-mark-url)

(defun slack-create-channel ()
  (interactive)
  (cl-labels
      ((on-create-channel (&key data &allow-other-keys)
                          (slack-request-handle-error
                           (data "slack-channel-create")
                           (let ((channel (slack-channel-create
                                           (plist-get data :channel))))
                             (push channel slack-channels)
                             (message "channel: %s created!"
                                      (slack-room-name channel))))))
    (slack-create-room slack-create-channel-url
                       #'on-create-channel)))

(defun slack-channel-rename ()
  (interactive)
  (slack-room-rename slack-channel-rename-url
                     (slack-channel-names)))

(defun slack-channel-invite ()
  (interactive)
  (slack-room-invite slack-channel-invite-url
                     (slack-channel-names)))

(defun slack-channel-leave ()
  (interactive)
  (let ((channel (slack-current-room-or-select
                  (slack-channel-names
                   #'(lambda (channels)
                       (cl-remove-if-not #'slack-room-member-p
                                         channels))))))
    (cl-labels
        ((on-channel-leave (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-channel-leave")
                            (oset channel is-member :json-false))))
      (slack-room-request-with-id slack-channel-leave-url
                                  (oref channel id)
                                  #'on-channel-leave))))

(defun slack-channel-join ()
  (interactive)
  (let* ((channel (slack-current-room-or-select
                   (slack-channel-names
                    #'(lambda (channels)
                        (cl-remove-if #'(lambda (c)
                                          (or (slack-room-member-p c)
                                              (slack-room-archived-p c)))
                                      channels))))))
    (cl-labels
        ((on-channel-join (&key data &allow-other-keys)
                          (slack-request-handle-error
                           (data "slack-channel-join"))))
      (slack-request
       slack-channel-join-url
       :params (list (cons "token" slack-token)
                     (cons "name" (slack-room-name channel)))
       :sync nil
       :success #'on-channel-join))))

(defun slack-channel-create-from-info (id)
  (cl-labels
      ((on-create-from-info (&key data &allow-other-keys)
                            (slack-request-handle-error
                             (data "slack-channel-create-from-info")
                             (let ((channel (slack-channel-create
                                             (plist-get data :channel))))
                               (push channel slack-channels)
                               (message "Channel: %s created"
                                        (slack-room-name channel))))))
    (slack-channel-fetch-info id #'on-create-from-info)))

(defun slack-channel-fetch-info (id success)
  (slack-request
   slack-channel-info-url
   :sync nil
   :params (list (cons "token" slack-token)
                 (cons "channel" id))
   :success success))

(defun slack-channel-archive ()
  (interactive)
  (let ((channel (slack-current-room-or-select
                  (slack-channel-names
                   #'(lambda (channels)
                       (cl-remove-if #'slack-room-archived-p
                                     channels))))))
    (cl-labels
        ((on-channel-archive (&key data &allow-other-keys)
                             (slack-request-handle-error
                              (data "slack-channel-archive"))))
      (slack-room-request-with-id slack-channel-archive-url
                                  (oref channel id)
                                  #'on-channel-archive))))

(defun slack-channel-unarchive ()
  (interactive)
  (let ((channel (slack-current-room-or-select
                  (slack-channel-names
                   #'(lambda (channels)
                       (cl-remove-if-not #'slack-room-archived-p
                                         channels))))))
    (cl-labels
        ((on-channel-unarchive (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-channel-unarchive"))))
      (slack-room-request-with-id slack-channel-unarchive-url
                                  (oref channel id)
                                  #'on-channel-unarchive))))

(defmethod slack-room-history-url ((_room slack-channel))
  slack-channel-history-url)

(provide 'slack-channel)
;;; slack-channel.el ends here
