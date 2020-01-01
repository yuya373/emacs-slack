;;; slack-channel.el --- slack channel implement      -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-request)
(require 'slack-conversations)

(defvar slack-buffer-function)
(defvar slack-completing-read-function)

(defconst slack-channel-update-mark-url "https://slack.com/api/channels.mark")

(defclass slack-channel (slack-group)
  ((is-member :initarg :is_member :initform nil :type boolean)))


(defun slack-channel-names (team &optional filter)
  (slack-room-names (slack-team-channels team) team filter))

(defun slack-channel-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (channels _groups _ims)
                  (slack-team-set-channels team channels)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (slack-log "Slack Channel List Updated"
                             team :level 'info)))
      (slack-conversations-list team #'success (list "public_channel")))))

(cl-defmethod slack-room-update-mark-url ((_room slack-channel))
  slack-channel-update-mark-url)

(defun slack-create-channel ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-conversations-create team "false")))

(cl-defmethod slack-room-subscribedp ((room slack-channel) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room team)))
      (and name
           (memq (intern name) subscribed-channels)))))

(cl-defmethod slack-room-hidden-p ((room slack-channel))
  (slack-room-archived-p room))

(cl-defmethod slack-room-member-p ((this slack-channel))
  (oref this is-member))

(provide 'slack-channel)
;;; slack-channel.el ends here
