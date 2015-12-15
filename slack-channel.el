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

(defmethod slack-room-history ((room slack-channel))
  (cl-labels ((on-channel-update (&key data &allow-other-keys)
                               (slack-room-on-history data room)))
    (with-slots (id) room
      (slack-room-request-update id
                                 slack-channel-history-url
                                 #'on-channel-update))))

(defun slack-channel-names ()
  (mapcar (lambda (channel)
            (cons (oref channel name) channel))
          slack-channels))

(defun slack-channel-select ()
  (interactive)
  (slack-room-select slack-channels))

(defun slack-channel-list-update ()
  (interactive)
  (cl-labels ((on-list-update
               (&key data &allow-other-keys)
               (unless (plist-get data :ok)
                 (error "%s" data))
               (setq slack-channels
                     (mapcar #'slack-channel-create
                             (plist-get data :channels)))))
    (slack-room-list-update slack-channel-list-url
                            #'on-list-update
                            :sync nil)))

(defconst slack-channel-update-mark-url "https://slack.com/api/channels.mark")

(defmethod slack-room-update-mark-url ((_room slack-channel))
  slack-channel-update-mark-url)

(provide 'slack-channel)
;;; slack-channel.el ends here
