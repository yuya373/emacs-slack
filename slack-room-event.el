;;; slack-room-event.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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
(require 'slack-util)
(require 'slack-event)
(require 'slack-conversations)

(defclass slack-room-event (slack-event slack-room-event-processable) ())

(cl-defmethod slack-event-find-room ((this slack-room-event) team)
  (let* ((payload (oref this payload))
         (id (plist-get payload :channel)))
    (slack-room-find id team)))

(defclass slack-channel-created-event (slack-room-event) ())

(defun slack-create-channel-created-event (payload)
  (make-instance 'slack-channel-created-event
                 :payload payload))

(cl-defmethod slack-event-find-room ((this slack-channel-created-event) _team)
  (with-slots (payload) this
    (slack-room-create (plist-get payload :channel) 'slack-channel)))

(cl-defmethod slack-event-save-room ((_this slack-channel-created-event) room team)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-notify ((_this slack-channel-created-event) room team)
  (slack-conversations-info room
                            team
                            #'(lambda ()
                                (slack-log (format "Created channel %s"
                                                   (slack-room-display-name room team))
                                           team :level 'info))))

(defclass slack-room-archive-event (slack-room-event) ())
(defclass slack-channel-archive-event (slack-room-archive-event) ())
(defclass slack-group-archive-event (slack-room-archive-event) ())

(defun slack-create-room-archive-event (payload)
  (let ((type (plist-get payload :type)))
    (cond ((string= "channel_archive" type)
           (make-instance 'slack-channel-archive-event
                          :payload payload))
          ((string= "group_archive" type)
           (make-instance 'slack-group-archive-event
                          :payload payload)))))

(cl-defmethod slack-event-save-room ((_this slack-channel-archive-event) room team)
  (oset room is-archived t)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((_this slack-group-archive-event) room team)
  (oset room is-archived t)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-notify ((_this slack-room-archive-event) room team)
  (slack-log (format "Channel: %s is archived"
                     (slack-room-name room team))
             team :level 'info))

(defclass slack-room-unarchive-event (slack-room-event) ())
(defclass slack-channel-unarchive-event (slack-room-unarchive-event) ())
(defclass slack-group-unarchive-event (slack-room-unarchive-event) ())

(defun slack-create-room-unarchive-event (payload)
  (let ((type (plist-get payload :type)))
    (cond ((string= "channel_unarchive" type)
           (make-instance 'slack-channel-unarchive-event
                          :payload payload))
          ((string= "group_unarchive" type)
           (make-instance 'slack-group-unarchive-event
                          :payload payload)))))

(cl-defmethod slack-event-save-room ((_this slack-channel-unarchive-event) room team)
  (oset room is-archived nil)
  (slack-team-set-channels team (list room)))

(cl-defmethod slack-event-save-room ((_this slack-group-unarchive-event) room team)
  (oset room is-archived nil)
  (slack-team-set-groups team (list room)))

(cl-defmethod slack-event-notify ((_this slack-room-unarchive-event) room team)
  (slack-log (format "Channel: %s is unarchived"
                     (slack-room-name room team))
             team :level 'info))

(provide 'slack-room-event)
;;; slack-room-event.el ends here
