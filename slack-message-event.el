;;; slack-message-event.el ---                       -*- lexical-binding: t; -*-

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

(require 'eieio)
(require 'slack-util)
(require 'slack-event)
(require 'slack-create-message)
(require 'slack-message-buffer)

(defvar slack-alert-icon)

(defclass slack-message-event (slack-event slack-message-event-processable)
  ((subtype :initarg :subtype :type string)))

(defun slack-create-message-event (payload)
  (let* ((type (plist-get payload :type))
         (subtype (or (plist-get payload :subtype) ""))
         (class (cond ((string= "message_changed" subtype)
                       'slack-message-changed-event)
                      ((string= "message_deleted" subtype)
                       'slack-message-deleted-event)
                      ((string= "message_replied" subtype)
                       'slack-message-replied-event)
                      ((string= "thread_broadcast" subtype)
                       'slack-thread-broadcast-message-event)
                      ((string= "bot_message" subtype)
                       'slack-bot-message-event)
                      (t 'slack-message-event))))
    (make-instance class
                   :type type
                   :subtype subtype
                   :payload payload)))

(defclass slack-bot-message-event (slack-message-event) ())
(defclass slack-thread-broadcast-message-event (slack-message-event) ())
(defclass slack-message-changed-event (slack-message-event) ())
(defclass slack-message-deleted-event (slack-message-event) ())
(defclass slack-message-replied-event (slack-message-event) ())

(cl-defgeneric slack-message-event-update-modeline (event message team))
(cl-defgeneric slack-message-event-retrieve-users (event message team callback))

(cl-defmethod slack-event-find-message ((this slack-message-event) team)
  (slack-message-create (oref this payload) team))

(cl-defmethod slack-event-find-message ((this slack-message-changed-event) team)
  (with-slots (payload) this
    (slack-message-create (plist-get payload :message)
                          team
                          (plist-get payload :channel))))

(cl-defmethod slack-event-find-message ((this slack-message-deleted-event) team)
  (let* ((payload (oref this payload))
         (channel (plist-get payload :channel))
         (room (slack-room-find channel team))
         (ts (plist-get payload :deleted_ts)))
    (when room
      (slack-room-find-message room ts))))

(cl-defmethod slack-event-find-message ((this slack-message-replied-event) team)
  (let* ((payload (oref this payload))
         (message-payload (plist-get payload :message))
         (thread-ts (plist-get message-payload :thread_ts))
         (channel (plist-get payload :channel))
         (room (slack-room-find channel team)))
    (when room
      (slack-room-find-message room thread-ts))))

(cl-defmethod slack-event-save-message ((this slack-message-changed-event) message team)
  (with-slots (payload) this
    (slack-if-let* ((room (slack-room-find message team))
                    (old (slack-room-find-message room (slack-ts message))))
        (progn
          (oset message reactions (oref old reactions))
          (slack-room-push-message room message team)))))

(cl-defmethod slack-event-save-message ((_this slack-message-deleted-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-room-delete-message room (slack-ts message))))

(cl-defmethod slack-event-save-message ((this slack-message-replied-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (let ((payload (plist-get (oref this payload) :message)))
        (oset message thread-ts (plist-get payload :thread_ts))
        (oset message reply-count (plist-get payload :reply_count))
        (oset message reply-users-count (plist-get payload :reply_users_count))
        (oset message latest-reply (plist-get payload :latest_reply))
        (oset message reply-users (plist-get payload :reply_users))
        (slack-room-push-message room message team))))

(cl-defmethod slack-event-update-buffer ((_this slack-message-event) message team)
  (slack-message-update-buffer message team))

(cl-defmethod slack-event-update-buffer ((_this slack-message-changed-event) message team)
  (slack-message-replace-buffer message team))

(cl-defmethod slack-event-update-buffer ((_this slack-message-deleted-event) message team)
  (when (slack-thread-message-p message)
    (slack-if-let* ((room (slack-room-find message team))
                    (parent (slack-room-find-message room (slack-thread-ts message)))
                    (thread-ts (slack-thread-ts parent))
                    (buffer (slack-buffer-find 'slack-thread-message-buffer team room thread-ts)))
        (slack-buffer-message-delete buffer (slack-ts message))))
  (when (slack-message-visible-p message team)
    (slack-if-let* ((room (slack-room-find message team))
                    (buf (slack-buffer-find 'slack-message-buffer team room)))
        (slack-buffer-message-delete buf (slack-ts message)))))

(cl-defmethod slack-event-update-buffer ((_this slack-message-replied-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-room-update-buffer room team message t)))

(cl-defmethod slack-event-notify ((_this slack-message-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-message-notify message room team)))

(cl-defmethod slack-event-notify ((_this slack-message-changed-event) _message _team))

(cl-defmethod slack-event-notify ((_this slack-message-deleted-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (if slack-message-custom-delete-notifier
          (funcall slack-message-custom-delete-notifier message room team)
        (alert "message deleted"
               :icon slack-alert-icon
               :title (format "\\[%s] from %s"
                              (slack-room-display-name room team)
                              (slack-message-sender-name message team))
               :category 'slack))))

(cl-defmethod slack-event-notify ((_this slack-message-replied-event) _message _team))

(cl-defmethod slack-message-event-update-modeline ((_this slack-message-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (progn
        (when (and (not (slack-message-ephemeral-p message))
                   (slack-message-visible-p message team))
          (slack-room-set-has-unreads room t team)

          (when (or (slack-message-mentioned-p message team)
                    (slack-im-p room)
                    (slack-mpim-p room))
            (let* ((count (slack-room-mention-count room team))
                   (next-count (+ count 1)))
              (slack-room-set-mention-count room next-count team))))
        (slack-update-modeline))))

(cl-defmethod slack-message-event-update-modeline ((_this slack-message-changed-event) _message _team)
  (slack-update-modeline))
(cl-defmethod slack-message-event-update-modeline ((_this slack-message-deleted-event) _message _team)
  (slack-update-modeline))

(cl-defmethod slack-event-update-ui ((this slack-message-event) message team)
  (cl-call-next-method)
  (slack-message-event-update-modeline this message team))

(cl-defmethod slack-message-event-retrieve-users ((this slack-bot-message-event) _message team cb)
  (let* ((payload (oref this payload))
         (bot-id (plist-get payload :bot_id)))
    (if (slack-find-bot bot-id team)
        (funcall cb)
      (slack-bot-info-request bot-id team cb))))

(cl-defmethod slack-message-event-retrieve-users ((_this slack-message-event) message team cb)
  (let* ((user-ids (slack-message-user-ids message))
         (missing-user-ids (slack-team-missing-user-ids team user-ids)))
    (if (< 0 (length missing-user-ids))
        (slack-user-info-request missing-user-ids
                                 team
                                 :after-success cb)
      (funcall cb))))

(cl-defmethod slack-event-update ((this slack-message-event) team)
  (let* ((message (slack-event-find-message this team)))
    (when message
      (slack-event-save-message this message team)
      (cl-labels
          ((update () (slack-event-update-ui this message team)))
        (slack-message-event-retrieve-users this message team #'update)))))

(provide 'slack-message-event)
;;; slack-message-event.el ends here
