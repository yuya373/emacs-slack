;;; slack-message-delete.el --- impl for message delete  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'alert)
(require 'slack-message)
(require 'slack-team)
(require 'slack-room)
(require 'slack-thread)
(require 'slack-buffer)
(require 'slack-message-update)

(defconst slack-message-delete-url "https://slack.com/api/chat.delete")
(defvar slack-current-room-id)
(defvar slack-current-team-id)

(defcustom slack-message-custom-delete-notifier nil
  "Custom notification function for deleted message.\ntake 3 Arguments.\n(lambda (MESSAGE ROOM TEAM) ...)."
  :group 'slack)

(defun slack-message-delete ()
  (interactive)
  (unless (and (boundp 'slack-current-team-id)
               (boundp 'slack-current-room-id))
    (error "Call From Slack Room Buffer"))
  (let* ((team (slack-team-find slack-current-team-id))
         (channel (slack-room-find slack-current-room-id
                                   team))
         (ts (ignore-errors (get-text-property (point) 'ts))))
    (unless ts
      (error "Call With Cursor On Message"))
    (let ((message (slack-room-find-message channel ts)))
      (when message
        (cl-labels
            ((on-delete
              (&key data &allow-other-keys)
              (slack-request-handle-error
               (data "slack-message-delete"))))
          (if (yes-or-no-p "Are you sure you want to delete this message?")
              (slack-request
               (slack-request-create
                slack-message-delete-url
                team
                :type "POST"
                :params (list (cons "ts" (oref message ts))
                              (cons "channel" (oref channel id)))
                :success #'on-delete))
            (message "Canceled")))))))

(defclass _slack-message-delete ()
  ((room :initarg :room)
   (message :initarg :message :initform nil)
   (team :initarg :team)))

(defclass _slack-thread-message-delete (_slack-message-delete) ())

(defun slack-message-deleted (payload team)
  (let* ((room (slack-room-find (plist-get payload :channel) team))
         (message (slack-room-find-message room (plist-get payload :deleted_ts)))
         (class (and message
                     (or (and (slack-message-thread-messagep message)
                              '_slack-thread-message-delete)
                         '_slack-message-delete)))
         (delete (and class
                      (make-instance class
                                     :team team
                                     :room room
                                     :message message))))

    (when delete
      (slack-message-delete--buffer delete)
      (slack-message-delete--notify delete))))

(defmethod slack-message-delete--notify ((this _slack-message-delete))
  (with-slots (message team room) this
    (when message
      (if slack-message-custom-delete-notifier
          (funcall slack-message-custom-delete-notifier message room team)
        (alert "message deleted"
               :icon slack-alert-icon
               :title (format "\\[%s] from %s"
                              (slack-room-display-name room)
                              (slack-message-sender-name message team))
               :category 'slack)))))

(defmethod slack-message-delete--buffer ((this _slack-message-delete))
  (with-slots (message room) this
    (when message
      (slack-buffer-delete-message (slack-room-buffer-name room)
                                   (oref message ts)))))

(defmethod slack-message-delete--buffer ((this _slack-thread-message-delete))
  (with-slots (message room team) this
    (let* ((parent (and message (slack-room-find-thread-parent room message)))
           (thread (and parent (slack-message-get-thread parent team))))
      (when thread
        (slack-thread-delete-message thread message)
        (slack-buffer-delete-message
         (slack-thread-buf-name room (oref thread thread-ts))
         (oref message ts))
        (slack-message-update parent team t)))))

(provide 'slack-message-delete)
;;; slack-message-delete.el ends here
