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
               slack-message-delete-url
               team
               :type "POST"
               :params (list (cons "ts" (oref message ts))
                             (cons "channel" (oref channel id)))
               :success #'on-delete
               :sync nil)
            (message "Canceled")))))))


(defun slack-message-deleted (payload team)
  (let* ((channel-id (plist-get payload :channel))
         (ts (plist-get payload :deleted_ts))
         (room (slack-room-find channel-id team))
         (buf-name (slack-room-buffer-name room))
         (buf (get-buffer buf-name))
         (thread-ts (plist-get (plist-get payload :previous_message) :thread_ts))
         (_delete (make-instance '_slack-message-delete
                                 :team team
                                 :room room
                                 :message (slack-room-find-message room ts)
                                 :parent (slack-room-find-message room thread-ts))))

    (slack-thread-delete-message _delete)
    (slack-buffer--delete-message _delete)
    (slack-notify-delete _delete)))

(defclass _slack-message-delete ()
  ((room :initarg :room)
   (message :initarg :message :initform nil)
   (team :initarg :team)
   (parent :initarg :parent :initform nil)))

(defmethod slack-notify-delete ((this _slack-message-delete))
  (with-slots (message team room) this
    (when message
      (alert "message deleted"
             :title (format "\\[%s] from %s"
                            (slack-room-buffer-name room)
                            (slack-message-sender-name message team))
             :category 'slack))))

(defmethod slack-buffer--delete-message ((this _slack-message-delete))
  (with-slots (message room) this
    (when message
      (slack-buffer-delete-message (slack-room-buffer-name room)
                                   (oref message ts)))))

(defmethod slack-thread-delete-message ((this _slack-message-delete))
  (with-slots (parent (deleted-message message) room team) this
    (when parent
      (with-slots (thread) parent
        (with-slots (messages) thread
          (setq messages (cl-remove-if #'(lambda (e) (string= (oref e ts) (oref deleted-message ts)))
                                       messages)))
        (let* ((buf (get-buffer (slack-thread-buf-name room (oref thread thread-ts)))))
          (slack-buffer-delete-message buf (oref deleted-message ts))))
      (slack-message-update parent team t))))

(provide 'slack-message-delete)
;;; slack-message-delete.el ends here
