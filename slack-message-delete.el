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
  (if (eq major-mode 'slack-file-info-mode)
      (slack-file-comment-delete)
    (if-let* ((buf slack-current-buffer))
        (slack-buffer-delete-message buf (slack-get-ts)))))

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
  (with-slots (message room team) this
    (and message
         (if-let* ((buf (slack-buffer-find 'slack-message-buffer room team)))
             (slack-buffer-message-delete buf (oref message ts))))))

(defmethod slack-message-delete--buffer ((this _slack-thread-message-delete))
  (with-slots (message room team) this
    (when message
      (if-let* ((parent (slack-room-find-thread-parent room message))
                (thread (slack-message-get-thread parent team))
                (ts (oref message ts))
                (buf (slack-buffer-find 'slack-thread-message-buffer
                                        room
                                        (oref thread thread-ts)
                                        team)))
          (progn
            (slack-thread-delete-message thread message)
            (slack-buffer-message-delete buf ts)
            (slack-message-update parent team t))))))

(provide 'slack-message-delete)
;;; slack-message-delete.el ends here
