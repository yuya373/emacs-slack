;;; slack-create-message.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
(require 'slack-reaction)
(require 'slack-message)
(require 'slack-file)
(require 'slack-user-message)
(require 'slack-bot-message)
(require 'slack-attachment)

(defun slack-reaction-create (payload)
  (apply #'slack-reaction "reaction"
         (slack-collect-slots 'slack-reaction payload)))

(defun slack-reply-broadcast-message-create (payload)
  (apply #'slack-reply-broadcast-message "reply-broadcast"
         (slack-collect-slots 'slack-reply-broadcast-message payload)))

(defun slack-room-or-children-p (room)
  (when (and room
             (eieio-object-p room))
    (cl-case (eieio-object-class-name room)
      (slack-room t)
      (slack-im t)
      (slack-group t)
      (slack-channel t)
      (t nil))))

(cl-defmethod slack-message-set-file ((m slack-message) payload)
  (let ((files (mapcar #'(lambda (file) (slack-file-create file))
                       (plist-get payload :files))))
    (oset m files files)
    m))

(cl-defmethod slack-message-set-attachments ((m slack-message) payload)
  (let ((attachments (append (plist-get payload :attachments) nil)))
    (if (< 0 (length attachments))
        (oset m attachments
              (mapcar #'slack-attachment-create attachments))))
  m)

(defun slack-message-set-blocks (message payload)
  (oset message blocks (mapcar #'slack-create-layout-block
                               (plist-get payload :blocks))))

(cl-defmethod slack-message-set-edited ((this slack-message) payload)
  (if (plist-get payload :edited)
      (oset this edited (apply #'make-instance 'slack-message-edited
                               (slack-collect-slots 'slack-message-edited
                                                    (plist-get payload :edited))))))

(defun slack-message-create (payload team &optional room)
  (when payload
    (plist-put payload :reactions (append (plist-get payload :reactions) nil))
    (plist-put payload :attachments (append (plist-get payload :attachments) nil))
    (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
    (when (and (not (plist-member payload :channel))
               (not (slack-room-or-children-p room))
               (not (stringp room)))
      (slack-log (format "`slack-room' child or channel required. ROOM: %S"
                         room)
                 team :level 'error))
    (when (slack-room-or-children-p room)
      (plist-put payload :channel (oref room id)))
    (when (stringp room)
      (plist-put payload :channel room))
    (cl-labels
        ((create-message
          (payload)
          (let ((subtype (plist-get payload :subtype)))
            (cond
             ((plist-member payload :reply_to)
              (apply #'make-instance 'slack-reply
                     (slack-collect-slots 'slack-reply payload)))
             ((or (and subtype (or (string-equal "reply_broadcast" subtype)
                                   (string= "thread_broadcast" subtype)))
                  (plist-get payload :reply_broadcast)
                  (plist-get payload :is_thread_broadcast))
              (slack-reply-broadcast-message-create payload))
             ((or (and subtype (string= "bot_message" subtype))
                  (and (plist-member payload :bot_id)
                       (plist-get payload :bot_id)))
              (apply #'slack-bot-message "bot-msg"
                     (slack-collect-slots 'slack-bot-message payload)))
             ((and (plist-member payload :user) (plist-get payload :user))
              (apply #'slack-user-message "user-msg"
                     (slack-collect-slots 'slack-user-message payload)))
             (t (progn
                  (slack-log (format "Unknown Message Type: %s" payload)
                             team :level 'debug)
                  (apply #'slack-message "unknown message"
                         (slack-collect-slots 'slack-message payload))))))))

      (let ((message (create-message payload)))
        (when message
          (slack-message-set-edited message payload)
          (slack-message-set-attachments message payload)
          (oset message reactions
                (mapcar #'slack-reaction-create (plist-get payload :reactions)))
          (slack-message-set-file message payload)
          (slack-message-set-blocks message payload)
          message)))))

(provide 'slack-create-message)
;;; slack-create-message.el ends here
