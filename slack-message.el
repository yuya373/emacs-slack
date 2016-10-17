;;; slack-message.el --- slack-message                -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-reaction)

(defvar slack-current-room-id)
(defvar slack-current-team-id)
(defconst slack-message-pins-add-url "https://slack.com/api/pins.add")
(defconst slack-message-pins-remove-url "https://slack.com/api/pins.remove")
(defconst slack-message-delete-url "https://slack.com/api/chat.delete")

(defclass slack-message ()
  ((type :initarg :type :type string)
   (subtype :initarg :subtype)
   (channel :initarg :channel :initform nil)
   (ts :initarg :ts :type string :initform "")
   (text :initarg :text :type (or null string) :initform nil)
   (item-type :initarg :item_type)
   (attachments :initarg :attachments :type (or null list) :initform nil)
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean)
   (pinned-to :initarg :pinned_to :type (or null list))
   (edited-at :initarg :edited-at :initform nil)
   (deleted-at :initarg :deleted-at :initform nil)))

(defclass slack-file-message (slack-message)
  ((file :initarg :file)
   ;; (bot-id :initarg :bot_id :type (or null string))
   ;; (username :initarg :username)
   ;; (display-as-bot :initarg :display_as_bot)
   (upload :initarg :upload)
   (user :initarg :user :initform nil)))

(defclass slack-reply (slack-message)
  ((user :initarg :user :initform nil)
   (reply-to :initarg :reply_to :type integer)
   (id :initarg :id :type integer)))

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (edited :initarg :edited)
   (id :initarg :id)
   (inviter :initarg :inviter)))

(defclass slack-bot-message (slack-message)
  ((bot-id :initarg :bot_id :type string)
   (username :initarg :username :type string :initform "")
   (icons :initarg :icons)))

(defclass slack-attachment ()
  ((fallback :initarg :fallback :initform nil)
   (title :initarg :title :initform nil)
   (title-link :initarg :title_link :initform nil)
   (pretext :initarg :pretext :initform nil)
   (text :initarg :text :initform nil)
   (author-name :initarg :author_name :initform "")
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :type (or null list))
   (image-url :initarg :image_url)
   (thumb-url :initarg :thumb_url)
   (is-share :initarg :is_share :initform nil)))

(defclass slack-shared-message (slack-attachment)
  ((ts :initarg :ts :initform nil)
   (color :initarg :color :initform nil)
   (channel-id :initarg :channel_id :initform nil)
   (channel-name :initarg :channel_name :initform nil)
   (from-url :initarg :from_url :initform nil)))

(defgeneric slack-message-sender-name  (slack-message team))
(defgeneric slack-message-to-string (slack-message))
(defgeneric slack-message-to-alert (slack-message))

(defgeneric slack-room-buffer-name (room))

(defun slack-room-find (id team)
  (if (and id team)
      (cl-labels ((find-room (room)
                             (string= id (oref room id))))
        (cond
         ((string-prefix-p "F" id) (slack-file-room-obj team))
         ((string-prefix-p "C" id) (cl-find-if #'find-room
                                               (oref team channels)))
         ((string-prefix-p "G" id) (cl-find-if #'find-room
                                               (oref team groups)))
         ((string-prefix-p "D" id) (cl-find-if #'find-room
                                               (oref team ims)))
         ((string-prefix-p "Q" id) (cl-find-if #'find-room
                                               (oref team search-results)))))))

(defun slack-reaction-create (payload)
  (apply #'slack-reaction "reaction"
         (slack-collect-slots 'slack-reaction payload)))

(defmethod slack-message-set-reactions ((m slack-message) payload)
  (let ((reactions (plist-get payload :reactions)))
    (if (< 0 (length reactions))
        (oset m reactions (mapcar #'slack-reaction-create reactions))))
  m)

(defun slack-attachment-create (payload)
  (plist-put payload :fields
             (append (plist-get payload :fields) nil))
  (if (plist-get payload :is_share)
      (apply #'slack-shared-message "shared-attachment"
             (slack-collect-slots 'slack-shared-message payload))
    (apply #'slack-attachment "attachment"
           (slack-collect-slots 'slack-attachment payload))))

(defmethod slack-message-set-attachments ((m slack-message) payload)
  (let ((attachments (plist-get payload :attachments)))
    (if (< 0 (length attachments))
        (oset m attachments
              (mapcar #'slack-attachment-create attachments))))
  m)

(cl-defun slack-message-create (payload &key room)
  (when payload
    (plist-put payload :reactions (append (plist-get payload :reactions) nil))
    (plist-put payload :attachments (append (plist-get payload :attachments) nil))
    (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
    (if room
        (plist-put payload :channel (oref room id)))
    (cl-labels ((create
                 (m)
                 (let ((subtype (plist-get m :subtype)))
                   (cond
                    ((plist-member m :reply_to)
                     (apply #'slack-reply "reply"
                            (slack-collect-slots 'slack-reply m)))
                    ((and subtype (string-prefix-p "file" subtype))
                     (apply #'slack-file-message "file-msg"
                            (slack-collect-slots 'slack-file-message m)))
                    ((plist-member m :user)
                     (apply #'slack-user-message "user-msg"
                            (slack-collect-slots 'slack-user-message m)))
                    ((and subtype (string= "bot_message" subtype))
                     (apply #'slack-bot-message "bot-msg"
                            (slack-collect-slots 'slack-bot-message m)))))))
      (let ((message (create payload)))
        (when message
          (slack-message-set-attachments message payload)
          (slack-message-set-reactions message payload))))))

(defmethod slack-message-equal ((m slack-message) n)
  (string= (oref m ts) (oref n ts)))

(defmethod slack-message-update ((m slack-message) team &optional replace no-notify)
  (cl-labels
      ((push-message-to (room msg)
                        (with-slots (messages) room
                          (when (< 0 (length messages))
                            (cl-pushnew msg messages
                                        :test #'slack-message-equal))
                          (update-latest room msg)))
       (update-latest (room msg)
                      (with-slots (latest) room
                        (if (or (null latest)
                                (string< (oref latest ts) (oref msg ts)))
                            (setq latest msg)))))
    (with-slots (channel) m
      (let ((room (slack-room-find channel team)))
        (when room
          (push-message-to room m)
          (slack-buffer-update room m team :replace replace)
          (unless no-notify
            (slack-message-notify m room team)))))))


(defun slack-message-edited (payload team)
  (let* ((edited-message (slack-decode (plist-get payload :message)))
         (room (slack-room-find (plist-get payload :channel) team))
         (message (slack-room-find-message room
                                           (plist-get edited-message :ts)))
         (edited-info (plist-get edited-message :edited)))
    (if message
        (progn
          (with-slots (text edited-at attachments) message
            (setq text (plist-get edited-message :text))
            (setq edited-at (plist-get edited-info :ts))
            (if (plist-get edited-message :attachments)
                (setq attachments
                      (mapcar #'slack-attachment-create
                              (plist-get edited-message :attachments)))))
          (slack-message-update message team t)))))

(defmethod slack-message-sender-name ((m slack-message) team)
  (slack-user-name (oref m user) team))

(defun slack-message-pins-add ()
  (interactive)
  (slack-message-pins-request slack-message-pins-add-url))

(defun slack-message-pins-remove ()
  (interactive)
  (slack-message-pins-request slack-message-pins-remove-url))

(defun slack-message-pins-request (url)
  (unless (and (bound-and-true-p slack-current-team-id)
               (bound-and-true-p slack-current-room-id))
    (error "Call From Slack Room Buffer"))
  (let* ((team (slack-team-find slack-current-team-id))
         (room (slack-room-find slack-current-room-id
                                team))
         (word (thing-at-point 'word))
         (ts (ignore-errors (get-text-property 0 'ts word))))
    (unless ts
      (error "Call From Slack Room Buffer"))
    (cl-labels ((on-pins-add
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-message-pins-request"))))
      (slack-request
       url
       team
       :params (list (cons "channel" (oref room id))
                     (cons "timestamp" ts))
       :success #'on-pins-add
       :sync nil))))

(defun slack-message-time-stamp (message)
  (seconds-to-time (string-to-number (oref message ts))))

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
         (deleted-ts (plist-get payload :ts))
         (channel (slack-room-find channel-id team))
         (message (slack-room-find-message channel ts)))
    (when message
      (oset message deleted-at deleted-ts)
      (alert "message deleted"
             :title (format "\\[%s] from %s"
                            (slack-room-name-with-team-name channel)
                            (slack-message-sender-name message team))
             :category 'slack)
      (slack-buffer-update channel message team :replace t))))

(provide 'slack-message)
;;; slack-message.el ends here
