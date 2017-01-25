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
  ((file :initarg :file)))

(defclass slack-file-share-message (slack-file-message)
  ((upload :initarg :upload)
   (user :initarg :user :initform nil)))

(defclass slack-file-comment ()
  ((id :initarg :id :type string)
   (file-id :initarg :file_id :type string)
   (created :initarg :created)
   (timestamp :initarg :timestamp)
   (user :initarg :user)
   (is-intro :initarg :is_intro)
   (comment :initarg :comment)
   (channel :initarg :channel)
   (reactions :initarg :reactions type list)))

(defclass slack-file-comment-message (slack-file-message)
  ((comment :initarg :comment :initform nil)))

(defclass slack-file-mention-message (slack-file-message)
  ((user :initarg :user :initform nil)))

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
   (author-name :initarg :author_name :initform nil)
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :initform '())
   (image-url :initarg :image_url)
   (thumb-url :initarg :thumb_url)
   (is-share :initarg :is_share :initform nil)
   (footer :initarg :footer :initform nil)
   (color :initarg :color :initform nil)
   (ts :initarg :ts :initform nil)
   (author-subname :initarg :author_subname :initform nil)))

(defclass slack-attachment-field ()
  ((title :initarg :title :initform nil)
   (value :initarg :value :initform nil)
   (short :initarg :short :initform nil)))

(defclass slack-shared-message (slack-attachment)
  ((channel-id :initarg :channel_id :initform nil)
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

(defmethod slack-message-set-reactions ((m slack-message) reactions)
  (oset m reactions reactions)
  m)

(defmethod slack-message-set-reactions ((m slack-file-message) rs)
  (oset (oref m file) reactions rs))

(defmethod slack-message-set-reactions ((m slack-file-comment-message) reactions)
  (oset (oref m comment) reactions reactions))

(defun slack-attachment-create (payload)
  (plist-put payload :fields
             (mapcar #'(lambda (field) (apply #'slack-attachment-field
                                              (slack-collect-slots 'slack-attachment-field field)))
                     (append (plist-get payload :fields) nil)))
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

(defmethod slack-message-set-file ((m slack-message) _payload _team)
  m)

(defmethod slack-message-set-file ((m slack-file-message) payload team)
  (let ((file (slack-file-create (plist-get payload :file))))
    (oset m file file)
    (slack-file-pushnew file team)
    m))

(defmethod slack-message-set-file-comment ((m slack-message) _payload)
  m)

(defmethod slack-message-set-file-comment ((m slack-file-comment-message) payload)
  (let* ((file-id (plist-get (plist-get payload :file) :id))
         (comment (plist-get payload :comment))
         (reactions (mapcar #'slack-reaction-create
                            (plist-get comment :reactions)))
         (file-comment (slack-file-comment-create comment file-id)))
    (oset file-comment reactions reactions)
    (oset m comment file-comment)
    m))

(cl-defun slack-message-create (payload team &key room)
  (when payload
    (plist-put payload :reactions (append (plist-get payload :reactions) nil))
    (plist-put payload :attachments (append (plist-get payload :attachments) nil))
    (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
    (if room
        (plist-put payload :channel (oref room id)))
    (cl-labels
        ((create-message
          (payload)
          (let ((subtype (plist-get payload :subtype)))
            (cond
             ((plist-member payload :reply_to)
              (apply #'slack-reply "reply"
                     (slack-collect-slots 'slack-reply payload)))
             ((and subtype (string-equal "file_share" subtype))
              (apply #'slack-file-share-message "file-share"
                     (slack-collect-slots 'slack-file-share-message payload)))
             ((and subtype (string-equal "file_comment" subtype))
              (apply #'slack-file-comment-message "file-comment"
                     (slack-collect-slots 'slack-file-comment-message payload)))
             ((and subtype (string-equal "file_mention" subtype))
              (apply #'slack-file-mention-message "file-mention"
                     (slack-collect-slots 'slack-file-mention-message payload)))
             ((plist-member payload :user)
              (apply #'slack-user-message "user-msg"
                     (slack-collect-slots 'slack-user-message payload)))
             ((and subtype (string= "bot_message" subtype))
              (apply #'slack-bot-message "bot-msg"
                     (slack-collect-slots 'slack-bot-message payload)))))))

      (let ((message (create-message payload)))
        (when message
          (slack-message-set-attachments message payload)
          (oset message reactions
                (mapcar #'slack-reaction-create (plist-get payload :reactions)))
          (slack-message-set-file message payload team)
          (slack-message-set-file-comment message payload))))))

(defmethod slack-message-equal ((m slack-message) n)
  (string= (oref m ts) (oref n ts)))

(defmethod slack-message-update ((m slack-message) team &optional replace no-notify)
  (cl-labels
      ((push-message-to
        (room msg)
        (with-slots (messages) room
          (when (< 0 (length messages))
            (setq messages
                  (cl-remove-if #'(lambda (n) (slack-message-equal msg n))
                                messages))
            (push msg messages))
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
  (let* ((room (slack-room-find (plist-get payload :channel) team))
         (edited-message (slack-message-create
                          (slack-decode (plist-get payload :message))
                          team :room room))
         (message (slack-room-find-message
                   room (oref edited-message ts)))
         (edited-info (plist-get edited-message :edited)))
    (if message
        (slack-message-update edited-message team t))))

(defmethod slack-message-sender-name ((m slack-message) team)
  (slack-user-name (oref m user) team))

(defmethod slack-message-sender-name ((m slack-file-comment-message) team)
  (slack-user-name (oref (oref m comment) user) team))

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
         (ts (ignore-errors (slack-get-ts))))
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
      (lui-delete (lambda () (equal (get-text-property (point) 'ts)
                                    (oref message ts)))))))

(defmethod slack-message-get-reactions ((m slack-message))
  (oref m reactions))

(defmethod slack-message-get-reactions ((m slack-file-message))
  (oref (oref m file) reactions))

(defmethod slack-message-get-reactions ((m slack-file-comment-message))
  (oref (oref m comment) reactions))

(provide 'slack-message)
;;; slack-message.el ends here
