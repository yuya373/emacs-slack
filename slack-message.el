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
(require 'subr-x)
(require 'slack-util)
(require 'slack-reaction)
(require 'slack-request)
;; (require 'slack-attachment)
(declare-function slack-attachment-create "slack-attachment")
(require 'slack-team)
;; (require 'slack-file)
(declare-function slack-file-create "slack-file")
;; (require 'slack-thread)
(declare-function slack-thread-create "slack-thread")

(defvar slack-current-buffer)

(defcustom slack-message-custom-delete-notifier nil
  "Custom notification function for deleted message.\ntake 3 Arguments.\n(lambda (MESSAGE ROOM TEAM) ...)."
  :type 'function
  :group 'slack)

(defconst slack-message-pins-add-url "https://slack.com/api/pins.add")
(defconst slack-message-pins-remove-url "https://slack.com/api/pins.remove")
(defconst slack-message-stars-add-url "https://slack.com/api/stars.add")
(defconst slack-message-stars-remove-url "https://slack.com/api/stars.remove")

(defclass slack-message ()
  ((type :initarg :type :type string)
   (subtype :initarg :subtype)
   (channel :initarg :channel :initform nil)
   (ts :initarg :ts :type string :initform "")
   (text :initarg :text :type (or null string) :initform nil)
   (item-type :initarg :item_type)
   (attachments :initarg :attachments :type (or null list) :initform nil)
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean :initform nil)
   (pinned-to :initarg :pinned_to :type (or null list))
   (deleted-at :initarg :deleted-at :initform nil)
   (thread :initarg :thread :initform nil)
   (thread-ts :initarg :thread_ts :initform nil)
   (hide :initarg :hide :initform nil)
   (files :initarg :files :initform '())
   (edited :initarg :edited :initform nil)
   (is-ephemeral :initarg :is_ephemeral :initform nil)))

(defclass slack-message-edited ()
  ((user :initarg :user :type string)
   (ts :initarg :ts :type string)))

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (id :initarg :id)
   (inviter :initarg :inviter)))

(defclass slack-reply-broadcast-message (slack-user-message)
  ((broadcast-thread-ts :initarg :broadcast_thread_ts :initform nil)))

(defclass slack-bot-message (slack-message)
  ((bot-id :initarg :bot_id :type string)
   (username :initarg :username :type string :initform "")
   (icons :initarg :icons)))

(defclass slack-file-comment-message (slack-message)
  ((file :initarg :file :initform nil)
   (comment :initarg :comment :initform nil)))

(cl-defmethod slack-message-sender-name ((m slack-file-comment-message) team)
  (with-slots (comment) m
    (slack-user-name (plist-get comment :user) team)))

(cl-defmethod slack-message-sender-id ((m slack-file-comment-message))
  (with-slots (comment) m
    (plist-get comment :user)))

(cl-defgeneric slack-message-sender-name  (slack-message team))
(cl-defgeneric slack-message-to-string (slack-message))
(cl-defgeneric slack-message-to-alert (slack-message))
(cl-defmethod slack-message-bot-id ((_this slack-message)) nil)

(cl-defgeneric slack-room-buffer-name (room team))

(defun slack-reaction-create (payload)
  (apply #'slack-reaction "reaction"
         (slack-collect-slots 'slack-reaction payload)))

(cl-defmethod slack-message-set-attachments ((m slack-message) payload)
  (let ((attachments (append (plist-get payload :attachments) nil)))
    (if (< 0 (length attachments))
        (oset m attachments
              (mapcar #'slack-attachment-create attachments))))
  m)

(cl-defmethod slack-message-set-file ((m slack-message) payload)
  (let ((files (mapcar #'(lambda (file) (slack-file-create file))
                       (plist-get payload :files))))
    (oset m files files)
    m))

(cl-defmethod slack-message-set-thread ((m slack-message) payload)
  (when (slack-message-thread-parentp m)
    (oset m thread (slack-thread-create m payload))))

(defun slack-reply-broadcast-message-create (payload)
  (let ((parent (cl-first (plist-get payload :attachments))))
    (plist-put payload :broadcast_thread_ts (plist-get parent :ts))
    (apply #'slack-reply-broadcast-message "reply-broadcast"
           (slack-collect-slots 'slack-reply-broadcast-message payload))))

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
              (apply #'make-instance 'slack-reply
                     (slack-collect-slots 'slack-reply payload)))
             ((or (and subtype (or (string-equal "reply_broadcast" subtype)
                                   (string= "thread_broadcast" subtype)))
                  (plist-get payload :reply_broadcast)
                  (plist-get payload :is_thread_broadcast))
              (slack-reply-broadcast-message-create payload))
             ((and (plist-member payload :user) (plist-get payload :user))
              (apply #'slack-user-message "user-msg"
                     (slack-collect-slots 'slack-user-message payload)))
             ((and subtype (string= "bot_message" subtype))
              (apply #'slack-bot-message "bot-msg"
                     (slack-collect-slots 'slack-bot-message payload)))
             ((and subtype (string= "file_comment" subtype))
              (apply #'slack-file-comment-message "file_comment"
                     (slack-collect-slots 'slack-file-comment-message payload)))
             (t (progn
                  (slack-log (format "Unknown Message Type: %s" payload)
                             team :level 'warn)
                  (apply #'slack-message "unknown message"
                         (slack-collect-slots 'slack-message payload))))))))

      (let ((message (create-message payload)))
        (when message
          (slack-message-set-edited message payload)
          (slack-message-set-attachments message payload)
          (oset message reactions
                (mapcar #'slack-reaction-create (plist-get payload :reactions)))
          (slack-message-set-file message payload)
          (slack-message-set-thread message payload)
          message)))))

(cl-defmethod slack-message-set-edited ((this slack-message) payload)
  (if (plist-get payload :edited)
      (oset this edited (apply #'make-instance 'slack-message-edited
                               (slack-collect-slots 'slack-message-edited
                                                    (plist-get payload :edited))))))

(cl-defmethod slack-message-edited-at ((this slack-message))
  (with-slots (edited) this
    (when edited
      (oref edited ts))))

(cl-defmethod slack-message-equal ((m slack-message) n)
  (string= (slack-ts m) (slack-ts n)))

(cl-defmethod slack-message-get-thread ((parent slack-message))
  (let ((thread (oref parent thread)))
    (unless thread
      (oset parent thread (slack-thread-create parent)))
    (oref parent thread)))

(cl-defmethod slack-message-sender-name ((m slack-message) team)
  (slack-user-name (oref m user) team))

(cl-defmethod slack-message-sender-id ((m slack-message))
  (oref m user))

(defun slack-message-pins-request (url room team ts)
  (cl-labels ((on-pins-add
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-pins-request"))))
    (slack-request
     (slack-request-create
      url
      team
      :params (list (cons "channel" (oref room id))
                    (cons "timestamp" ts))
      :success #'on-pins-add
      ))))

(cl-defmethod slack-ts ((this slack-message))
  (oref this ts))

(defun slack-ts-to-time (ts)
  (seconds-to-time (string-to-number ts)))

(defun slack-message-time-stamp (message)
  (slack-ts-to-time (slack-ts message)))

(cl-defmethod slack-user-find ((m slack-message) team)
  (slack-user--find (slack-message-sender-id m) team))

(cl-defmethod slack-message-star-added ((m slack-message))
  (oset m is-starred t))

(cl-defmethod slack-message-star-removed ((m slack-message))
  (oset m is-starred nil))

(defun slack-message-star-api-request (url params team)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data url))))
    (slack-request
     (slack-request-create
      url
      team
      :params params
      :success #'on-success))))

(cl-defmethod slack-message-star-api-params ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-reaction-delete ((this slack-message) reaction)
  (with-slots (reactions) this
    (setq reactions (slack-reaction--delete reactions reaction))))

(cl-defmethod slack-reaction-push ((this slack-message) reaction)
  (push reaction (oref this reactions)))

(cl-defmethod slack-reaction-find ((m slack-message) reaction)
  (slack-reaction--find (oref m reactions) reaction))

(cl-defmethod slack-message-reactions ((this slack-message))
  (oref this reactions))

(cl-defmethod slack-message-get-param-for-reaction ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-message-append-reaction ((m slack-message) reaction &optional _type _file-id)
  (slack-if-let* ((old-reaction (slack-reaction-find m reaction)))
      (slack-reaction-join old-reaction reaction)
    (slack-reaction-push m reaction)))

(cl-defmethod slack-message-pop-reaction ((m slack-message) reaction &optional _type _file-id)
  (slack-message--pop-reaction m reaction))

(defun slack-message--pop-reaction (message reaction)
  (let* ((old-reaction (slack-reaction-find message reaction))
         (decl-p (< 1 (oref old-reaction count))))
    (if decl-p
        (with-slots (count users) old-reaction
          (cl-decf count)
          (setq users (cl-remove-if
                       #'(lambda (old-user)
                           (cl-find old-user
                                    (oref reaction users)
                                    :test #'string=))
                       users)))
      (slack-reaction-delete message reaction))))

(cl-defmethod slack-message-get-text ((m slack-message))
  (oref m text))

(cl-defmethod slack-thread-message-p ((this slack-message))
  (and (oref this thread-ts)
       (not (string= (slack-ts this) (oref this thread-ts)))))

(cl-defmethod slack-thread-message-p ((_this slack-reply-broadcast-message))
  nil)

(cl-defmethod slack-message-thread-parentp ((m slack-message))
  (let* ((thread (oref m thread))
         (thread-ts (or (and thread (oref thread thread-ts))
                        (oref m thread-ts))))
    (and thread-ts (string= (slack-ts m) thread-ts))))

(cl-defmethod slack-message--inspect ((this slack-file-comment-message) _room _team)
  (let ((super (cl-call-next-method)))
    (with-slots (file comment) this
      (format "%s\nFILE:%s\nCOMMENT:%s"
              super
              file comment))))

(cl-defmethod slack-message-thread ((this slack-message) _room)
  (oref this thread))

(cl-defmethod slack-message-pinned-to-room-p ((this slack-message) room)
  (cl-find (oref room id)
           (oref this pinned-to)
           :test #'string=))

(provide 'slack-message)
;;; slack-message.el ends here
