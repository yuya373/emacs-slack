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
(require 'slack-block)

(defvar slack-current-buffer)
(defvar slack-message-user-regexp "<@\\([WU].*?\\)\\(|.*?\\)?>")

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
   (attachments :initarg :attachments :type (or null list) :initform nil)
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean :initform nil)
   (pinned-to :initarg :pinned_to :type (or null list))
   (deleted-at :initarg :deleted-at :initform nil)
   (hidden :initarg :hidden :initform nil)
   (files :initarg :files :initform '())
   (edited :initarg :edited :initform nil)
   (is-ephemeral :initarg :is_ephemeral :initform nil)
   (blocks :initarg :blocks :type (or null list) :initform nil)
   ;; thread
   (thread-ts :initarg :thread_ts :initform nil)
   (latest-reply :initarg :latest_reply :initform "" :type string)
   (last-read :initarg :last_read :initform "" :type string)
   (replies :initarg :replies :initform '() :type list)
   (reply-count :initarg :reply_count :initform 0 :type number)
   (reply-users :initarg :reply_users :initform '() :type list)
   (reply-users-count :initarg :reply_users_count :initform 0 :type number)
   (subscribed :initarg :subscribed :initform nil :type boolean)
   ))

(defclass slack-message-edited ()
  ((user :initarg :user :type string)
   (ts :initarg :ts :type string)))

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

(defun slack-reply-broadcast-message-create (payload)
  (let ((parent (cl-first (plist-get payload :attachments))))
    (plist-put payload :broadcast_thread_ts (plist-get parent :ts))
    (apply #'slack-reply-broadcast-message "reply-broadcast"
           (slack-collect-slots 'slack-reply-broadcast-message payload))))

(defun slack-room-or-children-p (room)
  (when (and room
             (eieio-object-p room))
    (cl-case (eieio-object-class-name room)
      (slack-room t)
      (slack-im t)
      (slack-group t)
      (slack-channel t)
      (t nil))))

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
             ((and (plist-member payload :user) (plist-get payload :user))
              (apply #'slack-user-message "user-msg"
                     (slack-collect-slots 'slack-user-message payload)))
             ((or (and subtype (string= "bot_message" subtype))
                  (and (plist-member payload :bot_id) (plist-get payload :bot_id)))
              (apply #'slack-bot-message "bot-msg"
                     (slack-collect-slots 'slack-bot-message payload)))
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

(defun slack-message-set-blocks (message payload)
  (oset message blocks (mapcar #'slack-create-layout-block
                               (plist-get payload :blocks))))

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

(cl-defmethod slack-message-sender-name ((m slack-message) team)
  (slack-user-name (oref m user) team))

(cl-defmethod slack-message-sender-id ((m slack-message))
  "")

(cl-defmethod slack-ts ((ts string))
  ts)

(cl-defmethod slack-ts ((this slack-message))
  (oref this ts))

(defun slack-ts-to-time (ts)
  (seconds-to-time (string-to-number ts)))

(defun slack-message-time-stamp (message)
  (slack-ts-to-time (slack-ts message)))

(cl-defmethod slack-user-find ((m slack-message) team)
  nil)

(cl-defmethod slack-message-star-added ((m slack-message))
  (oset m is-starred t))

(cl-defmethod slack-message-star-removed ((m slack-message))
  (oset m is-starred nil))

(cl-defmethod slack-message-star-api-params ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-reaction-delete ((this slack-message) reaction)
  (with-slots (reactions) this
    (setq reactions (slack-reaction-delete reaction reactions))))

(cl-defmethod slack-reaction-push ((this slack-message) reaction)
  (push reaction (oref this reactions)))

(cl-defmethod slack-reaction-find ((m slack-message) reaction)
  (slack-reaction--find (oref m reactions) reaction))

(cl-defmethod slack-message-reactions ((this slack-message))
  (oref this reactions))

(cl-defmethod slack-message-get-param-for-reaction ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-message-get-text ((m slack-message) team)
  (or (mapconcat #'identity
                 (cl-remove-if #'(lambda (block-message)
                                   (< (length block-message) 1))
                               (mapcar #'(lambda (bl)
                                           (slack-block-to-mrkdwn bl (list :team team)))
                                       (oref m blocks)))
                 "\n\n")
      (slack-message-unescape-string (oref m text) team)))

(cl-defmethod slack-thread-message-p ((this slack-message))
  (and (oref this thread-ts)
       (not (string= (slack-ts this) (oref this thread-ts)))))

(cl-defmethod slack-message-thread-parentp ((m slack-message))
  (let* ((thread-ts (slack-thread-ts m)))
    (when thread-ts
      (string= (slack-ts m) thread-ts))))

(cl-defmethod slack-message-pinned-to-room-p ((this slack-message) room)
  (cl-find (oref room id)
           (oref this pinned-to)
           :test #'string=))

(cl-defmethod slack-message-user-ids ((this slack-message))
  (let ((result (append (oref this reply-users) nil))
        (sender-id (slack-message-sender-id this)))
    (unless (slack-string-blankp sender-id)
      (push sender-id result))
    (with-slots (text) this
      (when text
        (let ((start 0))
          (while (and (< start (length text))
                      (string-match slack-message-user-regexp
                                    text
                                    start))
            (let ((user-id (match-string 1 text)))
              (when user-id
                (push user-id result)))
            (setq start (match-end 0))))))
    result))

(cl-defmethod slack-message-visible-p ((this slack-message) team)
  (if (slack-team-visible-threads-p team)
      t
    (or (not (slack-thread-message-p this))
        (slack-reply-broadcast-message-p this))))

(cl-defmethod slack-thread-ts ((this slack-message))
  (oref this thread-ts))

(cl-defmethod slack-message-handle-thread-subscribed ((this slack-message) payload)
  (oset this subscribed t)
  (oset this last-read (plist-get payload :last_read)))

(cl-defmethod slack-message-ephemeral-p ((this slack-message))
  (oref this is-ephemeral))

(cl-defmethod slack-message-subscribed-thread-message-p ((this slack-message) room)
  (and (slack-thread-messagep this)
       (slack-if-let* ((parent (slack-room-find-thread-parent room this)))
           (oref parent subscribed))))

(cl-defmethod slack-message-profile-image ((m slack-message) team)
  nil)

(cl-defmethod slack-message-user-status ((this slack-message) team)
  "")

(cl-defmethod slack-message-header ((this slack-message) team)
  (let* ((deleted-at (oref this deleted-at))
         (name (slack-message-sender-name this team))
         (status (slack-message-user-status this team))
         (time (slack-message-time-to-string (oref this ts)))
         (edited-at (slack-message-time-to-string (slack-message-edited-at this)))
         (deleted-at (slack-message-time-to-string (oref this deleted-at))))
    (concat (slack-if-let* ((image (slack-message-profile-image this team)))
                (concat (propertize "image" 'display image 'face 'slack-profile-image-face)
                        " ")
              "")
            (slack-message-put-header-property (concat name
                                                       (if (slack-string-blankp status)
                                                           ""
                                                         (concat " " status))
                                                       (if deleted-at
                                                           (concat " deleted_at: " deleted-at)
                                                         "")
                                                       (if edited-at
                                                           (concat " edited_at: " edited-at)
                                                         "")))
            (if (slack-message-starred-p this)
                " :star:"
              ""))))

(cl-defmethod slack-message-starred-p ((m slack-message))
  (oref m is-starred))

(cl-defmethod slack-message-display-thread-sign-p ((this slack-message) team)
  (and (slack-team-visible-threads-p team)
       (not (null (oref this thread-ts)))
       (not (string= (oref this thread-ts) (slack-ts this)))
       (not (eq major-mode 'slack-thread-message-buffer-mode))))

(cl-defmethod slack-message-body ((m slack-message) team)
  (let ((use-blocks-p (and (not (oref team disable-block-format))
                           (oref m blocks))))
    (if use-blocks-p
        (mapconcat #'(lambda (bl)
                       (slack-block-to-string bl (list :team team)))
                   (oref m blocks)
                   "\n\n")
      (if (oref m text)
          (propertize (slack-message-unescape-string (oref m text) team)
                      'face 'slack-message-output-text
                      'slack-text-type 'mrkdwn)
        ""))))

(provide 'slack-message)
;;; slack-message.el ends here
