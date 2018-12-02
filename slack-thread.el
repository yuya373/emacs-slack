;;; slack-thread.el ---                              -*- lexical-binding: t; -*-

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
(require 'lui)
(require 'slack-util)
(require 'slack-room)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-message)
(require 'slack-request)
(require 'slack-conversations)

(defvar slack-message-thread-status-keymap)
(defconst slack-subscriptions-thread-get-view-url "https://slack.com/api/subscriptions.thread.getView")
;; TODO max_ts: 1542880668.4351
(defconst slack-subscriptions-thread-clear-all-url "https://slack.com/api/subscriptions.thread.clearAll")
(defconst slack-subscriptions-thread-add-url "https://slack.com/api/subscriptions.thread.add")
(defconst slack-subscriptions-thread-remove-url "https://slack.com/api/subscriptions.thread.remove")
(defconst slack-subscriptions-thread-get-url
  "https://slack.com/api/subscriptions.thread.get")
(defconst thread-mark-url "https://slack.com/api/subscriptions.thread.mark")

(defcustom slack-thread-also-send-to-room 'ask
  "Whether a thread message should also be sent to its room.
If nil: don't send to the room.
If `ask': ask the user every time.
Any other non-nil value: send to the room."
  :type '(choice (const :tag "Never send message to the room." nil)
                 (const :tag "Ask the user every time." ask)
                 (const :tag "Always send message to the room." t))
  :group 'slack)

(defclass slack-thread ()
  ((thread-ts :initarg :thread_ts :initform "")
   (messages :initarg :messages :initform '())
   (has-unreads :initarg :has_unreads :initform nil)
   (mention-count :initarg :mention_count :initform 0)
   (reply-count :initarg :reply_count :initform 0)
   (replies :initarg :replies :initform '())
   (active :initarg :active :initform t)
   (root :initarg :root :type slack-message)
   (unread-count :initarg :unread_count :initform 0)
   (last-read :initarg :last_read :initform "0")))

(cl-defmethod slack-thread-messagep ((m slack-message))
  (if (and (oref m thread-ts) (not (slack-message-thread-parentp m)))
      t
    nil))

(cl-defmethod slack-thread-replies ((thread slack-thread) room team &key after-success (cursor nil) (oldest nil))
  (let* ((ts (oref thread thread-ts))
         (oldest (or oldest ts)))
    (cl-labels ((success (messages next-cursor has-more)
                         (when cursor
                           (setq messages
                                 (append (oref thread messages) messages)))
                         (oset thread messages
                               (slack-room-sort-messages
                                (cl-remove-if #'slack-message-thread-parentp
                                              messages)))
                         (when (functionp after-success)
                           (funcall after-success next-cursor has-more))))
      (slack-conversations-replies room ts team
                                   :after-success #'success
                                   :cursor cursor
                                   :oldest oldest))))

(cl-defmethod slack-thread-to-string ((m slack-message) team)
  (slack-if-let* ((thread (oref m thread)))
      (let* ((usernames (mapconcat #'identity
                                   (cl-remove-duplicates
                                    (mapcar #'(lambda (reply)
                                                (slack-user-name
                                                 (plist-get reply :user)
                                                 team))
                                            (oref thread replies))
                                    :test #'string=)
                                   " "))
             (text (format "%s reply from %s"
                           (oref thread reply-count)
                           usernames)))
        (propertize text
                    'face '(:underline t)
                    'keymap slack-message-thread-status-keymap))
    ""))

(cl-defmethod slack-thread-create ((m slack-message) &optional payload)
  (if payload
      (let ((replies (plist-get payload :replies))
            (reply-count (plist-get payload :reply_count))
            (unread-count (plist-get payload :unread_count))
            (last-read (plist-get payload :last_read)))
        (make-instance 'slack-thread
                       :thread_ts (slack-ts m)
                       :root m
                       :replies replies
                       :reply_count (or reply-count 0)
                       :unread_count (or unread-count 1)
                       :last_read last-read))
    (make-instance 'slack-thread
                   :thread_ts (slack-ts m)
                   :root m)))

(cl-defmethod slack-merge ((old slack-thread) new)
  (oset old replies (oref new replies))
  (oset old reply-count (oref new reply-count))
  (oset old unread-count (oref new unread-count)))

(cl-defmethod slack-thread-equal ((thread slack-thread) other)
  (and (string-equal (oref thread thread-ts)
                     (oref other thread-ts))
       (string-equal (oref (oref thread root) channel)
                     (oref (oref other root) channel))))

(cl-defmethod slack-thread-delete-message ((thread slack-thread) message)
  (with-slots (messages reply-count) thread
    (setq messages (cl-remove-if #'(lambda (e)
                                     (string= (slack-ts e)
                                              (slack-ts message)))
                                 messages))
    (setq reply-count (length messages))))

(cl-defmethod slack-thread-marked ((thread slack-thread) payload)
  (let ((unread-count (plist-get payload :unread_count))
        (last-read (plist-get payload :last_read)))
    (oset thread unread-count unread-count)
    (oset thread last-read last-read)))

(defun slack-subscriptions-thread-get-view (team &optional current-ts after-success)
  (let ((current-ts (or current-ts
                        (substring
                         (number-to-string (time-to-seconds (current-time)))
                         0 15))))
    (cl-labels
        ((create-thread (payload)
                        (slack-if-let*
                            ((root-msg (plist-get payload :root_msg))
                             (room-id (plist-get root-msg :channel))
                             (room (slack-room-find room-id team))
                             (ts (plist-get root-msg :ts))
                             (message (slack-message-create root-msg team))
                             (latest-replies (plist-get payload :latest_replies))
                             (messages (mapcar #'(lambda (e)
                                                   (slack-message-create e
                                                                         team
                                                                         :room room))
                                               latest-replies))
                             (thread (slack-message-thread message room)))

                            (progn
                              (oset thread messages messages)
                              thread)))
         (success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-subscriptions-thread-get-view")
                   (let ((total-unread-replies (plist-get data :total_unread_replies))
                         (new-threads-count (plist-get data :new_threads_count))
                         (threads (plist-get data :threads))
                         (has-more (plist-get data :has_more)))
                     (when (functionp after-success)
                       (funcall after-success
                                total-unread-replies
                                new-threads-count
                                (mapcar #'create-thread threads)
                                has-more))))))
      (slack-request
       (slack-request-create
        slack-subscriptions-thread-get-view-url
        team
        :type "POST"
        :params (list (cons "current_ts" current-ts))
        :success #'success)))))

(defun slack-subscriptions-thread-clear-all (team)
  (let ((current-ts (substring
                     (number-to-string (time-to-seconds (current-time)))
                     0 15)))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-subscriptions-thread-clear-all"))))
      (slack-request
       (slack-request-create
        slack-subscriptions-thread-clear-all-url
        team
        :type "POST"
        :params (list (cons "current_ts" current-ts))
        :success #'success)))))

(defun slack-subscriptions-thread-add (room ts team &optional after-success)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-subscriptions-thread-add")
                 (when (functionp after-success)
                   (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-subscriptions-thread-add-url
      team
      :type "POST"
      :params (list (cons "thread_ts" ts)
                    (cons "last_read" ts)
                    (cons "channel" (oref room id)))
      :success #'success))))

(defun slack-subscriptions-thread-remove (room ts team &optional after-success)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-subscriptions-thread-remove")
                 (when (functionp after-success)
                   (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-subscriptions-thread-remove-url
      team
      :type "POST"
      :params (list (cons "thread_ts" ts)
                    (cons "last_read" ts)
                    (cons "channel" (oref room id)))
      :success #'success))))

(defun slack-subscriptions-thread-get (room ts team &optional after-success handle-error)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-subscriptions-thread-get" handle-error)
                 (when (functionp after-success)
                   (let ((subscriptions (plist-get data :subscriptions)))
                     (funcall after-success subscriptions))))))
    (slack-request
     (slack-request-create
      slack-subscriptions-thread-get-url
      team
      :type "POST"
      :params (list (cons "thread_ts" ts)
                    (cons "channel" (oref room id)))
      :success #'success))))

(provide 'slack-thread)
;;; slack-thread.el ends here
