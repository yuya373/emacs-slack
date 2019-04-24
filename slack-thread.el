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
(defconst slack-subscriptions-thread-add-url "https://slack.com/api/subscriptions.thread.add")
(defconst slack-subscriptions-thread-remove-url "https://slack.com/api/subscriptions.thread.remove")
(defconst slack-subscriptions-thread-get-url
  "https://slack.com/api/subscriptions.thread.get")
(defconst slack-thread-mark-url
  "https://slack.com/api/subscriptions.thread.mark")

(defcustom slack-thread-also-send-to-room 'ask
  "Whether a thread message should also be sent to its room.
If nil: don't send to the room.
If `ask': ask the user every time.
Any other non-nil value: send to the room."
  :type '(choice (const :tag "Never send message to the room." nil)
                 (const :tag "Ask the user every time." ask)
                 (const :tag "Always send message to the room." t))
  :group 'slack)

(cl-defmethod slack-thread-messagep ((m slack-message))
  (if (and (oref m thread-ts) (not (slack-message-thread-parentp m)))
      t
    nil))

(cl-defmethod slack-thread-replies ((this slack-message) room team &key after-success (cursor nil) (oldest nil))
  (let* ((ts (slack-thread-ts this))
         (oldest (or oldest ts)))
    (cl-labels ((success (messages next-cursor has-more)
                         (slack-room-set-messages room messages team)
                         (when (functionp after-success)
                           (funcall after-success next-cursor has-more))))
      (slack-conversations-replies room ts team
                                   :after-success #'success
                                   :cursor cursor
                                   :oldest oldest))))

;; TODO: format is [[profile-image...], N replies, Last reply n (hours|days) ago]
(cl-defmethod slack-thread-to-string ((m slack-message) team)
  (if (slack-message-thread-parentp m)
    (let* ((usernames (mapconcat #'identity
                                 (cl-remove-duplicates
                                  (mapcar #'(lambda (reply)
                                              (slack-user-name
                                               (plist-get reply :user)
                                               team))
                                          (oref m replies))
                                  :test #'string=)
                                 " "))
           (text (format "%s reply from %s"
                         (oref m reply-count)
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

(cl-defmethod slack-thread-mark ((this slack-message) room ts team)
  (let* ((channel (oref room id))
         (thread-ts (or (oref this thread-ts)
                        ;; initial thread reply
                        (slack-ts this)))
         (params (list (cons "channel" channel)
                       (cons "thread_ts" thread-ts)
                       (cons "ts" ts))))
    (slack-subscriptions-thread-get
     room
     thread-ts
     team
     #'(lambda (subscriptions)
         (if (cl-find-if #'(lambda (subscription)
                             (or (string= subscription ts)
                                 (string< subscription ts)))
                         subscriptions)
             (cl-labels
                 ((success (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-thread-mark"))))
               (slack-request
                (slack-request-create
                 slack-thread-mark-url
                 team
                 :type "POST"
                 :params params
                 :success #'success))))))))

(provide 'slack-thread)
;;; slack-thread.el ends here
