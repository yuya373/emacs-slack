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
(require 'slack-message-formatter)

(defvar slack-message-thread-status-keymap)
;; (defconst all-threads-url "https://slack.com/api/subscriptions.thread.getView")
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

(defmethod slack-thread-messagep ((m slack-message))
  (if (and (oref m thread-ts) (not (slack-message-thread-parentp m)))
      t
    nil))

(cl-defmethod slack-thread-request-messages ((thread slack-thread) room team &key after-success)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-thread-request-messages")
                    (let ((messages (mapcar #'(lambda (payload)
                                                (slack-message-create payload
                                                                      team
                                                                      :room room))
                                            (plist-get data :messages))))
                      (oset thread messages
                            (slack-room-sort-messages
                             (cl-remove-if #'slack-message-thread-parentp
                                           messages)))))
                   (if after-success
                       (funcall after-success))))

    (slack-request
     (slack-request-create
      (slack-room-replies-url room)
      team
      :params (list (cons "thread_ts" (oref thread thread-ts))
                    (cons "channel" (oref room id)))
      :success #'on-success))))

(defmethod slack-thread-to-string ((m slack-message) team)
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

(defmethod slack-thread-create ((m slack-message) &optional payload)
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

(defmethod slack-merge ((old slack-thread) new)
  (oset old replies (oref new replies))
  (oset old reply-count (oref new reply-count))
  (oset old unread-count (oref new unread-count)))

(defmethod slack-thread-equal ((thread slack-thread) other)
  (and (string-equal (oref thread thread-ts)
                     (oref other thread-ts))
       (string-equal (oref (oref thread root) channel)
                     (oref (oref other root) channel))))

;; (cl-defun slack-thread-get-all (&key (sync nil) (ts nil))
;;   (let ((team (slack-team-select)))
;;     (cl-labels
;;         ((on-success (&key data &allow-other-keys)
;;                      (slack-request-handle-error
;;                       (data "slack-thread-get-all")
;;                       (let ((threads-data (append (plist-get data :threads) nil))
;;                             (total-unread (plist-get data :total_unread_replies))
;;                             (more (if (eq :json-false (plist-get data :has_more)) nil t))
;;                             (new-count (plist-get data :new_threads_count)))
;;                         (with-slots (threads) team
;;                           (with-slots
;;                               (initializedp total-unread-replies new-threads-count has-more) threads
;;                             (setq has-more more)
;;                             (setq initializedp t)
;;                             (setq total-unread-replies total-unread)
;;                             (setq new-threads-count new-count)
;;                             (let ((parents (cl-loop for thread in threads-data
;;                                                     collect (slack-message-create
;;                                                              (plist-get thread :root_msg) team))))
;;                               (mapc #'(lambda (parent) (slack-message-update parent team nil t))
;;                                     parents))))))))
;;       (slack-request
;;        (slack-request-create
;;         all-threads-url
;;         team
;;         :type "POST"
;;         :params (list (cons "limit" "10")
;;                       (cons "current_ts" (or ts (format-time-string "%s"))))
;;         :success #'on-success
;;         :sync sync)))))

(defmethod slack-thread-title ((thread slack-thread) team)
  (with-slots (root) thread
    (let ((room (slack-room-find (oref root channel) team))
          (body (slack-message-body root team)))
      (when room
        (format "%s - %s" (slack-room-name room team)
                (concat (substring body 0 (min 50 (length body))) "..."))))))

;; (defun slack-thread-select (&optional reload)
;;   (interactive)
;;   (cl-labels
;;       ((load-threads (threads)
;;                      (slack-thread-get-all :sync t
;;                                            :ts (cl-first
;;                                                 (cl-sort
;;                                                  (mapcar #'(lambda (thread) (oref thread thread-ts)) threads)
;;                                                  #'string<))))
;;        (select-thread (threads team has-more)
;;                       (let* ((alist (cl-remove-if-not
;;                                      #'(lambda (cons) (car cons))
;;                                      (mapcar #'(lambda (thread)
;;                                                  (let ((title (slack-thread-title thread team)))
;;                                                    (and title (cons title thread))))
;;                                              threads)))
;;                              (maybe-has-more (if has-more
;;                                                  (append alist (list (cons "(load more)" 'load-more))) alist))
;;                              (selected (slack-select-from-list (maybe-has-more "Select Thread: "))))
;;                         selected))
;;        (collect-thread-parents (messages)
;;                                (mapcar #'(lambda (m) (oref m thread))
;;                                        (cl-remove-if #'(lambda (m) (not (slack-message-thread-parentp m)))
;;                                                      messages)))
;;        (collect-threads (team)
;;                         (cl-loop for room in (with-slots (groups ims channels) team
;;                                                (append ims groups channels))
;;                                  append (collect-thread-parents (oref room messages)))))

;;     (let* ((team (slack-team-select)))

;;       (with-slots (initializedp has-more) (oref team threads)
;;         (if (or (not initializedp) has-more) (load-threads (collect-threads team))))

;;       (let ((selected (select-thread (collect-threads team) team nil)))
;;         (if (eq selected 'load-more)
;;             (slack-thread-select t)
;;           (slack-thread-show-messages selected
;;                                       (slack-room-find (oref (oref selected root) channel) team)
;;                                       team))))))

(defmethod slack-thread-delete-message ((thread slack-thread) message)
  (with-slots (messages reply-count) thread
    (setq messages (cl-remove-if #'(lambda (e)
                                     (string= (slack-ts e)
                                              (slack-ts message)))
                                 messages))
    (setq reply-count (length messages))))

(defmethod slack-thread-update-mark ((thread slack-thread) room msg team)
  (with-slots (thread-ts) thread
    (with-slots (id) room
      (with-slots (ts) msg
        (cl-labels
            ((on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-thread-mark"))))

          (slack-request
           (slack-request-create
            thread-mark-url
            team
            :params (list (cons "channel" id)
                          (cons "thread_ts" thread-ts)
                          (cons "ts" ts))
            :success #'on-success)))))))

(defmethod slack-thread-marked ((thread slack-thread) payload)
  (let ((unread-count (plist-get payload :unread_count))
        (last-read (plist-get payload :last_read)))
    (oset thread unread-count unread-count)
    (oset thread last-read last-read)))

(defmethod slack-thread-update-last-read ((thread slack-thread) msg)
  (with-slots (ts) msg
    (oset thread last-read ts)))

(provide 'slack-thread)
;;; slack-thread.el ends here
