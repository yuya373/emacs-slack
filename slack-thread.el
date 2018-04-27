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

(defvar lui-prompt-string "> ")
(defconst all-threads-url "https://slack.com/api/subscriptions.thread.getView")
(defconst thread-mark-url "https://slack.com/api/subscriptions.thread.mark")

(defcustom slack-thread-also-send-to-room 'ask
  "Whether a thread message should also be sent to its room.
If nil: don't send to the room.
If `ask': ask the user every time.
Any other non-nil value: send to the room."
  :type '(choice (const :tag "Never send message to the room." nil)
                 (const :tag "Ask the user every time." ask)
                 (const :tag "Always send message to the room." t)))

(define-derived-mode slack-thread-mode slack-mode "Slack - Thread"
  ""
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-thread-message--send))

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

(defun slack-thread-start ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-start-thread buf (slack-get-ts))))

(defun slack-thread-message--send (message)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-send-message buf message)))

(defun slack-thread-send-message (room team message thread-ts)
  (let ((message (slack-message-prepare-links
                  (slack-escape-message message)
                  team))
        (broadcast (if (eq slack-thread-also-send-to-room 'ask)
                       (y-or-n-p (format "Also send to %s ? "
                                         (slack-room-name room)))
                     slack-thread-also-send-to-room)))
    (progn
      (slack-message-inc-id team)
      (with-slots (message-id sent-message self-id) team
        (let* ((payload (list :id message-id
                              :channel (oref room id)
                              :reply_broadcast broadcast
                              :thread_ts thread-ts
                              :type "message"
                              :user self-id
                              :text message))
               (json (json-encode payload))
               (obj (slack-message-create payload team :room room)))
          (slack-ws-send json team)
          (puthash message-id obj sent-message))))))

(defun slack-thread-show-or-create ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (if (slack-thread-message-buffer-p buf)
          (error "Already in thread")
        (slack-buffer-display-thread buf (slack-get-ts)))))

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

(defmethod slack-thread-show-messages ((thread slack-thread) room team)
  (cl-labels
      ((after-success ()
                      (let ((buf (slack-create-thread-message-buffer
                                  room team (oref thread thread-ts))))
                        (slack-buffer-display buf))))
    (slack-thread-request-messages thread room team
                                   :after-success #'after-success)))

(defmethod slack-thread-to-string ((m slack-message) team)
  (with-slots (thread) m
    (if thread
        (let* ((usernames (mapconcat #'identity
                                     (cl-remove-duplicates
                                      (mapcar #'(lambda (reply)
                                                  (slack-user-name
                                                   (plist-get reply :user)
                                                   team))
                                              (oref thread replies))
                                      :test #'string=)
                                     " "))
               (text (format "\n%s reply from %s"
                             (oref thread reply-count)
                             usernames)))
          (propertize text
                      'face '(:underline t)
                      'keymap (let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1] #'slack-thread-show-or-create)
                                (define-key map (kbd "RET") #'slack-thread-show-or-create)
                                map)))
      "")))

(defmethod slack-thread-create ((m slack-message) team &optional payload)
  (if payload
      (let ((replies (plist-get payload :replies))
            (reply-count (plist-get payload :reply_count))
            (unread-count (plist-get payload :unread_count))
            (last-read (plist-get payload :last_read)))
        (make-instance 'slack-thread
                       :thread_ts (oref m ts)
                       :root m
                       :replies replies
                       :reply_count (or reply-count 0)
                       :unread_count (or unread-count 1)
                       :last_read last-read))
    (make-instance 'slack-thread
                   :thread_ts (oref m ts)
                   :root m)))

(defmethod slack-merge ((old slack-thread) new)
  (oset old replies (oref new replies))
  (oset old reply-count (oref new reply-count))
  (oset old unread-count (oref new unread-count)))

(defun slack-thread-update-state (payload team)
  (slack-if-let* ((message-payload (plist-get payload :message))
                  (thread-ts (plist-get message-payload :thread_ts))
                  (room (slack-room-find (plist-get payload :channel) team))
                  (message (slack-room-find-message room thread-ts))
                  (thread (slack-message-get-thread message team))
                  (new-thread (slack-thread-create message team message-payload)))
      (progn
        (slack-merge thread new-thread)
        (slack-message-update message team t t))
    (message "THREAD_TS: %s, ROOM: %s, MESSAGE: %s THREAD: %s, NEW_THREAD:%s"
             thread-ts
             (not (null room))
             (not (null message))
             (not (null thread))
             (not (null new-thread)))))

(defmethod slack-thread-equal ((thread slack-thread) other)
  (and (string-equal (oref thread thread-ts)
                     (oref other thread-ts))
       (string-equal (oref (oref thread root) channel)
                     (oref (oref other root) channel))))

(cl-defun slack-thread-get-all (&key (sync nil) (ts nil))
  (let ((team (slack-team-select)))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-thread-get-all")
                      (let ((threads-data (append (plist-get data :threads) nil))
                            (total-unread (plist-get data :total_unread_replies))
                            (more (if (eq :json-false (plist-get data :has_more)) nil t))
                            (new-count (plist-get data :new_threads_count)))
                        (with-slots (threads) team
                          (with-slots
                              (initializedp total-unread-replies new-threads-count has-more) threads
                            (setq has-more more)
                            (setq initializedp t)
                            (setq total-unread-replies total-unread)
                            (setq new-threads-count new-count)
                            (let ((parents (cl-loop for thread in threads-data
                                                    collect (slack-message-create
                                                             (plist-get thread :root_msg) team))))
                              (mapc #'(lambda (parent) (slack-message-update parent team nil t))
                                    parents))))))))
      (slack-request
       (slack-request-create
        all-threads-url
        team
        :type "POST"
        :params (list (cons "limit" "10")
                      (cons "current_ts" (or ts (format-time-string "%s"))))
        :success #'on-success)))))

(defmethod slack-thread-title ((thread slack-thread) team)
  (with-slots (root) thread
    (let ((room (slack-room-find (oref root channel) team))
          (body (slack-message-body root team)))
      (when room
        (format "%s - %s" (slack-room-name room)
                (concat (substring body 0 (min 50 (length body))) "..."))))))

(defun slack-thread-select (&optional reload)
  (interactive)
  (cl-labels
      ((load-threads (threads)
                     (slack-thread-get-all :sync t
                                           :ts (cl-first
                                                (cl-sort
                                                 (mapcar #'(lambda (thread) (oref thread thread-ts)) threads)
                                                 #'string<))))
       (select-thread (threads team has-more)
                      (let* ((alist (cl-remove-if-not
                                     #'(lambda (cons) (car cons))
                                     (mapcar #'(lambda (thread)
                                                 (let ((title (slack-thread-title thread team)))
                                                   (and title (cons title thread))))
                                             threads)))
                             (maybe-has-more (if has-more
                                                 (append alist (list (cons "(load more)" 'load-more))) alist))
                             (selected (slack-select-from-list (maybe-has-more "Select Thread: "))))
                        selected))
       (collect-thread-parents (messages)
                               (mapcar #'(lambda (m) (oref m thread))
                                       (cl-remove-if #'(lambda (m) (not (slack-message-thread-parentp m)))
                                                     messages)))
       (collect-threads (team)
                        (cl-loop for room in (with-slots (groups ims channels) team
                                               (append ims groups channels))
                                 append (collect-thread-parents (oref room messages)))))

    (let* ((team (slack-team-select)))

      (with-slots (initializedp has-more) (oref team threads)
        (if (or (not initializedp) has-more) (load-threads (collect-threads team))))

      (let ((selected (select-thread (collect-threads team) team nil)))
        (if (eq selected 'load-more)
            (slack-thread-select t)
          (slack-thread-show-messages selected
                                      (slack-room-find (oref (oref selected root) channel) team)
                                      team))))))

(defmethod slack-thread-delete-message ((thread slack-thread) message)
  (with-slots (messages reply-count) thread
    (setq messages (cl-remove-if #'(lambda (e) (string= (oref e ts) (oref message ts)))
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

(defun slack-room-unread-threads ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-unread-threads buf)))

(defmethod slack-thread-update-last-read ((thread slack-thread) msg)
  (with-slots (ts) msg
    (oset thread last-read ts)))

(provide 'slack-thread)
;;; slack-thread.el ends here
