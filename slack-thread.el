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
(require 'slack-message)

(defvar lui-prompt-string "> ")
(defconst all-threads-url "https://slack.com/api/subscriptions.thread.getView")
(defconst thread-mark-url "https://slack.com/api/subscriptions.thread.mark")

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
  (let* ((team (slack-team-find slack-current-team-id))
         (room (and team(slack-room-find slack-current-room-id team)))
         (ts (slack-get-ts))
         (message (slack-room-find-message room ts))
         (buf (and room ts (slack-thread-get-buffer-create room team ts))))
    (unless buf (error "Can't create slack thread buffer"))
    (if (object-of-class-p message 'slack-reply-broadcast-message)
        (error "Can't start thread from broadcasted message"))
    (with-current-buffer buf
      (slack-thread-insert-as-root-message message team))
    (funcall slack-buffer-function buf)))

(defun slack-thread-message--send (message)
  (if slack-current-team-id
      (let* ((team (slack-team-find slack-current-team-id))
             (room (slack-room-find slack-current-room-id team))
             (message (slack-message-prepare-links (slack-escape-message message) team))
             (broadcast (y-or-n-p (format "Also send to %s ? " (slack-room-name room)))))

        (slack-message-inc-id team)
        (with-slots (message-id sent-message self-id) team
          (let* ((payload (list :id message-id
                                :channel (oref room id)
                                :reply_broadcast broadcast
                                :thread_ts slack-target-ts
                                :type "message"
                                :user self-id
                                :text message))
                 (json (json-encode payload))
                 (obj (slack-message-create payload team :room room)))
            (slack-ws-send json team)
            (puthash message-id obj sent-message))))))

(defmethod slack-thread-update-buffer ((thread slack-thread) message room team &key replace)
  (let ((buf (get-buffer (slack-thread-buf-name room (oref thread thread-ts)))))
    (when buf
      (if replace
          (slack-buffer-replace buf message)
        (with-current-buffer buf
          (slack-buffer-insert message team))))))

(defun slack-thread-buf-name (room thread-ts)
  (format "%s %s - %s" (slack-room-buffer-name room) "Thread" thread-ts))

(defun slack-thread-get-buffer-create (room team thread-ts)
  (let* ((buf-name (slack-thread-buf-name room thread-ts))
         (buf (get-buffer buf-name)))
    (when buf
      (with-current-buffer buf (kill-buffer))
      (setq buf nil))
    (unless buf
      (setq buf (generate-new-buffer buf-name))
      (with-current-buffer buf
        (slack-thread-mode)
        (slack-buffer-enable-emojify)
        (goto-char lui-input-marker)
        (set (make-local-variable 'slack-target-ts) thread-ts)
        (set (make-local-variable 'slack-current-team-id) (oref team id))
        (set (make-local-variable 'slack-current-room-id) (oref room id))
        ;; (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
        (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)))
    buf))

(defun slack-thread-show-or-create ()
  (interactive)
  (if (eq major-mode 'slack-thread-mode)
      (error "Already in thread")
    (let* ((line (thing-at-point 'line))
           (team-id slack-current-team-id)
           (team (and team-id (slack-team-find team-id)))
           (room-id slack-current-room-id)
           (room (and room-id team (slack-room-find room-id team)))
           (ts (slack-get-ts)))
      (if (or (not team) (not room) (not ts))
          (error "Can't find: %s" (or (and (not team) "team")
                                      (and (not room) "room")
                                      (and (not ts) "ts"))))
      (let* ((thread (slack-room-find-thread room ts)))
        (if thread
            (slack-thread-show-messages thread room team)
          (slack-thread-start))))))

(defun slack-thread-insert-as-root-message (message team)
  (slack-buffer-insert message team)
  (let ((lui-time-stamp-position nil))
    (lui-insert (format "%s\n" (make-string lui-fill-column ?=)) t)))

(defmethod slack-room-replies-url ((_room slack-channel))
  "https://slack.com/api/channels.replies")

(defmethod slack-room-replies-url ((_room slack-group))
  "https://slack.com/api/groups.replies")

(defmethod slack-room-replies-url ((_room slack-im))
  "https://slack.com/api/im.replies")

(defmethod slack-thread-request-messages ((thread slack-thread) room team)
  (with-slots (thread-ts) thread
    (with-slots (id) room
      (cl-labels
          ((on-success (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-thread-request-messages")
                        (let ((messages (mapcar #'(lambda (payload) (slack-message-create payload team :room room))
                                                (plist-get data :messages))))
                          (mapc #'(lambda (m) (slack-thread-add-message thread m))
                                (cl-remove-if #'slack-message-thread-parentp messages))))))

        (slack-request (slack-room-replies-url room)
                       team
                       :params (list (cons "thread_ts" thread-ts)
                                     (cons "channel" id))
                       :success #'on-success)))))

(defmethod slack-thread-show-messages ((thread slack-thread) room team)
  (let* ((buf (slack-thread-get-buffer-create room team (oref thread thread-ts)))
         (messages (slack-room-sort-messages (copy-sequence (oref thread messages)))))
    (if (< (length messages) (oref thread reply-count))
        (progn
          (slack-thread-request-messages thread room team)
          (setq messages (oref thread messages))))
    (with-current-buffer buf
      (slack-thread-insert-as-root-message (oref thread root) team)
      (cl-loop for m in messages
               do (slack-buffer-insert m team)))
    (funcall slack-buffer-function buf)
    (oset thread unread-count 0)
    (let* ((msg (car (last messages)))
           (room (and msg (slack-room-find (oref msg channel) team))))
      (when (and msg room (string< (oref room last-read) (oref msg ts)))
        (slack-room-update-last-read room msg)
        (slack-room-update-mark room team msg))
      (when (and msg room (string< (oref thread last-read) (oref msg ts)))
        (slack-thread-update-last-read thread msg)
        (slack-thread-update-mark thread room msg team)))))

(defmethod slack-thread-to-string ((m slack-message) team)
  (with-slots (thread) m
    (if thread
        (let* ((usernames (mapconcat #'identity
                                     (cl-remove-duplicates
                                      (mapcar #'(lambda (reply) (slack-user-name (plist-get reply :user) team))
                                              (oref thread replies))
                                      :test #'string=)
                                     " "))
               (text (format "\n%s reply from %s" (oref thread reply-count) usernames)))
          (propertize text
                      'face '(:underline t)
                      'keymap (let ((map (make-sparse-keymap)))
                                (define-key map (kbd "RET") #'slack-thread-show-messages)
                                map)))
      "")))

(defmethod slack-thread-create ((m slack-message) team &optional payload)
  (let* ((replies (and payload (append (plist-get payload :replies) nil)))
         (reply-count (and payload (plist-get payload :reply_count))
                      )
         (unread-count (and payload (plist-get payload :unread_count)))
         (last-read (and payload (plist-get payload :last_read)))
         (thread (make-instance 'slack-thread
                                :thread_ts (oref m ts)
                                :root m
                                :replies replies
                                :reply_count (or reply-count 0)
                                :unread_count (or unread-count 1)
                                :last_read last-read)))

    ;; (with-slots (threads) team
    ;;   (cl-pushnew thread (oref threads all) :test #'slack-thread-equal))
    ;; thread)
    thread))

(defmethod slack-thread-set-messages ((thread slack-thread) messages)
  (let ((count (length messages)))
    (oset thread messages messages)
    (oset thread reply-count count)
    (oset thread replies (mapcar #'(lambda (m) (list :user (slack-message-sender-id m)
                                                     :ts (oref m ts)))
                                 messages))))

(defmethod slack-thread-add-message ((thread slack-thread) msg)
  (with-slots (messages) thread
    (cl-pushnew msg messages :test #'slack-message-equal)
    (setq messages (slack-room-sort-messages (copy-sequence messages)))))

(defmethod slack-message-thread-parentp ((m slack-message))
  (let* ((thread (oref m thread))
         (thread-ts (or (and thread (oref thread thread-ts))
                        (oref m thread-ts))))
    (and thread-ts (string= (oref m ts) thread-ts))))

(defmethod slack-message-thread-messagep ((m slack-message))
  (and (oref m thread-ts) (not (slack-message-thread-parentp m))))

(defun slack-thread-update-state (payload team)
  (let* ((room (slack-room-find (plist-get payload :channel) team))
         (state (plist-get payload :message))
         (message (and room (slack-room-find-message room (plist-get state :ts))))
         (thread (and message (slack-message-get-thread message team))))
    (when thread
      (with-slots (replies reply-count) thread
        (setq replies (append (plist-get state :replies) nil))
        (setq reply-count (plist-get state :reply_count)))
      (slack-message-update message team t t))))

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
       all-threads-url
       team
       :type "POST"
       :params (list (cons "limit" "10")
                     (cons "current_ts" (or ts (format-time-string "%s"))))
       :sync sync
       :success #'on-success))))

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

(defun slack-thread-setup-edit-buf (thread-ts room team type)
  (slack-message-setup-edit-buf room type :ts thread-ts :team team))

(defmethod slack-thread-delete-message ((thread slack-thread) message)
  (with-slots (messages) thread
    (setq messages (cl-remove-if #'(lambda (e) (string= (oref e ts) (oref message ts)))
                                 messages))))

(defmethod slack-thread-update-mark ((thread slack-thread) room msg team)
  (with-slots (thread-ts) thread
    (with-slots (id) room
      (with-slots (ts) msg
        (cl-labels
            ((on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-thread-mark"))))

          (slack-request
           thread-mark-url
           team
           :params (list (cons "channel" id)
                         (cons "thread_ts" thread-ts)
                         (cons "ts" ts))
           :sync nil
           :success #'on-success))))))

(defmethod slack-thread-marked ((thread slack-thread) payload)
  (let ((unread-count (plist-get payload :unread_count))
        (last-read (plist-get payload :last_read)))
    (oset thread unread-count unread-count)
    (oset thread last-read last-read)))

(defun slack-room-unread-threads ()
  (interactive)
  (if (or (not (boundp 'slack-current-team-id))
          (not (boundp 'slack-current-room-id)))
      (error "Call from Slack Buffer"))
  (let* ((team (slack-team-find slack-current-team-id))
         (room (slack-room-find slack-current-room-id team))

         (threads (mapcar #'(lambda (m) (oref m thread))
                          (cl-remove-if
                           #'(lambda (m)
                               (or (not (slack-message-thread-parentp m))
                                   (not (< 0 (oref (oref m thread) unread-count)))))
                           (oref room messages))))
         (alist (mapcar #'(lambda (thread) (cons (slack-thread-title thread team) thread))
                        (cl-sort threads
                                 #'string>
                                 :key #'(lambda (thread) (oref thread thread-ts)))))
         (selected (slack-select-from-list (alist "Select Thread: "))))
    (slack-thread-show-messages selected room team)))

(defmethod slack-thread-update-last-read ((thread slack-thread) msg)
  (with-slots (ts) msg
    (oset thread last-read ts)))

(provide 'slack-thread)
;;; slack-thread.el ends here
