;;; slack-websocket.el --- slack websocket interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'websocket)
(require 'slack-util)
(require 'slack-request)
(require 'slack-message)
(require 'slack-team)
(require 'slack-reply)
(require 'slack-file)
(require 'slack-dialog)
(defconst slack-api-test-url "https://slack.com/api/api.test")

(defclass slack-typing ()
  ((room :initarg :room :initform nil)
   (limit :initarg :limit :initform nil)
   (users :initarg :users :initform nil)))

(defclass slack-typing-user ()
  ((limit :initarg :limit :initform nil)
   (user-name :initarg :user-name :initform nil)))

(defun slack-ws-set-connect-timeout-timer (team)
  (slack-ws-cancel-connect-timeout-timer team)
  (cl-labels
      ((on-timeout ()
                   (slack-log (format "websocket open timeout")
                              team)
                   (slack-ws-close team)
                   (slack-ws-set-reconnect-timer team)))
    (oset team websocket-connect-timeout-timer
          (run-at-time (oref team websocket-connect-timeout-sec)
                       nil
                       #'on-timeout))))

(defun slack-ws-cancel-connect-timeout-timer (team)
  (when (timerp (oref team websocket-connect-timeout-timer))
    (cancel-timer (oref team websocket-connect-timeout-timer))
    (oset team websocket-connect-timeout-timer nil)))

(cl-defun slack-ws-open (team &key (on-open nil) (ws-url nil))
  (slack-if-let* ((conn (oref team ws-conn))
                  (state (websocket-ready-state conn)))
      (cond ((websocket-openp conn)
             (slack-log "Websocket is Already Open" team))
            ((eq state 'connecting)
             (slack-log "Websocket is connecting" team))
            ((eq state 'closed)
             (slack-log "Websocket is closed" team)))

    (progn
      (slack-ws-set-connect-timeout-timer team)
      (cl-labels
          ((on-message (websocket frame)
                       (slack-ws-on-message websocket frame team))
           (handle-on-open (_websocket)
                           (oset team reconnect-count 0)
                           (oset team connected t)
                           (slack-log "WebSocket on-open"
                                      team :level 'debug)
                           (when (functionp on-open)
                             (funcall on-open)))
           (on-close (websocket)
                     (oset team ws-conn nil)
                     (oset team connected nil)
                     (slack-log (format "Websocket on-close: STATE: %s"
                                        (websocket-ready-state websocket))
                                team :level 'debug)
                     (unwind-protect
                         (progn
                           (unless (oref team inhibit-reconnection)
                             (slack-ws-set-reconnect-timer team)))
                       (oset team inhibit-reconnection nil)))
           (on-error (_websocket type err)
                     (slack-log (format "Error on `websocket-open'. TYPE: %s, ERR: %s"
                                        type err)
                                team
                                :level 'error)))
        (oset team ws-conn
              (condition-case error-var
                  (websocket-open (or ws-url (oref team ws-url))
                                  :on-message #'on-message
                                  :on-open #'handle-on-open
                                  :on-close #'on-close
                                  :on-error #'on-error
                                  :nowait (oref team websocket-nowait))
                (error
                 (slack-log (format "An Error occured while opening websocket connection: %s"
                                    error-var)
                            team
                            :level 'error)
                 ;; (slack-ws-close team)
                 ;; (slack-ws-set-reconnect-timer team)
                 nil)))))))

(cl-defun slack-ws-close (&optional team (close-reconnection nil))
  (interactive)
  (unless team
    (setq team slack-teams))
  (let ((called-interactively (called-interactively-p 'any)))
    (cl-labels
        ((close (team)
                (slack-ws-cancel-ping-timer team)
                (slack-ws-cancel-ping-check-timers team)
                (when (or close-reconnection
                          called-interactively)
                  (slack-ws-cancel-reconnect-timer team)
                  (oset team inhibit-reconnection t))
                (with-slots (connected ws-conn last-pong) team
                  (when ws-conn
                    (websocket-close ws-conn)
                    (slack-log "Slack Websocket Closed" team)))))
      (if (listp team)
          (progn
            (mapc #'close team)
            (slack-request-worker-quit))
        (close team)
        (slack-request-worker-remove-request team)
        )
      )))

(defun slack-ws-send (payload team)
  (with-slots (waiting-send ws-conn) team
    (push payload waiting-send)
    (cl-labels
        ((reconnect ()
                    (slack-ws-close team)
                    (slack-ws-set-reconnect-timer team)))
      (if (websocket-openp ws-conn)
          (condition-case err
              (websocket-send-text ws-conn (json-encode payload))
            (error
             (slack-log (format "Error in `slack-ws-send`: %s" err)
                        team :level 'debug)
             (reconnect)))
        (reconnect)))))

(defun slack-ws-resend (team)
  (with-slots (waiting-send) team
    (let ((candidate waiting-send))
      (setq waiting-send nil)
      (cl-loop for msg in candidate
               do (slack-ws-send msg team)))))

;; (:type error :error (:msg Socket URL has expired :code 1))
(defun slack-ws-handle-error (payload team)
  (let* ((err (plist-get payload :error))
         (code (plist-get err :code)))
    (cond
     ((eq 1 code)
      (slack-ws-close team)
      (slack-ws-set-reconnect-timer team))
     (t (slack-log (format "Unknown Error: %s, MSG: %s"
                           code (plist-get err :msg))
                   team)))))

(defun slack-ws-on-message (_websocket frame team)
  ;; (message "%s" (slack-request-parse-payload
  ;;                (websocket-frame-payload frame)))
  (when (websocket-frame-completep frame)
    (let* ((payload (slack-request-parse-payload
                     (websocket-frame-payload frame)))
           (decoded-payload (and payload (slack-decode payload)))
           (type (and decoded-payload
                      (plist-get decoded-payload :type))))
      ;; (message "%s" decoded-payload)
      (when (slack-team-event-log-enabledp team)
        (slack-log-websocket-payload decoded-payload team))
      (when decoded-payload
        (cond
         ((string= type "error")
          (slack-ws-handle-error decoded-payload team))
         ((string= type "pong")
          (slack-ws-handle-pong decoded-payload team))
         ((string= type "hello")
          (slack-ws-cancel-connect-timeout-timer team)
          (slack-ws-cancel-reconnect-timer team)
          (slack-cancel-notify-adandon-reconnect)
          (slack-ws-set-ping-timer team)
          (slack-ws-resend team)
          (slack-log "Slack Websocket Is Ready!" team :level 'info))
         ((plist-get decoded-payload :reply_to)
          (slack-ws-handle-reply decoded-payload team))
         ((string= type "message")
          (slack-ws-handle-message decoded-payload team))
         ((string= type "reaction_added")
          (slack-ws-handle-reaction-added decoded-payload team))
         ((string= type "reaction_removed")
          (slack-ws-handle-reaction-removed decoded-payload team))
         ((string= type "channel_created")
          (slack-ws-handle-channel-created decoded-payload team))
         ((or (string= type "channel_archive")
              (string= type "group_archive"))
          (slack-ws-handle-room-archive decoded-payload team))
         ((or (string= type "channel_unarchive")
              (string= type "group_unarchive"))
          (slack-ws-handle-room-unarchive decoded-payload team))
         ((string= type "channel_deleted")
          (slack-ws-handle-channel-deleted decoded-payload team))
         ((or (string= type "channel_rename")
              (string= type "group_rename"))
          (slack-ws-handle-room-rename decoded-payload team))
         ((or (string= type "channel_left")
              (string= type "group_left"))
          (slack-ws-handle-room-left decoded-payload team))
         ((string= type "channel_joined")
          (slack-ws-handle-channel-joined decoded-payload team))
         ((string= type "group_joined")
          (slack-ws-handle-group-joined decoded-payload team))
         ((string= type "presence_change")
          (slack-ws-handle-presence-change decoded-payload team))
         ((or (string= type "bot_added")
              (string= type "bot_changed"))
          (slack-ws-handle-bot decoded-payload team))
         ((string= type "file_created")
          (slack-ws-handle-file-created decoded-payload team))
         ((or (string= type "file_deleted")
              (string= type "file_unshared"))
          (slack-ws-handle-file-deleted decoded-payload team))
         ((or (string= type "im_marked")
              (string= type "channel_marked")
              (string= type "group_marked"))
          (slack-ws-handle-room-marked decoded-payload team))
         ((string= type "thread_marked")
          (slack-ws-handle-thread-marked decoded-payload team))
         ((string= type "thread_subscribed")
          (slack-ws-handle-thread-subscribed decoded-payload team))
         ((string= type "im_open")
          (slack-ws-handle-im-open decoded-payload team))
         ((string= type "im_close")
          (slack-ws-handle-im-close decoded-payload team))
         ((string= type "team_join")
          (slack-ws-handle-team-join decoded-payload team))
         ((string= type "user_typing")
          (slack-ws-handle-user-typing decoded-payload team))
         ((string= type "user_change")
          (slack-ws-handle-user-change decoded-payload team))
         ((string= type "member_joined_channel")
          (slack-ws-handle-member-joined-channel decoded-payload team))
         ((string= type "member_left_channel")
          (slack-ws-handle-member-left_channel decoded-payload team))
         ((or (string= type "dnd_updated")
              (string= type "dnd_updated_user"))
          (slack-ws-handle-dnd-updated decoded-payload team))
         ((string= type "star_added")
          (slack-ws-handle-star-added decoded-payload team))
         ((string= type "star_removed")
          (slack-ws-handle-star-removed decoded-payload team))
         ((string= type "reconnect_url")
          (slack-ws-handle-reconnect-url decoded-payload team))
         ((string= type "app_conversation_invite_request")
          (slack-ws-handle-app-conversation-invite-request decoded-payload team))
         ((string= type "commands_changed")
          (slack-ws-handle-commands-changed decoded-payload team))
         ((string= type "dialog_opened")
          (slack-ws-handle-dialog-opened decoded-payload team))
         )))))

(defun slack-ws-handle-reconnect-url (payload team)
  (oset team reconnect-url (plist-get payload :url)))

(defun slack-user-typing (team)
  (with-slots (typing typing-timer) team
    (with-slots (limit users room) typing
      (let ((current (float-time)))
        (if (and typing-timer (timerp typing-timer)
                 (< limit current))
            (progn
              (cancel-timer typing-timer)
              (setq typing-timer nil)
              (setq typing nil))
          (if (slack-buffer-show-typing-p
               (get-buffer (slack-room-buffer-name room team)))
              (let ((team-name (slack-team-name team))
                    (room-name (slack-room-name room team))
                    (visible-users (cl-remove-if
                                    #'(lambda (u) (< (oref u limit) current))
                                    users)))
                (slack-log
                 (format "Slack [%s - %s] %s is typing..."
                         team-name room-name
                         (mapconcat #'(lambda (u) (oref u user-name))
                                    visible-users
                                    ", "))
                 team
                 :level 'info))))))))

(defun slack-ws-handle-user-typing (payload team)
  (let* ((user (slack-user-name (plist-get payload :user) team))
         (room (slack-room-find (plist-get payload :channel) team)))
    (if (and user room
             (slack-buffer-show-typing-p (get-buffer (slack-room-buffer-name room team))))
        (let ((limit (+ 3 (float-time))))
          (with-slots (typing typing-timer) team
            (if (and typing (equal room (oref typing room)))
                (with-slots ((typing-limit limit)
                             (typing-room room) users) typing
                  (setq typing-limit limit)
                  (let ((typing-user (make-instance 'slack-typing-user
                                                    :limit limit
                                                    :user-name user)))
                    (setq users
                          (cons typing-user
                                (cl-remove-if #'(lambda (u)
                                                  (string= (oref u user-name)
                                                           user))
                                              users))))))
            (unless typing
              (let ((new-typing (make-instance 'slack-typing
                                               :room room :limit limit))
                    (typing-user (make-instance 'slack-typing-user
                                                :limit limit :user-name user)))
                (oset new-typing users (list typing-user))
                (setq typing new-typing))
              (setq typing-timer
                    (run-with-timer t 1 #'slack-user-typing team))))))))

(defun slack-ws-handle-team-join (payload team)
  (let ((user (slack-decode (plist-get payload :user))))
    (slack-user-info-request
     (plist-get user :id) team
     :after-success #'(lambda ()
                        (slack-log (format "User %s Joind Team: %s"
                                           (plist-get (slack-user--find (plist-get user :id)
                                                                        team)
                                                      :name)
                                           (slack-team-name team))
                                   team
                                   :level 'info)))))

(defun slack-ws-handle-im-open (payload team)
  (cl-labels
      ((notify
        (im)
        (slack-room-history-request
         im team
         :after-success #'(lambda (&rest _ignore)
                            (slack-log (format "Direct Message Channel with %s is Open"
                                               (slack-user-name (oref im user) team))
                                       team :level 'info))
         :async t)))
    (let ((exist (slack-room-find (plist-get payload :channel) team)))
      (if exist
          (progn
            (oset exist is-open t)
            (notify exist))
        (with-slots (ims) team
          (let ((im (slack-room-create
                     (list :id (plist-get payload :channel)
                           :user (plist-get payload :user))
                     team 'slack-im)))
            (setq ims (cons im ims))
            (notify im)))))))

(defun slack-ws-handle-im-close (payload team)
  (let ((im (slack-room-find (plist-get payload :channel) team)))
    (when im
      (oset im is-open nil)
      (slack-log (format "Direct Message Channel with %s is Closed"
                         (slack-user-name (oref im user) team))
                 team :level 'info))))

(defun slack-ws-handle-message (payload team)
  (let ((subtype (plist-get payload :subtype)))
    (cond
     ((and subtype (string= subtype "message_changed"))
      (slack-ws-change-message payload team))
     ((and subtype (string= subtype "message_deleted"))
      (slack-ws-delete-message payload team))
     ((and subtype (string= subtype "message_replied"))
      (slack-thread-update-state payload team))
     (t
      (slack-ws-update-message payload team)))))

(defun slack-ws-change-message (payload team)
  (slack-if-let* ((room-id (plist-get payload :channel))
                  (room (slack-room-find room-id team))
                  (message-payload (plist-get payload :message))
                  (ts (plist-get message-payload :ts))
                  (base (slack-room-find-message room ts))
                  (new-message (slack-message-create message-payload
                                                     team
                                                     :room room)))
      (slack-message-update base team t
                            (not (slack-message-changed--copy base new-message)))))


(defun slack-ws-delete-message (payload team)
  (slack-if-let* ((room-id (plist-get payload :channel))
                  (room (slack-room-find room-id team))
                  (ts (plist-get payload :deleted_ts))
                  (message (slack-room-find-message room ts)))
      (slack-message-deleted message room team)))

(defun slack-ws-update-message (payload team)
  (let ((subtype (plist-get payload :subtype)))
    (if (string= subtype "bot_message")
        (slack-ws-update-bot-message payload team)
      (slack-message-update (slack-message-create payload team)
                            team))))

(defun slack-ws-update-bot-message (payload team)
  (let* ((bot-id (plist-get payload :bot_id))
         (username (plist-get payload :username))
         (user (plist-get payload :user))
         (bot (or (slack-find-bot bot-id team)
                  (slack-find-bot-by-name username team)
                  (slack-user--find user team)))
         (message (slack-message-create payload team)))
    (if bot
        (slack-message-update message team)
      (slack-bot-info-request bot-id
                              team
                              #'(lambda (team)
                                  (slack-message-update message team))))))

(defun slack-ws-remove-from-resend-queue (payload team)
  (with-slots (waiting-send) team
    (slack-log (format "waiting-send: %s" (length waiting-send))
               team :level 'trace)
    (setq waiting-send
          (cl-remove-if #'(lambda (e) (eq (plist-get e :id)
                                          (plist-get payload :reply_to)))
                        waiting-send))
    (slack-log (format "waiting-send: %s" (length waiting-send))
               team :level 'trace)))

(defun slack-ws-handle-reply (payload team)
  (let ((ok (plist-get payload :ok)))
    (if (eq ok :json-false)
        (let ((err (plist-get payload :error)))
          (slack-log (format "Error code: %s msg: %s"
                             (plist-get err :code)
                             (plist-get err :msg))
                     team))
      (let ((message-id (plist-get payload :reply_to)))
        (when (integerp message-id)
          (slack-message-handle-reply
           (slack-message-create payload team)
           team)
          (slack-ws-remove-from-resend-queue payload team))))))

(defun slack-ws-handle-reaction-added-to-file (file-id reaction team)
  (let* ((file (slack-file-find file-id team))
         (item-type "file"))
    (cl-labels
        ((update (&rest _args)
                 (slack-with-file file-id team
                   (slack-message-append-reaction file reaction)
                   (slack-message-update file team)
                   (cl-loop for channel in (slack-file-channel-ids file)
                            do (slack-if-let*
                                   ((channel (slack-room-find channel team))
                                    (message (slack-room-find-file-share-message
                                              channel (oref file id))))

                                   (progn
                                     (slack-message-append-reaction message
                                                                    reaction
                                                                    item-type
                                                                    file-id)
                                     (slack-message-update message
                                                           team t t)))))))
      (if file (update)
        (slack-file-request-info file-id 1 team #'update)))))

(defun slack-ws-handle-reaction-added (payload team)
  (let* ((item (plist-get payload :item))
         (item-type (plist-get item :type))
         (reaction (make-instance 'slack-reaction
                                  :name (plist-get payload :reaction)
                                  :count 1
                                  :users (list (plist-get payload :user)))))
    (cl-labels
        ((update-message (message)
                         (slack-message-append-reaction message reaction item-type)
                         (slack-message-update message team t t)))
      (cond
       ((string= item-type "file")
        (slack-ws-handle-reaction-added-to-file (plist-get item :file)
                                                reaction
                                                team))
       ((string= item-type "message")
        (slack-if-let* ((room (slack-room-find (plist-get item :channel) team))
                        (message (slack-room-find-message room (plist-get item :ts))))
            (progn
              (update-message message)
              (slack-reaction-notify payload team room))))))))

(defun slack-ws-handle-reaction-removed-from-file (file-id reaction team)
  (let* ((file (slack-file-find file-id team))
         (item-type "file"))
    (cl-labels
        ((update (&rest _args)
                 (slack-with-file file-id team
                   (slack-message-pop-reaction file reaction)
                   (slack-message-update file team)
                   (cl-loop for channel in (slack-file-channel-ids file)
                            do (slack-if-let*
                                   ((channel (slack-room-find channel team))
                                    (message (slack-room-find-file-share-message
                                              channel (oref file id))))
                                   (progn
                                     (slack-message-pop-reaction message
                                                                 reaction
                                                                 item-type
                                                                 file-id)
                                     (slack-message-update message team t t)))))))
      (if file (update)
        (slack-file-request-info file-id 1 team #'update)))))

(defun slack-ws-handle-reaction-removed (payload team)
  (let* ((item (plist-get payload :item))
         (item-type (plist-get item :type))
         (reaction (make-instance 'slack-reaction
                                  :name (plist-get payload :reaction)
                                  :count 1
                                  :users (list (plist-get payload :user)))))
    (cl-labels
        ((update-message (message)
                         (slack-message-pop-reaction message reaction item-type)
                         (slack-message-update message team t t)))
      (cond
       ((string= item-type "file")
        (slack-ws-handle-reaction-removed-from-file (plist-get item :file)
                                                    reaction
                                                    team))
       ((string= item-type "message")
        (slack-if-let* ((room (slack-room-find (plist-get item :channel) team))
                        (message (slack-room-find-message room (plist-get item :ts))))
            (progn
              (update-message message)
              (slack-reaction-notify payload team room))))))))

(defun slack-ws-handle-channel-created (payload team)
  (let ((channel (slack-room-create (plist-get payload :channel)
                                    team 'slack-channel)))
    (push channel (oref team channels))
    (slack-room-info-request channel team)
    (slack-log (format "Created channel %s"
                       (slack-room-display-name channel team))
               team :level 'info)))

(defun slack-ws-handle-room-archive (payload team)
  (let* ((id (plist-get payload :channel))
         (room (slack-room-find id team)))
    (oset room is-archived t)
    (slack-log (format "Channel: %s is archived"
                       (slack-room-display-name room team))
               team :level 'info)))

(defun slack-ws-handle-room-unarchive (payload team)
  (let* ((id (plist-get payload :channel))
         (room (slack-room-find id team)))
    (oset room is-archived nil)
    (slack-log (format "Channel: %s is unarchived"
                       (slack-room-display-name room team))
               team :level 'info)))

(defun slack-ws-handle-channel-deleted (payload team)
  (let ((id (plist-get payload :channel)))
    (slack-room-deleted id team)))

(defun slack-ws-handle-room-rename (payload team)
  (let* ((c (plist-get payload :channel))
         (room (slack-room-find (plist-get c :id) team))
         (old-name (slack-room-name room team))
         (new-name (plist-get c :name)))
    (oset room name new-name)
    (slack-log (format "Renamed channel from %s to %s"
                       old-name
                       new-name)
               team :level 'info)))
(defun slack-ws-handle-group-joined (payload team)
  (let ((group (slack-room-create (plist-get payload :channel) team 'slack-group)))
    (push group (oref team groups))
    (slack-room-info-request group team)
    (slack-log (format "Joined group %s"
                       (slack-room-display-name group team))
               team :level 'info)))

(defun slack-ws-handle-channel-joined (payload team)
  (let ((channel (slack-room-find (plist-get (plist-get payload :channel) :id) team)))
    (slack-room-info-request channel team)
    (slack-log (format "Joined channel %s"
                       (slack-room-display-name channel team))
               team :level 'info)))

(defun slack-ws-handle-presence-change (payload team)
  (let* ((id (plist-get payload :user))
         (user (slack-user--find id team))
         (presence (plist-get payload :presence)))
    (plist-put user :presence presence)))

(defun slack-ws-handle-bot (payload team)
  (let ((bot (plist-get payload :bot)))
    (with-slots (bots) team
      (push bot bots))))

(defun slack-ws-handle-file-created (payload team)
  (slack-if-let* ((file-id (plist-get (plist-get payload :file) :id))
                  (room (slack-file-room-obj team))
                  (buffer (slack-buffer-find 'slack-file-list-buffer
                                             room
                                             team)))
      (slack-file-request-info file-id 1 team
                               #'(lambda (file _team)
                                   (slack-buffer-update buffer file)))))

(defun slack-ws-handle-file-deleted (payload team)
  (let ((file-id (plist-get payload :file_id))
        (room (slack-file-room-obj team)))
    (with-slots (messages) room
      (setq messages (cl-remove-if #'(lambda (f)
                                       (string= file-id (oref f id)))
                                   messages)))))

(defun slack-ws-cancel-ping-timer (team)
  (with-slots (ping-timer) team
    (if (timerp ping-timer)
        (cancel-timer ping-timer))
    (setq ping-timer nil)))

(defun slack-ws-set-ping-timer (team)
  (slack-ws-cancel-ping-timer team)
  (cl-labels ((ping ()
                    (slack-ws-ping team)))
    (oset team ping-timer (run-at-time 10 nil #'ping))))

(defun slack-ws-current-time-str ()
  (number-to-string (time-to-seconds (current-time))))

(defun slack-ws-ping (team)
  (slack-message-inc-id team)
  (with-slots (message-id) team
    (let* ((time (slack-ws-current-time-str))
           (m (list :id message-id
                    :type "ping"
                    :time time)))
      (slack-ws-set-check-ping-timer team time)
      (slack-ws-send m team)
      (slack-log (format "Send PING: %s" time)
                 team :level 'trace))))

(defun slack-ws-set-check-ping-timer (team time)
  (puthash time (run-at-time (oref team check-ping-timeout-sec)
                             nil #'slack-ws-ping-timeout team)
           (oref team ping-check-timers)))


(defun slack-ws-ping-timeout (team)
  (slack-log "Slack Websocket PING Timeout." team :level 'warn)
  (slack-ws-close team)
  (slack-ws-set-reconnect-timer team))

(defun slack-ws-cancel-ping-check-timers (team)
  (maphash #'(lambda (key value)
               (if (timerp value)
                   (cancel-timer value)))
           (oref team ping-check-timers))
  (slack-team-init-ping-check-timers team))

(defvar slack-disconnected-timer nil)
(defun slack-notify-abandon-reconnect (team)
  (unless slack-disconnected-timer
    (setq slack-disconnected-timer
          (run-with-idle-timer 5 t
                               #'(lambda ()
                                   (slack-log
                                    "Reconnect Count Exceeded. Manually invoke `slack-start'."
                                    team :level 'error))))))

(defun slack-cancel-notify-adandon-reconnect ()
  (if (and slack-disconnected-timer
           (timerp slack-disconnected-timer))
      (progn
        (cancel-timer slack-disconnected-timer)
        (setq slack-disconnected-timer nil))))

(defun slack-request-api-test (team &optional after-success)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-request-api-test")
                    (if after-success
                        (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-api-test-url
      team
      :type "POST"
      :success #'on-success))))

(defun slack-on-authorize-for-reconnect (data team)
  (let ((team-data (plist-get data :team))
        (self-data (plist-get data :self)))
    (oset team ws-url (plist-get data :url))
    (oset team domain (plist-get team-data :domain))
    (oset team id (plist-get team-data :id))
    (oset team name (plist-get team-data :name))
    (oset team self self-data)
    (oset team self-id (plist-get self-data :id))
    (oset team self-name (plist-get self-data :name))
    (cl-labels
        ((on-open ()
                  (slack-channel-list-update team)
                  (slack-group-list-update team)
                  (slack-im-list-update team)
                  (slack-bot-list-update team)
                  (cl-loop for buffer in (oref team slack-message-buffer)
                           do (slack-if-let*
                                  ((live-p (buffer-live-p buffer))
                                   (slack-buffer (with-current-buffer buffer
                                                   (and (bound-and-true-p
                                                         slack-current-buffer)
                                                        slack-current-buffer))))
                                  (slack-buffer-load-missing-messages
                                   slack-buffer)))
                  (slack-team-kill-buffers
                   team :except '(slack-message-buffer
                                  slack-message-edit-buffer
                                  slack-message-share-buffer
                                  slack-room-message-compose-buffer))))
      (slack-ws-open team :on-open #'on-open))))

(defun slack-authorize-for-reconnect (team)
  (cl-labels
      ((on-error (&key error-thrown symbol-status &allow-other-keys)
                 (slack-log (format "Slack Reconnect Failed: %s, %s"
                                    error-thrown
                                    symbol-status)
                            team)
                 (slack-ws-set-reconnect-timer team))
       (on-success (data)
                   (slack-on-authorize-for-reconnect data team)))
    (slack-authorize team #'on-error #'on-success)))

(defun slack-ws-reconnect (team &optional force)
  "Reconnect if `reconnect-count' is not exceed `reconnect-count-max'.
if FORCE is t, ignore `reconnect-count-max'.
TEAM is one of `slack-teams'"
  (cl-labels ((abort (team)
                     (slack-notify-abandon-reconnect team)
                     (slack-ws-close team t))
              (use-reconnect-url ()
                                 (slack-log "Reconnect with reconnect-url" team)
                                 (slack-ws-open team
                                                :ws-url (oref team reconnect-url)))
              (do-reconnect (team)
                            (cl-incf (oref team reconnect-count))
                            (slack-ws-close team)
                            (if (< 0 (length (oref team reconnect-url)))
                                (slack-request-api-test team
                                                        #'use-reconnect-url)
                              (slack-authorize-for-reconnect team))))
    (with-slots
        (reconnect-count (reconnect-max reconnect-count-max)) team
      (if (and (not force) reconnect-max (< reconnect-max reconnect-count))
          (abort team)
        (do-reconnect team)
        (slack-log (format "Slack Websocket Try To Reconnect %s/%s"
                           reconnect-count
                           reconnect-max)
                   team
                   :level 'warn
                   )))))

(defun slack-ws-set-reconnect-timer (team)
  (slack-ws-cancel-reconnect-timer team)
  (cl-labels
      ((on-timeout ()
                   (slack-ws-reconnect team)))
    (oset team reconnect-timer
          (run-at-time (oref team reconnect-after-sec)
                       nil
                       #'on-timeout))))

(defun slack-ws-cancel-reconnect-timer (team)
  (with-slots (reconnect-timer) team
    (if (timerp reconnect-timer)
        (cancel-timer reconnect-timer))
    (setq reconnect-timer nil)))

(defun slack-ws-handle-pong (payload team)
  (slack-ws-remove-from-resend-queue payload team)
  (let* ((key (plist-get payload :time))
         (timer (gethash key (oref team ping-check-timers))))
    (slack-log (format "Receive PONG: %s" key)
               team :level 'trace)
    (slack-ws-set-ping-timer team)
    (when timer
      (cancel-timer timer)
      (remhash key (oref team ping-check-timers))
      (slack-log (format "Remove PING Check Timer: %s" key)
                 team :level 'trace))))

(defun slack-ws-handle-room-marked (payload team)
  (slack-if-let* ((channel (plist-get payload :channel))
                  (room (slack-room-find channel team))
                  (ts (plist-get payload :ts))
                  (unread-count-display (plist-get payload :unread_count_display)))
      (progn
        (oset room unread-count-display unread-count-display)
        (oset room last-read ts)
        (slack-update-modeline))))

(defun slack-ws-handle-thread-marked (payload team)
  (let* ((subscription (plist-get payload :subscription))
         (thread-ts (plist-get subscription :thread_ts))
         (channel (plist-get subscription :channel))
         (room (slack-room-find channel team))
         (parent (and room (slack-room-find-message room thread-ts))))
    (when (and parent (oref parent thread))
      (slack-thread-marked (oref parent thread) subscription))))

(defun slack-ws-handle-thread-subscribed (payload team)
  (let* ((thread-data (plist-get payload :subscription))
         (room (slack-room-find (plist-get thread-data :channel) team))
         (message (and (slack-room-find-message room (plist-get thread-data :thread_ts))))
         (thread (and message (oref message thread))))
    (when thread
      (slack-thread-marked thread thread-data))))

(defun slack-ws-handle-user-change (payload team)
  (let* ((user (plist-get payload :user))
         (id (plist-get user :id)))
    (with-slots (users) team
      (setq users
            (cons user
                  (cl-remove-if #'(lambda (u)
                                    (string= id (plist-get u :id)))
                                users))))))

(defun slack-ws-handle-member-joined-channel (payload team)
  (slack-if-let* ((user (plist-get payload :user))
                  (channel (slack-room-find (plist-get payload :channel) team)))
      (progn
        (cl-pushnew user (oref channel members)
                    :test #'string=)
        (slack-log (format "%s joined %s"
                           (slack-user-name user team)
                           (slack-room-name channel team))
                   team
                   :level 'info))))

(defun slack-ws-handle-member-left_channel (payload team)
  (slack-if-let* ((user (plist-get payload :user))
                  (channel (slack-room-find (plist-get payload :channel) team)))
      (progn
        (oset channel members
              (cl-remove-if #'(lambda (e) (string= e user))
                            (oref channel members)))
        (slack-log (format "%s left %s"
                           (slack-user-name user team)
                           (slack-room-name channel team))
                   team
                   :level 'info))))

(defun slack-ws-handle-dnd-updated (payload team)
  (let* ((user (slack-user--find (plist-get payload :user) team))
         (updated (slack-user-update-dnd-status user (plist-get payload :dnd_status))))
    (oset team users
          (cons updated (cl-remove-if #'(lambda (user) (string= (plist-get user :id)
                                                                (plist-get updated :id)))
                                      (oref team users))))))

;; [star_added event | Slack](https://api.slack.com/events/star_added)
(defun slack-ws-handle-star-added-to-file (file-id team)
  (let ((file (slack-file-find file-id team)))
    (cl-labels
        ((update (&rest _args)
                 (slack-with-file file-id team
                   (slack-message-star-added file)
                   (slack-message-update file team))))
      (if file (update)
        (slack-file-request-info file-id 1 team #'update)))))

(defun slack-ws-handle-star-added (payload team)
  (let* ((item (plist-get payload :item))
         (item-type (plist-get item :type)))
    (cl-labels
        ((update-message (message)
                         (slack-message-star-added message)
                         (slack-message-update message team t t)))
      (cond
       ((string= item-type "file")
        (slack-ws-handle-star-added-to-file (plist-get (plist-get item :file) :id)
                                            team))
       ((string= item-type "message")
        (slack-if-let* ((room (slack-room-find (plist-get item :channel) team))
                        (ts (plist-get (plist-get item :message) :ts))
                        (message (slack-room-find-message room ts)))
            (update-message message)))))
    (slack-if-let* ((star (oref team star)))
        (slack-star-add star item team))))


;; [2018-07-25 16:30:03] (:type star_added :user U1013370U :item (:type file :file (:id FBWDT9VH8) :date_create 1532503802 :file_id FBWDT9VH8 :user_id USLACKBOT) :event_ts 1532503802.000378)
(defun slack-ws-handle-star-removed-from-file (file-id team)
  (let ((file (slack-file-find file-id team)))
    (cl-labels
        ((update (&rest _args)
                 (slack-with-file file-id team
                   (slack-message-star-removed file)
                   (slack-message-update file team))))
      (if file (update)
        (slack-file-request-info file-id 1 team #'update)))))

(defun slack-ws-handle-star-removed (payload team)
  (let* ((item (plist-get payload :item))
         (item-type (plist-get item :type)))
    (cl-labels
        ((update-message (message)
                         (slack-message-star-removed message)
                         (slack-message-update message team t t)))
      (cond
       ((string= item-type "file")
        (slack-ws-handle-star-removed-from-file (plist-get (plist-get item :file) :id)
                                                team))
       ((string= item-type "message")
        (slack-if-let* ((room (slack-room-find (plist-get item :channel) team))
                        (ts (plist-get (plist-get item :message) :ts))
                        (message (slack-room-find-message room ts)))
            (update-message message)))))

    (slack-if-let* ((star (oref team star)))
        (slack-star-remove star item team))))

(defun slack-ws-handle-app-conversation-invite-request (payload team)
  (let* ((app-user (plist-get payload :app_user))
         (channel-id (plist-get payload :channel_id))
         (invite-message-ts (plist-get payload :invite_message_ts))
         (scope-info (plist-get payload :scope_info))
         (room (slack-room-find channel-id team)))
    (if (yes-or-no-p (format "%s\n%s\n"
                             (format "%s would like to do following in %s"
                                     (slack-user-name app-user team)
                                     (slack-room-name room team))
                             (mapconcat #'(lambda (scope)
                                            (format "* %s"
                                                    (plist-get scope
                                                               :short_description)))
                                        scope-info
                                        "\n")))
        (slack-app-conversation-allow-invite-request team
                                                     :channel channel-id
                                                     :app-user app-user
                                                     :invite-message-ts invite-message-ts)
      (slack-app-conversation-deny-invite-request team
                                                  :channel channel-id
                                                  :app-user app-user
                                                  :invite-message-ts invite-message-ts))))

(cl-defun slack-app-conversation-allow-invite-request (team &key channel
                                                            app-user
                                                            invite-message-ts)
  (let ((url "https://slack.com/api/apps.permissions.internal.add")
        (params (list (cons "channel" channel)
                      (cons "app_user" app-user)
                      (cons "invite_message_ts" invite-message-ts)
                      (cons "did_confirm" "true")
                      (cons "send_ephemeral_error_message" "true"))))
    (cl-labels
        ((log-error (err)
                    (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                       err
                                       url
                                       params)
                               team :level 'error))
         (on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-app-conversation-allow-invite-request"
                            #'log-error)
                      (message "DATA: %s" data))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :success #'on-success)))))

(cl-defun slack-app-conversation-deny-invite-request (team &key channel
                                                           app-user
                                                           invite-message-ts)
  (let ((url "https://slack.com/api/apps.permissions.internal.denyAdd")
        (params (list (cons "channel" channel)
                      (cons "app_user" app-user)
                      (cons "invite_message_ts" invite-message-ts))))
    (cl-labels
        ((log-error (err)
                    (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                       err
                                       url
                                       params)
                               team :level 'error))
         (on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-app-conversation-deny-invite-request"
                            #'log-error)
                      (message "DATA: %s" data))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :success #'on-success)))))

(defun slack-ws-handle-commands-changed (payload team)
  (let ((commands-updated (mapcar #'slack-command-create
                                  (plist-get payload :commands_updated)))
        (commands-removed (mapcar #'slack-command-create
                                  (plist-get payload :commands_removed)))
        (commands '()))
    (cl-loop for command in (oref team commands)
             if (and (not (cl-find-if #'(lambda (e) (slack-equalp command e))
                                      commands-removed))
                     (not (cl-find-if #'(lambda (e) (slack-equalp command e))
                                      commands-updated)))
             do (push command commands))
    (cl-loop for command in commands-updated
             do (push command commands))
    (oset team commands commands)))

(defun slack-ws-handle-dialog-opened (payload team)
  (slack-if-let*
      ((dialog-id (plist-get payload :dialog_id))
       (client-token (plist-get payload :client_token))
       (valid-client-tokenp (string= (slack-team-client-token team)
                                     client-token)))
      (slack-dialog-get dialog-id team)))

(defun slack-ws-handle-room-left (payload team)
  (slack-if-let* ((room (slack-room-find (plist-get payload :channel)
                                         team)))
      (progn
        (when (slot-exists-p room 'is-member)
          (oset room is-member nil))
        (when (and (not (slack-channel-p room)) (slack-group-p room))
          (oset team groups
                (cl-remove-if #'(lambda (e)
                                  (slack-room-equal-p e room))
                              (oref team groups))))
        (slack-log (format "You left %s" (slack-room-name room team))
                   team :level 'info))))


(provide 'slack-websocket)
;;; slack-websocket.el ends here

