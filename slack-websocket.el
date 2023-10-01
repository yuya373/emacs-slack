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
(require 'slack-team)
(require 'slack-team-ws)
(require 'slack-file)
(require 'slack-dialog-buffer)
(require 'slack-user)
(require 'slack-group)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-thread)
(require 'slack-bot)
(require 'slack-usergroup)
(require 'slack-slash-commands)
(require 'slack-star)
(require 'slack-message-notification)
(require 'slack-room-buffer)
(require 'slack-typing)
(require 'slack-stars-buffer)
(require 'slack-conversations)
(require 'slack-dnd-status)
(require 'slack-message-event)
(require 'slack-reply-event)
(require 'slack-reaction-event)
(require 'slack-star-event)
(require 'slack-room-event)
(require 'slack-thread-event)

(defconst slack-api-test-url "https://slack.com/api/api.test")
(defconst slack-rtm-connect-url "https://slack.com/api/rtm.connect")

(defun slack-ws-on-timeout (team-id)
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (let ((debug-on-error t))
      (slack-log (format "websocket open timeout")
                 team)
      (slack-ws--close ws team)
      (slack-ws-reconnect ws team))))

(cl-defmethod slack-ws-open ((ws slack-team-ws) team &key (on-open nil) (ws-url nil))
  (let ((websocket-nowait-p (oref ws websocket-nowait)))
    (unless websocket-nowait-p
      (slack-ws-set-connect-timeout-timer ws
                                          #'slack-ws-on-timeout
                                          (slack-team-id team)))
    (cl-labels
        ((on-message (_websocket frame)
                     (slack-ws-on-message ws frame team))
         (handle-on-open (_websocket)
                         (oset ws reconnect-count 0)
                         (oset ws connected t)
                         (slack-log "WebSocket on-open"
                                    team :level 'debug)
                         (when (functionp on-open)
                           (funcall on-open)))
         (on-close (websocket)
                   (oset ws connected nil)
                   (slack-log (format "Websocket on-close: STATE: %s"
                                      (websocket-ready-state websocket))
                              team :level 'debug))
         (on-error (_websocket type err)
                   (slack-log (format "Error on `websocket-open'. TYPE: %s, ERR: %s"
                                      type err)
                              team
                              :level 'error)))
      (slack-log (format "Opening websocket connection. NOWAIT: %s, URL: %s"
                         (oref ws websocket-nowait)
                         (oref ws url))
                 team
                 :level 'debug)
      (oset ws conn
            (condition-case error-var
                (websocket-open (or ws-url (oref ws url))
                                :on-message #'on-message
                                :on-open #'handle-on-open
                                :on-close #'on-close
                                :on-error #'on-error
                                :nowait websocket-nowait-p
                                :custom-header-alist (list (cons "Cookie" (format "d=%s" (slack-team-cookie team)))))
              (error
               (slack-log (format "An Error occured while opening websocket connection: %s"
                                  error-var)
                          team
                          :level 'error)
               nil)))
      (if websocket-nowait-p
          (slack-ws-set-connect-timeout-timer ws
                                              #'slack-ws-on-timeout
                                              (slack-team-id team)))
      (slack-log (format "Called `websocket-open' URL: %s"
                         (oref ws url))
                 team :level 'debug))))

(defun slack-ws-close ()
  (interactive)
  (mapc #'(lambda (team) (slack-ws--close (oref team ws) team t))
        (hash-table-values slack-teams-by-token))
  (slack-request-worker-quit))

(cl-defun slack-ws--close (ws team &optional (close-reconnection nil))
  (cl-labels
      ((close (ws team)
              (slack-ws-cancel-ping-timer ws)
              (slack-ws-cancel-ping-check-timers ws)
              (when close-reconnection
                (slack-ws-cancel-reconnect-timer ws)
                (oset ws inhibit-reconnection t))
              (let ((conn (oref ws conn)))
                (condition-case error-var
                    (websocket-close conn)
                  (error (slack-log (format "An Error occured while closing websocket connection: %s"
                                            error-var)
                                    team
                                    :level 'error)))
                (when (and conn (process-live-p (websocket-conn conn)))
                  (slack-log "websocket prosess still alive. call `delete-process' again."
                             team
                             :level 'debug)
                  (delete-process (websocket-conn conn)))
                (slack-log "Slack Websocket Closed" team))))
    (close ws team)
    (slack-request-worker-remove-request team)))

(defun slack-ws-payload-ping-p (payload)
  (string= "ping" (plist-get payload :type)))

(defun slack-ws-payload-presence-sub-p (payload)
  (string= "presence_sub" (plist-get payload :type)))

(defun slack-ws-retryable-payload-p (payload)
  (and (not (slack-ws-payload-ping-p payload))
       (not (slack-ws-payload-presence-sub-p payload))))

(cl-defmethod slack-ws-send ((ws slack-team-ws) payload team)
  (slack-log-websocket-payload payload team t)
  (with-slots (waiting-send conn) ws
    (when (slack-ws-retryable-payload-p payload)
      (push payload waiting-send))
    (cl-labels
        ((reconnect ()
                    (slack-ws--close ws team)
                    (slack-ws-reconnect ws team)))
      (if (websocket-openp conn)
          (condition-case err
              (websocket-send-text conn (json-encode payload))
            (error
             (slack-log (format "Error in `slack-ws-send`: %s" err)
                        team :level 'debug)
             (reconnect)))
        (reconnect)))))

(cl-defmethod slack-ws-resend ((ws slack-team-ws) team)
  (with-slots (waiting-send) ws
    (let ((candidate waiting-send))
      (setq waiting-send nil)
      (cl-loop for msg in candidate
               do (slack-ws-send ws msg team)))))

(defun slack-ws-on-ping-timeout (team-id)
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (slack-log "Slack Websocket PING Timeout." team :level 'warn)
    (slack-ws--close ws team)
    (slack-ws-reconnect ws team)))

(defun slack-ws-ping (team-id)
  (let ((team (slack-team-find team-id)))
    (with-slots (message-id ws) team
      (let* ((time (number-to-string (time-to-seconds (current-time))))
             (ping-message (list :id message-id
                                 :type "ping"
                                 :time time)))
        (slack-team-send-message team ping-message)
        (slack-log (format "Send PING: %s" time) team :level 'trace)
        (slack-ws-set-ping-check-timer ws time
                                       #'slack-ws-on-ping-timeout
                                       (slack-team-id team))
        (slack-log (format "Set PING timeout timer. timeout in %s sec"
                           (oref ws check-ping-timeout-sec))
                   team :level 'trace)
        ))))

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

(defun slack-ws-abort-reconnect (team-id)
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (slack-notify-abandon-reconnect team)
    (slack-ws--close ws team t)))

(defun slack-ws-reconnect-with-reconnect-url (team-id)
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (slack-log "Reconnect with reconnect-url" team)
    (slack-ws-open ws team
                   :ws-url (oref ws reconnect-url))))

(defun slack-ws-on-reconnect-open (team-id)
  (let* ((team (slack-team-find team-id)))
    (slack-conversations-list-update team)
    ;; (slack-user-list-update team)

    (slack-dnd-status-team-info team)
    (when (hash-table-p (oref team slack-message-buffer))
      (cl-loop for sb in (hash-table-values (oref team slack-message-buffer))
               do (slack-if-let* ((buffer (and sb (slack-buffer-buffer sb)))
                                  (live-p (buffer-live-p buffer)))
                      (slack-buffer-load-missing-messages sb))))

    (slack-team-kill-buffers
     team :except '(slack-message-buffer
                    slack-message-edit-buffer
                    slack-message-share-buffer
                    slack-room-message-compose-buffer))))

(defun slack-ws--reconnect (team-id &optional force)
  (let* ((team (slack-team-find team-id))
         (ws (oref team ws)))
    (cl-labels
        ((on-authorize-error (&key error-thrown symbol-status &allow-other-keys)
                             (slack-log (format "Reconnect Failed: %s, %s"
                                                error-thrown
                                                symbol-status)
                                        team)
                             (slack-ws-reconnect ws team))
         (on-authorize-success (data)
                               (let ((team-data (plist-get data :team))
                                     (self-data (plist-get data :self)))
                                 (slack-team-set-ws-url team (plist-get data :url))
                                 (oset team domain (plist-get team-data :domain))
                                 (oset team id (plist-get team-data :id))
                                 (oset team name (plist-get team-data :name))
                                 (oset team self self-data)
                                 (oset team self-id (plist-get self-data :id))
                                 (oset team self-name (plist-get self-data :name))
                                 (slack-ws-open ws team
                                                :on-open #'(lambda ()
                                                             (slack-ws-on-reconnect-open team-id))))))
      (if (and (not force) (slack-ws-reconnect-count-exceed-p ws))
          (slack-ws-abort-reconnect team-id)
        (progn
          (slack-ws-inc-reconnect-count ws)
          (slack-ws--close ws team)
          (if (slack-ws-use-reconnect-url-p ws)
              (slack-request-api-test team
                                      #'(lambda ()
                                          (slack-ws-reconnect-with-reconnect-url team-id)))
            (slack-authorize team
                             #'on-authorize-error
                             #'on-authorize-success))
          (slack-log (format "Reconnecting... [%s/%s]"
                             (oref ws reconnect-count)
                             (oref ws reconnect-count-max))
                     team
                     :level 'warn))))))

(cl-defmethod slack-ws-reconnect ((ws slack-team-ws) team)
  "Reconnect if `reconnect-count' is not exceed `reconnect-count-max'.
TEAM is one of `slack-teams'"
  (unless (oref ws inhibit-reconnection)
    (slack-ws-set-reconnect-timer ws
                                  #'slack-ws--reconnect
                                  (slack-team-id team))))

;; Message handler

(cl-defmethod slack-ws-handle-pong ((ws slack-team-ws) payload team)
  (slack-ws-remove-from-resend-queue ws payload team)
  (let* ((key (plist-get payload :time))
         (timer (gethash key (oref ws ping-check-timers))))
    (slack-log (format "Receive PONG: %s. RTT is %s sec"
                       key
                       (- (time-to-seconds (current-time)) (string-to-number key)))
               team :level 'trace)
    (when timer
      (cancel-timer timer)
      (remhash key (oref ws ping-check-timers))
      (slack-log (format "Remove PING Check Timer: %s" key)
                 team :level 'trace))

    (slack-ws-set-ping-timer ws #'slack-ws-ping (slack-team-id team))
    ))

;; (:type error :error (:msg Socket URL has expired :code 1))
(cl-defmethod slack-ws-handle-error ((ws slack-team-ws) payload team)
  (let* ((err (plist-get payload :error))
         (code (plist-get err :code)))
    (cond
     ((eq 1 code)
      (slack-ws--close ws team)
      (slack-ws-reconnect ws team))
     (t (slack-log (format "Unknown Error: %s, MSG: %s"
                           code (plist-get err :msg))
                   team)))))

(cl-defmethod slack-ws-on-message ((ws slack-team-ws) frame team)
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
          (slack-ws-handle-error ws decoded-payload team))
         ((string= type "pong")
          (slack-ws-handle-pong ws decoded-payload team))
         ((string= type "hello")
          (slack-ws-cancel-connect-timeout-timer ws)
          (slack-ws-cancel-reconnect-timer ws)
          (slack-cancel-notify-adandon-reconnect)
          (slack-ws-set-ping-timer ws #'slack-ws-ping (slack-team-id team))
          (slack-ws-resend ws team)
          (slack-log "Slack Websocket Is Ready!" team :level 'info))
         ((plist-get decoded-payload :reply_to)
          (slack-ws-handle-reply ws decoded-payload team))
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
         ((string= type "thread_unsubscribed")
          (slack-ws-handle-thread-unsubscribed decoded-payload team))
         ((string= type "im_open")
          (slack-ws-handle-im-open decoded-payload team))
         ((or (string= type "im_close")
              (string= type "group_close"))
          (slack-ws-handle-close decoded-payload team))
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
         ((or ;; (string= type "dnd_updated")
           (string= type "dnd_updated_user"))
          (slack-ws-handle-dnd-updated decoded-payload team))
         ((string= type "star_added")
          (slack-ws-handle-star-added decoded-payload team))
         ((string= type "star_removed")
          (slack-ws-handle-star-removed decoded-payload team))
         ((string= type "reconnect_url")
          (slack-ws-handle-reconnect-url ws decoded-payload team))
         ((string= type "app_conversation_invite_request")
          (slack-ws-handle-app-conversation-invite-request decoded-payload team))
         ((string= type "commands_changed")
          (slack-ws-handle-commands-changed decoded-payload team))
         ((string= type "dialog_opened")
          (slack-ws-handle-dialog-opened decoded-payload team))
         ((string= type "subteam_created")
          (slack-ws-handle-subteam-created decoded-payload team))
         ((string= type "subteam_updated")
          (slack-ws-handle-subteam-updated decoded-payload team))
         ((string= type "pin_removed")
          (slack-ws-handle-pin-removed decoded-payload team))
         ((string= type "pin_added")
          (slack-ws-handle-pin-added decoded-payload team))
         ((string= type "update_thread_state")
          (slack-ws-handle-update-thread-state payload team))
         )))))

(defun slack-ws-handle-update-thread-state (payload team)
  (let* ((has-unreads (eq t (plist-get payload :has_unreads)))
         (mention-count (plist-get payload :mention_count)))
    (slack-if-let* ((counts (oref team counts)))
        (slack-counts-update-threads counts has-unreads mention-count))))

(defun slack-ws-handle-pin-added (payload team)
  (let* ((item (plist-get payload :item))
         (message (plist-get item :message))
         (ts (plist-get message :ts))
         (channel-id (plist-get payload :channel_id)))
    (slack-if-let*
        ((room (slack-room-find channel-id team))
         (message (slack-room-find-message room ts)))
        (cl-pushnew channel-id (oref message pinned-to)
                    :test #'string=))))

(defun slack-ws-handle-pin-removed (payload team)
  (let* ((item (plist-get payload :item))
         (message (plist-get item :message))
         (ts (plist-get message :ts))
         (channel-id (plist-get payload :channel_id)))
    (slack-if-let*
        ((room (slack-room-find channel-id team))
         (message (slack-room-find-message room ts)))
        (oset message pinned-to (cl-remove-if #'(lambda (e) (string= channel-id e))
                                              (oref message pinned-to))))))

(cl-defmethod slack-ws-handle-reconnect-url ((ws slack-team-ws) payload)
  (oset ws reconnect-url (plist-get payload :url)))

(defun slack-ws-handle-user-typing (payload team)
  (slack-if-let*
      ((user-id (plist-get payload :user))
       (room (slack-room-find (plist-get payload :channel) team))
       (buf (slack-buffer-find 'slack-message-buffer team room))
       (show-typing-p (slack-buffer-show-typing-p (get-buffer
                                                   (slack-buffer-name buf)))))
      (cl-labels
          ((update-typing (user)
                          (let ((limit (+ 3 (float-time))))
                            (slack-if-let* ((typing (oref team typing))
                                            (typing-room (slack-room-find (oref typing room-id) team))
                                            (same-room-p (string= (oref room id) (oref typing-room id))))
                                (progn
                                  (slack-typing-set-limit typing limit)
                                  (slack-typing-add-user typing user limit))
                              (oset team
                                    typing
                                    (slack-typing-create room limit user))
                              (oset team
                                    typing-timer
                                    (run-with-timer t 1
                                                    #'slack-typing-display
                                                    (slack-team-id team)))))))
        (slack-if-let*
            ((user (slack-user-name user-id team)))
            (update-typing user)
          (slack-user-info-request
           user-id team
           :after-success
           #'(lambda ()
               (update-typing (slack-user-name user-id team))))))))

(defun slack-ws-handle-team-join (payload team)
  (let ((user (slack-decode (plist-get payload :user))))
    (cl-labels
        ((after-success ()
                        (let ((user-id (plist-get user :id)))
                          (slack-log (format "User %s Joind Team: %s"
                                             (slack-user-name user-id team)
                                             (slack-team-name team))
                                     team
                                     :level 'info))))
      (slack-user-info-request (plist-get user :id)
                               team
                               :after-success #'after-success))))

(defun slack-ws-handle-im-open (payload team)
  (slack-event-update (slack-create-im-open-event payload)
                      team))

(defun slack-ws-handle-close (payload team)
  (slack-event-update (slack-create-room-close-event payload)
                      team))

(defun slack-ws-handle-message (payload team)
  (slack-event-update (slack-create-message-event payload)
                      team))

(defun slack-ws-payload-pong-p (payload)
  (string= "pong" (plist-get payload :type)))

(cl-defmethod slack-ws-remove-from-resend-queue ((ws slack-team-ws) payload team)
  (unless (slack-ws-payload-pong-p payload)
    (with-slots (waiting-send) ws
      (slack-log (format "waiting-send: %s" (length waiting-send))
                 team :level 'trace)
      (setq waiting-send
            (cl-remove-if #'(lambda (e) (eq (plist-get e :id)
                                            (plist-get payload :reply_to)))
                          waiting-send))
      (slack-log (format "waiting-send: %s" (length waiting-send))
                 team :level 'trace))))

(cl-defmethod slack-ws-handle-reply ((ws slack-team-ws) payload team)
  (let ((ok (plist-get payload :ok)))
    (if (eq ok :json-false)
        (let* ((err (plist-get payload :error))
               (code (plist-get err :code))
               (msg (plist-get err :msg))
               (template "Failed to send message. Error code: %s msg: %s"))
          (slack-log (format template code msg) team :level 'error))
      (slack-event-update (slack-create-reply-event payload) team)
      (slack-ws-remove-from-resend-queue ws payload team))))

(defun slack-ws-handle-reaction-added (payload team)
  (slack-if-let* ((event (slack-create-reaction-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-reaction-removed (payload team)
  (slack-if-let* ((event (slack-create-reaction-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-channel-created (payload team)
  (slack-event-update (slack-create-channel-created-event payload)
                      team))

(defun slack-ws-handle-room-archive (payload team)
  (slack-event-update (slack-create-room-archive-event payload)
                      team))

(defun slack-ws-handle-room-unarchive (payload team)
  (slack-event-update (slack-create-room-unarchive-event payload)
                      team))

(defun slack-ws-handle-channel-deleted (payload team)
  (slack-event-update (slack-create-channel-deleted-event payload)
                      team))

(defun slack-ws-handle-room-rename (payload team)
  (slack-event-update (slack-create-room-rename-event payload)
                      team))

(defun slack-ws-handle-group-joined (payload team)
  (slack-event-update (slack-create-group-joined-event payload)
                      team))

(defun slack-ws-handle-channel-joined (payload team)
  (slack-event-update (slack-create-channel-joined-event payload)
                      team))

(defun slack-ws-handle-presence-change (payload team)
  (let ((id (plist-get payload :user))
        (presence (plist-get payload :presence)))
    (puthash id presence (oref team presence))))

(defun slack-ws-handle-bot (payload team)
  (let ((bot (plist-get payload :bot)))
    (slack-team-set-bots team (list bot))))

(defun slack-ws-handle-file-created (payload team)
  (slack-if-let* ((file-id (plist-get (plist-get payload :file) :id))
                  (buffer (slack-buffer-find 'slack-file-list-buffer team)))
      (slack-file-request-info file-id 1 team
                               #'(lambda (file &rest _args)
                                   (slack-buffer-update buffer file)))))

(defun slack-ws-handle-file-deleted (payload team)
  (let ((file-id (plist-get payload :file_id)))
    (remhash file-id (oref team files))))

(defun slack-ws-handle-room-marked (payload team)
  (slack-event-update (slack-create-room-marked-event payload)
                      team))

(defun slack-ws-handle-thread-marked (payload team)
  (let* ((type (plist-get payload :type)))
    (slack-counts-update team)
    (when (string= type "thread")
      (slack-event-update (slack-create-thread-marked-event payload)
                          team))))

(defun slack-ws-handle-thread-subscribed (payload team)
  (slack-event-update (slack-create-thread-subscribed-event payload)
                      team))

(defun slack-ws-handle-thread-unsubscribed (payload team)
  (slack-event-update (slack-create-thread-unsubscribed-event payload)
                      team))

(defun slack-ws-handle-user-change (payload team)
  (let ((user (plist-get payload :user)))
    (slack-team-set-users team (list user))))

(defun slack-ws-handle-member-joined-channel (payload team)
  (slack-event-update (slack-create-member-joined-room-event payload)
                      team))

(defun slack-ws-handle-member-left_channel (payload team)
  (slack-event-update (slack-create-member-left-room-event payload)
                      team))

(defun slack-ws-handle-dnd-updated (payload team)
  (let* ((user (plist-get payload :user))
         (payload (plist-get payload :dnd_status))
         (status (slack-create-dnd-status payload)))
    (puthash user status (oref team dnd-status))))

(defun slack-ws-handle-star-added (payload team)
  (slack-if-let* ((event (slack-create-star-event payload)))
      (slack-event-update event team)))

(defun slack-ws-handle-star-removed (payload team)
  (slack-if-let* ((event (slack-create-star-event payload)))
      (slack-event-update event team)))

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
        (oset room is-member nil)
        (slack-log (format "You left %s" (slack-room-name room team))
                   team :level 'info))))

(defun slack-ws-handle-subteam-created (payload team)
  (let ((usergroup (slack-usergroup-create (plist-get payload :subteam))))
    (push usergroup (oref team usergroups))))

(defun slack-ws-handle-subteam-updated (payload team)
  (let ((usergroup (slack-usergroup-create (plist-get payload :subteam))))
    (oset team usergroups (cons usergroup
                                (cl-remove-if #'(lambda (e)
                                                  (string= (oref e id)
                                                           (oref usergroup id)))
                                              (oref team usergroups))))))

(cl-defmethod slack-team-send-message ((this slack-team) message)
  (if (or (slack-ws-payload-ping-p message)
          (slack-ws-payload-presence-sub-p message))
      (progn
        (slack-team-inc-message-id this)
        (with-slots (ws) this
          (slack-ws-send ws message this)))))

(cl-defmethod slack-team-open-ws ((this slack-team) &key on-open ws-url)
  (with-slots (ws) this
    (slack-ws-open ws this
                   :on-open on-open
                   :ws-url ws-url)))

(cl-defmethod slack-team-disconnect ((team slack-team))
  (slack-ws--close (oref team ws) team))

(defun slack-team-delete ()
  (interactive)
  (let ((selected (slack-team-select t t)))
    (if (yes-or-no-p (format "Delete %s from `slack-teams'?"
                             (oref selected name)))
        (progn
          (slack-team--delete selected)
          (slack-team-disconnect selected)
          (message "Delete %s from `slack-teams'" (oref selected name))))))

(cl-defmethod slack-team-send-presence-sub ((this slack-team))
  (let ((type "presence_sub")
        (ids (let ((result))
               (cl-loop for im in (slack-team-ims this)
                        do (when (slack-room-open-p im)
                             (push (oref im user) result)))
               result)))
    (slack-team-send-message this
                             (list :id (oref this message-id)
                                   :type type
                                   :ids ids))))
(defun slack-authorize (team &optional error-callback success-callback)
  (let ((authorize-request (oref team authorize-request)))
    (if (and authorize-request (not (request-response-done-p authorize-request)))
        (slack-log "Authorize Already Requested" team)
      (cl-labels
          ((on-error (&key error-thrown symbol-status response data)
                     (oset team authorize-request nil)
                     (slack-log (format "Authorize Failed: %s" error-thrown)
                                team)
                     (when (functionp error-callback)
                       (funcall error-callback
                                :error-thrown error-thrown
                                :symbol-status symbol-status
                                :response response
                                :data data)))
           (on-success (&key data &allow-other-keys)
                       (oset team authorize-request nil)
                       (slack-request-handle-error
                        (data "slack-authorize")
                        (slack-log "Authorization Finished" team)
                        (if success-callback
                            (funcall success-callback data)
                          (cl-labels
                              ((on-emoji-download (paths)
                                                  (oset team
                                                        emoji-download-watch-timer
                                                        (run-with-idle-timer 5 t
                                                                             #'slack-team-watch-emoji-download-complete
                                                                             team paths)))
                               (on-open ()
                                        (slack-conversations-list-update team)
                                        ;; (slack-user-list-update team)
                                        (slack-dnd-status-team-info team)
                                        (slack-download-emoji team #'on-emoji-download)
                                        (slack-command-list-update team)
                                        (slack-usergroup-list-update team)
                                        (slack-update-modeline)))
                            (let ((self (plist-get data :self))
                                  (team-data (plist-get data :team)))
                              (oset team id (plist-get team-data :id))
                              (oset team name (plist-get team-data :name))
                              (oset team self self)
                              (oset team self-id (plist-get self :id))
                              (oset team self-name (plist-get self :name))
                              (slack-team-set-ws-url team (plist-get data :url))
                              (oset team domain (plist-get team-data :domain)))
                            (puthash (oref team id) (oref team token) slack-tokens-by-id)
                            (slack-team-open-ws team :on-open #'on-open))))))
        (let ((request (slack-request
                        (slack-request-create
                         slack-rtm-connect-url
                         team
                         :params (list (cons "mpim_aware" "1")
                                       (cons "presence_sub" "true"))
                         :success #'on-success
                         :error #'on-error
                         :no-retry t))))
          (oset team authorize-request request))))))

(defalias 'slack-room-list-update 'slack-conversations-list-update)
(defun slack-conversations-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (channels groups ims)
                  (slack-team-set-channels team channels)
                  (slack-team-set-groups team groups)
                  (slack-team-set-ims team ims)
                  (slack-counts-update team)
                  (slack-user-info-request
                   (mapcar #'(lambda (im) (oref im user))
                           (slack-team-ims team))
                   team)
                  (slack-team-send-presence-sub team)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (slack-log "Slack Channel List Updated"
                             team :level 'info)
                  (slack-log "Slack Group List Updated"
                             team :level 'info)
                  (slack-log "Slack Im List Updated"
                             team :level 'info)))
      (slack-conversations-list team #'success))))

(defun slack-im-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (_channels _groups ims)
                  (slack-team-set-ims team ims)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (slack-log "Slack Im List Updated"
                             team :level 'info)
                  (slack-team-send-presence-sub team)))
      (slack-conversations-list team #'success (list "im")))))

(provide 'slack-websocket)
;;; slack-websocket.el ends here
