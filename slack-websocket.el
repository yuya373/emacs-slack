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
(require 'slack-request)
(require 'slack-message)
(require 'slack-reply)

(defvar slack-ws-ping-time)
(defvar slack-ws-ping-id)
(defvar slack-ws nil)
(defvar slack-ws-url nil)
(defvar slack-ws-ping-timer)
(defvar slack-ws-ping-timeout-timer nil)
(defvar slack-ws-waiting-resend nil)
(defvar slack-last-ping-time nil)
(defcustom slack-ws-reconnect-auto nil
  "If t, reconnect slack websocket server when ping timeout."
  :group 'slack)

(defun slack-ws-open ()
  (unless slack-ws
    (setq slack-ws (websocket-open
                    slack-ws-url
                    :on-message #'slack-ws-on-message))))

(defun slack-ws-close ()
  (interactive)
  (if slack-ws
      (progn
        (websocket-close slack-ws)
        (setq slack-ws nil)
        (slack-ws-cancel-ping-timer)
        (slack-ws-cancel-timeout-timer)
        (message "Slack Websocket Closed"))
    (message "Slack Websocket is not open")))

(defun slack-ws-send (payload)
  (cl-labels ((restart ()
                       (message "Slack Websocket is Closed. Try to Reconnect")
                       (slack-start))
              (delete-from-waiting-list
               (payload)
               (setq slack-ws-waiting-resend
                     (cl-remove-if #'(lambda (p) (string= payload p))
                                   slack-ws-waiting-resend))))
    (push payload slack-ws-waiting-resend)
    (if (websocket-openp slack-ws)
        (condition-case e
            (progn
              (websocket-send-text slack-ws payload)
              (delete-from-waiting-list payload))
          ('websocket-closed (restart)))
      (restart))))

(defun slack-ws-resend ()
  (let ((waiting slack-ws-waiting-resend))
    (setq slack-ws-waiting-resend nil)
    (mapcar #'(lambda (m)
                (slack-ws-send m)
                (sleep-for 1))
            waiting)))

(defun slack-ws-recursive-decode (payload)
  (cl-labels ((decode (e) (if (stringp e)
                              (decode-coding-string e 'utf-8-unix)
                            e))
              (recur (payload acc)
                     (if (and payload (< 0 (length payload)))
                         (let ((h (car payload)))
                           (if (and (not (stringp h)) (or (arrayp h) (listp h)))
                               (recur (cdr payload) (cons (recur (append h nil) ()) acc))
                             (recur (cdr payload) (cons (decode h) acc))))
                       (reverse acc))))
    (recur payload ())))


(defun slack-ws-on-message (_websocket frame)
  ;; (message "%s" (websocket-frame-payload frame))
  (when (websocket-frame-completep frame)
    (let* ((payload (slack-request-parse-payload
                     (websocket-frame-payload frame)))
           (decoded-payload (slack-ws-recursive-decode payload))
           (type (plist-get decoded-payload :type)))
      ;; (message "%s" decoded-payload)
      (cond
       ((string= type "pong")
        (slack-ws-handle-pong decoded-payload))
       ((string= type "hello")
        (setq slack-ws-ping-timer
              (run-at-time "10 sec" 10 #'slack-ws-ping))
        (slack-ws-resend)
        (message "Slack Websocket Is Ready!"))
       ((plist-get decoded-payload :reply_to)
        (slack-ws-handle-reply decoded-payload))
       ((string= type "message")
        (slack-ws-handle-message decoded-payload))
       ((string= type "reaction_added")
        (slack-ws-handle-reaction-added decoded-payload))
       ((string= type "reaction_removed")
        (slack-ws-handle-reaction-removed decoded-payload))
       ((string= type "channel_created")
        (slack-ws-handle-channel-created decoded-payload))
       ((or (string= type "channel_archive")
            (string= type "group_archive"))
        (slack-ws-handle-room-archive decoded-payload))
       ((or (string= type "channel_unarchive")
            (string= type "group_unarchive"))
        (slack-ws-handle-room-unarchive decoded-payload))
       ((string= type "channel_deleted")
        (slack-ws-handle-channel-deleted decoded-payload))
       ((or (string= type "channel_rename")
            (string= type "group_rename"))
        (slack-ws-handle-room-rename decoded-payload))
       ((or (string= type "channel_joined")
            (string= type "group_joined"))
        (slack-ws-handle-room-joined decoded-payload))
       ((string= type "presence_change")
        (slack-ws-handle-presence-change decoded-payload))
       ((or (string= type "bot_added")
            (string= type "bot_changed"))
        (slack-ws-handle-bot decoded-payload))
       ((string= type "file_shared")
        (slack-ws-handle-file-shared decoded-payload))
       ((or (string= type "file_deleted")
            (string= type "file_unshared"))
        (slack-ws-handle-file-deleted decoded-payload))))))

(defun slack-ws-handle-message (payload)
  (let ((m (slack-message-create payload)))
    (if m
        (slack-message-update m))))

(defun slack-ws-handle-reply (payload)
  (let ((ok (plist-get payload :ok)))
    (if (eq ok :json-false)
        (let ((err (plist-get payload :error)))
          (message "Error code: %s msg: %s"
                   (plist-get err :code)
                   (plist-get err :msg)))
      (let ((message-id (plist-get payload :reply_to)))
        (if (integerp message-id)
            (slack-message-handle-reply
             (slack-message-create payload)))))))

(cl-defmacro slack-ws-handle-reaction ((payload) &body body)
  `(let* ((item (plist-get ,payload :item))
          (room (slack-room-find (plist-get item :channel))))
     (if room
         (let ((msg (slack-room-find-message room (plist-get item :ts))))
           (if msg
               (let* ((r-name (plist-get ,payload :reaction))
                      (r-count 1)
                      (r-users (list (plist-get ,payload :user)))
                      (reaction (make-instance 'slack-reaction
                                               :name r-name
                                               :count r-count
                                               :users r-users)))

                 ,@body
                 (slack-message-update msg t t)))))))

(defun slack-ws-handle-reaction-added (payload)
  (slack-ws-handle-reaction
   (payload)
   (slack-message-append-reaction msg reaction)
   (slack-reaction-notify payload)))

(defun slack-ws-handle-reaction-removed (payload)
  (slack-ws-handle-reaction
   (payload)
   (slack-message-pop-reaction msg reaction)))

(defun slack-ws-handle-channel-created (payload)
  (let ((id (plist-get (plist-get payload :channel) :id)))
    (slack-channel-create-from-info id)))

(defun slack-ws-handle-room-archive (payload)
  (let* ((id (plist-get payload :channel))
         (room (slack-room-find id)))
    (oset room is-archived t)
    (message "Channel: %s is archived"
             (slack-room-name room))))

(defun slack-ws-handle-room-unarchive (payload)
  (let* ((id (plist-get payload :channel))
         (room (slack-room-find id)))
    (oset room is-archived :json-false)
    (message "Channel: %s is unarchived"
             (slack-room-name room))))

(defun slack-ws-handle-channel-deleted (payload)
  (let ((id (plist-get payload :channel)))
    (slack-room-deleted id)))

(defun slack-ws-handle-room-rename (payload)
  (let* ((c (plist-get payload :channel))
         (room (slack-room-find (plist-get c :id)))
         (old-name (slack-room-name room))
         (new-name (plist-get c :name)))
    (oset room name new-name)
    (message "Renamed channel from %s to %s"
             old-name
             new-name)))

(defun slack-ws-handle-room-joined (payload)
  (cl-labels
      ((replace-room (room rooms)
                     (cons room (cl-delete-if
                                 #'(lambda (r)
                                     (slack-room-equal-p room r))
                                 rooms))))
    (let* ((c (plist-get payload :channel)))
      (if (plist-get c :is_channel)
          (let ((channel (slack-channel-create c)))
            (setq slack-channels
                  (replace-room channel slack-channels))
            (message "Joined channel %s"
                     (slack-room-name channel)))
        (let ((group (slack-group-create c)))
          (setq slack-groups
                (replace-room group slack-groups))
          (message "Joined group %s"
                   (slack-room-name group)))))))

(defun slack-ws-handle-presence-change (payload)
  (let* ((id (plist-get payload :user))
         (user (slack-user-find id))
         (presence (plist-get payload :presence)))
    (plist-put user :presence presence)))

(defun slack-ws-handle-bot (payload)
  (let ((bot (plist-get payload :bot)))
    (push bot slack-bots)))

(defun slack-ws-cancel-timeout-timer ()
  (if (timerp slack-ws-ping-timeout-timer)
      (cancel-timer slack-ws-ping-timeout-timer))
  (setq slack-ws-ping-timeout-timer nil))

(defun slack-ws-cancel-ping-timer ()
  (if (timerp slack-ws-ping-timer)
      (cancel-timer slack-ws-ping-timer))
  (setq slack-ws-ping-timer nil))

(defun slack-ws-ping-timeout ()
  (message "Slack Websocket PING Timeout.")
  (slack-ws-cancel-timeout-timer)
  (slack-ws-cancel-ping-timer)
  (if slack-ws-reconnect-auto
      (slack-start)
    (slack-ws-close)))

(defun slack-ws-handle-pong (_payload)
  (slack-ws-cancel-timeout-timer))

(defun slack-ws-ping ()
  (let* ((m (list :id slack-message-id
                  :type "ping"
                  :time (format-time-string "%s")))
         (json (json-encode m)))
    (setq slack-ws-ping-id (plist-get m :id)
          slack-ws-ping-time (plist-get m :time))
    (slack-ws-send json)
    (setq slack-ws-ping-timeout-timer
          (run-at-time "5 sec" nil #'slack-ws-ping-timeout))))

(defun slack-ws-handle-file-shared (payload)
  (let ((file (slack-file-create (plist-get payload :file))))
    (slack-file-pushnew file)))

(defun slack-ws-handle-file-deleted (payload)
  (let ((file-id (plist-get payload :file_id))
        (room (slack-file-room-obj)))
    (with-slots (messages last-read) room
      (setq messages (cl-remove-if #'(lambda (f)
                                       (string= file-id (oref f id)))
                                   messages)))))

(provide 'slack-websocket)
;;; slack-websocket.el ends here
