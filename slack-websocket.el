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

(defvar slack-ws-ping-success)
(defvar slack-ws-ping-time)
(defvar slack-ws-ping-id)
(defvar slack-ws nil)
(defvar slack-ws-url nil)
(defvar slack-ws-ping-timer)

(defun slack-ws-open ()
  (unless slack-ws
    (setq slack-ws (websocket-open
                    slack-ws-url
                    :on-message #'slack-ws-on-message))
    (setq slack-ws-ping-timer
          (run-at-time "5 min" 300 #'slack-ws-ping))))

(defun slack-ws-close ()
  (interactive)
  (if slack-ws
      (progn
        (websocket-close slack-ws)
        (setq slack-ws nil)
        (message "Slack Websocket Closed"))
    (message "Slack Websocket is not open")))

(defun slack-ws-send (payload)
  (condition-case e
      (websocket-send-text slack-ws payload)
    ('websocket-closed
     (message "Slack Websocket is Closed. Try to Reconnect")
     (slack-start)
     (websocket-send-text slack-ws payload))))

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
      (cond
       ((string= type "pong")
        (slack-ws-handle-pong decoded-payload))
       ((string= type "hello")
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
        (slack-ws-handle-bot decoded-payload))))))

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
      (if (integerp (plist-get payload :reply_to))
          (slack-message-handle-reply
           (slack-message-create payload))))))

(cl-defmacro slack-ws-handle-reaction ((payload) &body body)
  `(let* ((item (plist-get ,payload :item))
          (room (slack-room-find (plist-get item :channel))))
     (if room
         (let ((msg (slack-room-find-message room (plist-get item :ts))))
           (if msg
               (let* ((r-name (plist-get ,payload :reaction))
                      (r-count 1)
                      (r-users (list (slack-user-find (plist-get ,payload :user))))
                      (reaction (make-instance 'slack-reaction
                                               :name r-name
                                               :count r-count
                                               :users r-users)))

                 ,@body
                 (slack-message-update msg t)))))))

(defun slack-ws-handle-reaction-added (payload)
  (slack-ws-handle-reaction
   (payload)
   (slack-message-append-reaction msg reaction)))

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

(defun slack-ws-handle-pong (payload)
  (let ((pong-time (plist-get payload :time))
        (pong-id (plist-get payload :reply_to)))
    (if (and (string= slack-ws-ping-time pong-time)
             (eq slack-ws-ping-id pong-id))
        (message "Slack Websocket PONG"))))

(defun slack-ws-ping ()
  (let* ((m (list :id slack-message-id
                  :type "ping"
                  :time (format-time-string "%s")))
         (json (json-encode m)))
    (setq slack-ws-ping-id (plist-get m :id)
          slack-ws-ping-time (plist-get m :time))
    (cl-incf slack-message-id)
    (slack-ws-send json)
    (message "Slack Websocket PING")))

(provide 'slack-websocket)
;;; slack-websocket.el ends here
