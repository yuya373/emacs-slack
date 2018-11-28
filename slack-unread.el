;;; slack-unread.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@archlinux>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'slack-util)
(require 'slack-request)
(require 'slack-room)
(require 'slack-message-buffer)


(defvar slack-completing-read-function)
(defconst slack-unread-history-url "https://slack.com/api/unread.history")
(defconst slack-unread-collapse-url "https://slack.com/api/unread.collapse")
(defconst slack-unread-expand-url "https://slack.com/api/unread.expand")

(cl-defun slack-unread-history (team after-success &key (sync nil) (on-error))
  (let ((timestamp (number-to-string (time-to-seconds (current-time))))
        (sort "newest"))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-unread-history" (when (functionp on-error)
                                                  #'(lambda (err)
                                                      (funcall on-error err team))))
                   (let ((channels-count
                          (plist-get data :channels_count))
                         (total-messages-count
                          (plist-get data :total_messages_count))
                         (channels (plist-get data :channels)))
                     (funcall after-success
                              channels-count
                              total-messages-count
                              channels)))))
      (slack-request
       (slack-request-create
        slack-unread-history-url
        team
        :type "POST"
        :params (list (cons "timestamp" timestamp)
                      (cons "sort" sort))
        :success #'success
        :sync sync)))))

(defun slack-unread-select-rooms (rooms team)
  (let* ((candidates (mapcar #'(lambda (e)
                                 (cl-destructuring-bind (id total-unreads collapsed) e
                                   (let ((room (slack-room-find id team)))
                                     (cons (format "%s (%s)"
                                                   (slack-room-name room team)
                                                   total-unreads)
                                           (list room collapsed)))))
                             rooms))
         (selected (funcall slack-completing-read-function
                            "Select Channel: "
                            candidates
                            nil t)))
    (cl-destructuring-bind (room collapsed) (cdr (cl-assoc selected candidates :test #'string=))
      (if collapsed
          (slack-room-display room team)
        (slack-unread-collapse room team)))))

(defun slack-all-unreads ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels
        ((display-channel
          (channels)
          (let ((channels (mapcar #'(lambda (e)
                                      (list (plist-get e :channel_id)
                                            (plist-get e :total_unreads)
                                            (plist-get e :collapsed)))
                                  channels)))
            (run-at-time 0 nil #'slack-unread-select-rooms
                         channels team)))
         (success (channels-count _total-messages-count channels)
                  (if (< 0 channels-count)
                      (display-channel channels)
                    (slack-log "No unread messages"
                               team
                               :level 'info))))
      (slack-unread-history team #'success))))

(defun slack-unread-expand (room team)
  (let ((channel (oref room id)))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-unread-expand")
                   (slack-log (format "Expand %s"
                                      (slack-room-name room team))
                              team :level 'info))))
      (slack-request
       (slack-request-create
        slack-unread-expand-url
        team
        :type "POST"
        :params (list (cons "channel" channel))
        :success #'success)))))

(defun slack-unread-collapse (room team)
  (let ((channel (oref room id)))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-unread-collapse")
                   (slack-log (format "Collapse %s"
                                      (slack-room-name room team))
                              team :level 'info))))
      (slack-request
       (slack-request-create
        slack-unread-collapse-url
        team
        :type "POST"
        :params (list (cons "channel" channel))
        :success #'success)))))

(provide 'slack-unread)
;;; slack-unread.el ends here
