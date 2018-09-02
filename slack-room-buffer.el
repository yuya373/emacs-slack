;;; slack-room-buffer.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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

(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-request)

(defconst slack-message-delete-url "https://slack.com/api/chat.delete")
(defconst slack-get-permalink-url "https://slack.com/api/chat.getPermalink")

(defclass slack-room-buffer (slack-buffer)
  ((room :initarg :room :type slack-room)))

(defmethod slack-buffer-name :static ((class slack-room-buffer) room team)
  (slack-if-let* ((room-name (slack-room-name room team)))
      (format  "*Slack - %s : %s"
               (oref team name)
               room-name)))

(defmethod slack-buffer-name ((this slack-room-buffer))
  (with-slots (room team) this
    (slack-buffer-name (eieio-object-class-name this) room team)))

(defmethod slack-buffer-delete-message ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (cl-labels
            ((on-delete
              (&key data &allow-other-keys)
              (slack-request-handle-error
               (data "slack-message-delete"))))
          (if (yes-or-no-p "Are you sure you want to delete this message?")
              (slack-request
               (slack-request-create
                slack-message-delete-url
                team
                :type "POST"
                :params (list (cons "ts" (slack-ts message))
                              (cons "channel" (oref room id)))
                :success #'on-delete))
            (message "Canceled"))))))

(defmethod slack-buffer-message-delete ((this slack-room-buffer) ts)
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (lui-delete #'(lambda () (equal (get-text-property (point) 'ts)
                                      ts))))))

(defmethod slack-buffer-copy-link ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts))
                    (template "https://%s.slack.com/archives/%s/p%s%s"))
        (cl-labels
            ((on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-get-permalink")
                          (let ((permalink (plist-get data :permalink)))
                            (kill-new permalink)
                            (message "Link Copied to Clipboard")))))
          (slack-request
           (slack-request-create
            slack-get-permalink-url
            team
            :type "POST"
            :params (list (cons "channel" (oref room id))
                          (cons "message_ts" ts))
            :success #'on-success))))))

(defmethod slack-buffer--replace ((this slack-room-buffer) ts)
  (with-slots (room) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-buffer-replace this message))))

(defmethod slack-buffer-toggle-email-expand ((this slack-room-buffer) ts file-id)
  (with-slots (room) this
    (slack-if-let* ((message (slack-room-find-message room ts))
                    (file (cl-find-if
                           #'(lambda (e) (string= (oref e id)
                                                  file-id))
                           (oref message files))))
        (progn
          (oset file is-expanded (not (oref file is-expanded)))
          (slack-buffer-update this message :replace t)))))

;; POST
(defconst slack-actions-list-url "https://slack.com/api/apps.actions.list")
;; POST
;; params (action_id, type, app_id, channel, message_ts)
(defconst slack-actions-run-url "https://slack.com/api/apps.actions.run")

(defmethod slack-buffer-execute-message-action ((this slack-room-buffer) ts)
  (with-slots (team room) this
    (cl-labels
        ((select
          (app-actions)
          (let ((choices (mapcan
                          #'(lambda (app)
                              (mapcar #'(lambda (action)
                                          (cons (format "%s - %s"
                                                        (plist-get action :name)
                                                        (plist-get app :app_name))
                                                (cons app action)))
                                      (plist-get app :actions)))
                          app-actions)))
            (cdr (cl-assoc (funcall slack-completing-read-function
                                    "Select Action: " choices)
                           choices :test #'string=))))
         (on-success-run (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-buffer-execute-message-action"
                                #'(lambda (err) (slack-log (format "%s" err)
                                                           team
                                                           :level 'error)))))
         (on-success-list
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-buffer-execute-message-action")
           (slack-if-let*
               ((app-actions (plist-get data :app_actions))
                (selected (select app-actions))
                (params (list (cons "message_ts" ts)
                              (cons "channel" (oref room id))
                              (cons "type" (plist-get (cdr selected)
                                                      :type))
                              (cons "action_id" (plist-get (cdr selected)
                                                           :action_id))
                              (cons "app_id" (plist-get (car selected)
                                                        :app_id))
                              (cons "client_token"
                                    (slack-team-client-token team)))))
               (slack-request
                (slack-request-create
                 slack-actions-run-url
                 team
                 :type "POST"
                 :params params
                 :success #'on-success-run))))))
      (slack-request
       (slack-request-create
        slack-actions-list-url
        team
        :type "POST"
        :success #'on-success-list)))))

(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
