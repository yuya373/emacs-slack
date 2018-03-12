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
  (slack-if-let* ((room-name (slack-room-name room)))
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
                :params (list (cons "ts" (oref message ts))
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

(defmethod slack-buffer-toggle-email-expand ((this slack-room-buffer) ts)
  (with-slots (room) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (progn
          (with-slots (file) message
            (oset file is-expanded (not (oref file is-expanded))))
          (slack-buffer-update this message :replace t)))))

(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
