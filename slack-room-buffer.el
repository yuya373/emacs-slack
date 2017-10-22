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
(require 'slack-buffer)

(defclass slack-room-buffer (slack-buffer)
  ((room :initarg :room :type slack-room)))

(defmethod slack-buffer-name :static ((class slack-room-buffer) room team)
  (if-let* ((room-name (slack-room-name room)))
      (format  "*Slack - %s : %s"
               (oref team name)
               room-name)))

(defmethod slack-buffer-name ((this slack-room-buffer))
  (with-slots (room team) this
    (slack-buffer-name (eieio-object-class-name this) room team)))

(defmethod slack-buffer-display-file ((this slack-room-buffer) file-id)
  (with-slots (team) this
    (cl-labels
        ((open (file _)
               (slack-buffer-display
                (slack-create-file-info-buffer team file))))
      (slack-file-request-info file-id 1 team #'open))))

(defmethod slack-buffer-delete-message ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (if-let* ((message (slack-room-find-message room ts)))
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

(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
