;;; slack-pinned-item.el ---                         -*- lexical-binding: t; -*-

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
(require 'slack-file)

(defconst slack-room-pins-list-url "https://slack.com/api/pins.list")

(defclass slack-pinned-item ()
  ((message :initarg :message)))

(cl-defmethod slack-message-user-ids ((this slack-pinned-item))
  (with-slots (message) this
    (slack-message-user-ids message)))

(defun slack-pinned-item-create (payload room team)
  (let* ((type (plist-get payload :type))
         (message (cond
                   ((string= type "message")
                    (slack-message-create (plist-get payload :message)
                                          team room))
                   ((string= type "file")
                    (or (slack-file-find (plist-get (plist-get payload
                                                               :file)
                                                    :id)
                                         team)
                        (slack-file-create (plist-get payload
                                                      :file)))))))

    (slack-pinned-item :message message)))

(cl-defmethod slack-ts ((this slack-pinned-item))
  (slack-ts (oref this message)))

(cl-defmethod slack-message-to-string ((this slack-pinned-item) team)
  (with-slots (message) this
    (if (or (slack-file-p message)
            (slack-file-email-p message))
        (slack-message-to-string message (slack-ts message) team)
      (slack-message-to-string message team))))

(defun slack-pins-list (room team after-success)
  (cl-labels
      ((callback (items)
                 (funcall after-success items))
       (success
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-pins-list")
         (let* ((items (mapcar #'(lambda (item)

                                   (slack-pinned-item-create item
                                                             room
                                                             team))
                               (plist-get data :items)))
                (user-ids (slack-team-missing-user-ids
                           team (cl-loop for item in items
                                         nconc (slack-message-user-ids item)))))
           (if (< 0 (length user-ids))
               (slack-users-info-request
                user-ids team
                :after-success #'(lambda () (callback items)))
             (callback items))))))
    (slack-request
     (slack-request-create
      slack-room-pins-list-url
      team
      :params (list (cons "channel" (oref room id)))
      :success #'success))))

(provide 'slack-pinned-item)
;;; slack-pinned-item.el ends here
