;;; slack-im.el ---slack direct message interface    -*- lexical-binding: t; -*-

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

(require 'eieio)
(require 'slack-util)
(require 'slack-room)
(require 'slack-buffer)
(require 'slack-user)

(defvar slack-buffer-function)

(defconst slack-im-history-url "https://slack.com/api/im.history")
(defconst slack-im-buffer-name "*Slack - Direct Messages*")
(defconst slack-user-list-url "https://slack.com/api/users.list")
(defconst slack-im-list-url "https://slack.com/api/im.list")

(defclass slack-im (slack-room)
  ((user :initarg :user)))

(defun slack-im-create (payload)
  (apply #'slack-im "im"
         (slack-collect-slots 'slack-im payload)))

(defmethod slack-im-user-presence ((room slack-im) team)
  (with-slots ((user-id user)) room
    (let ((user (slack-user-find user-id team)))
      (slack-user-presence-to-string user))))

(defmethod slack-room-name ((room slack-im) team)
  (with-slots (user) room
    (slack-user-name user team)))

(defun slack-im-user-name (im team)
  (with-slots (user) im
    (slack-user-name user team)))

(defun slack-im-names (team)
  (with-slots (ims) team
    (mapcar #'(lambda (im) (cons (slack-im-user-name im team) im))
            ims)))

(defmethod slack-room-buffer-name ((room slack-im) team)
  (let ((user-name (slack-user-name (oref room user) team)))
    (concat slack-im-buffer-name
            " : "
            (slack-team-name team)
            " : " user-name)))

(defun slack-im-select ()
  (interactive)
  (let ((team (slack-team-select)))
    (with-slots (ims) team
      (slack-room-select ims team))))

(defun slack-user-equal-p (a b)
  (string= (plist-get a :id) (plist-get b :id)))

(defun slack-user-pushnew (user team)
  (with-slots (users) team
    (cl-pushnew user users :test #'slack-user-equal-p)))

(defun slack-im-update-room-list (users team)
  (cl-labels ((on-update-room-list
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-im-update-room-list")
                (mapc #'(lambda (u) (slack-user-pushnew u team))
                      (append users nil))
                (oset team ims (mapcar #'slack-im-create
                                        (plist-get data :ims)))
                (message "Slack Im List Updated"))))
    (slack-room-list-update slack-im-list-url
                            #'on-update-room-list
                            team
                            :sync nil)))

(defun slack-im-list-update ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-request
     slack-user-list-url
     team
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (slack-request-handle-error (data "slack-im-list-update")
                              (let ((users (plist-get data :members)))
                                (slack-im-update-room-list users team)))))
     :sync nil)))

(defconst slack-im-update-mark-url "https://slack.com/api/im.mark")

(defmethod slack-room-update-mark-url ((_room slack-im))
  slack-im-update-mark-url)

(defmethod slack-room-history-url ((_room slack-im))
  slack-im-history-url)

(provide 'slack-im)
;;; slack-im.el ends here
