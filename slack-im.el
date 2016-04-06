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
(defconst slack-im-close-url "https://slack.com/api/im.close")
(defconst slack-im-open-url "https://slack.com/api/im.open")
(defconst slack-im-update-mark-url "https://slack.com/api/im.mark")

(defclass slack-im (slack-room)
  ((user :initarg :user)
   (is-open :initarg :is_open :initform nil)))

(defmethod slack-room-open-p ((room slack-im))
  (oref room is-open))

(defmethod slack-room-name-with-team-name ((room slack-im))
  (with-slots (team-id user) room
    (let* ((team (slack-team-find team-id))
           (user-name (slack-user-name user team)))
      (format "%s - %s" (oref team name) user-name))))

(defmethod slack-im-user-presence ((room slack-im))
  (with-slots ((user-id user) team-id) room
    (let* ((team (slack-team-find team-id))
           (user (slack-user-find user-id team)))
      (slack-user-presence-to-string user))))

(defmethod slack-room-name ((room slack-im))
  (with-slots (user team-id) room
    (slack-user-name user (slack-team-find team-id))))

(defun slack-im-user-name (im team)
  (with-slots (user) im
    (slack-user-name user team)))

(defun slack-im-names (team)
  (with-slots (ims) team
    (mapcar #'(lambda (im) (cons (slack-im-user-name im team) im))
            ims)))

(defmethod slack-room-buffer-name ((room slack-im))
  (concat slack-im-buffer-name
          " : "
          (slack-room-name-with-team-name room)))

(defun slack-im-select ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-room-select
     (cl-loop for team in (list team)
              for ims = (cl-remove-if #'(lambda (im) (not (oref im is-open)))
                                      (oref team ims))
              nconc ims))))

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
                (oset team ims
                      (mapcar #'(lambda (d)
                                  (slack-room-create d team 'slack-im))
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

(defmethod slack-room-update-mark-url ((_room slack-im))
  slack-im-update-mark-url)

(defmethod slack-room-history-url ((_room slack-im))
  slack-im-history-url)

(defun slack-im-close ()
  (interactive)
  (let* ((team (slack-team-select))
         (alist (cl-remove-if #'(lambda (im-names)
                                  (not (oref (cdr im-names) is-open)))
                              (slack-im-names team))))
    (slack-select-from-list
     (alist "Select User: ")
     (cl-labels
         ((on-success
           (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-im-close")
            (if (plist-get data :already_closed)
                (let ((im (slack-room-find (oref selected id) team)))
                  (oset im is-open nil)
                  (message "Direct Message Channel with %s Already Closed"
                           (slack-user-name (oref im user) team)))))))
       (slack-request
        slack-im-close-url
        team
        :type "POST"
        :params (list (cons "channel" (oref selected id)))
        :success #'on-success
        :sync nil)))))

(defun slack-im-open ()
  (interactive)
  (let* ((team (slack-team-select))
         (alist (cl-remove-if #'(lambda (im-names)
                                  (oref (cdr im-names) is-open))
                              (slack-im-names team))))
    (slack-select-from-list
     (alist "Select User: ")
     (cl-labels
         ((on-success
           (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-im-open")
            (if (plist-get data :already_open)
                (let ((im (slack-room-find (oref selected id) team)))
                  (oset im is-open t)
                  (message "Direct Message Channel with %s Already Open"
                           (slack-user-name (oref im user) team)))))))
       (slack-request
        slack-im-open-url
        team
        :type "POST"
        :params (list (cons "user" (oref selected user)))
        :success #'on-success
        :sync nil)))))

(provide 'slack-im)
;;; slack-im.el ends here
