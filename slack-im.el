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
(require 'slack-request)

(defvar slack-buffer-function)

(defconst slack-im-history-url "https://slack.com/api/im.history")
(defconst slack-im-buffer-name "*Slack - Direct Messages*")
(defconst slack-user-list-url "https://slack.com/api/users.list")
(defconst slack-im-list-url "https://slack.com/api/im.list")
(defconst slack-im-close-url "https://slack.com/api/im.close")
(defconst slack-im-open-url "https://slack.com/api/im.open")
(defconst slack-im-update-mark-url "https://slack.com/api/im.mark")

(defclass slack-im (slack-room)
  ((user :initarg :user :initform "")
   (is-open :initarg :is_open :initform t)
   (is-user-deleted :initarg :is_user_deleted :initform nil)))

(defmethod slack-merge ((this slack-im) other)
  (call-next-method)
  (with-slots (user is-open) this
    (setq user (oref other user))
    (setq is-open (oref other is-open))))

(defmethod slack-room-open-p ((room slack-im))
  (oref room is-open)
  (not (oref room is-user-deleted)))

(defmethod slack-im-user-presence ((room slack-im))
  (with-slots (team-id) room
    (let ((team (slack-team-find team-id)))
      (slack-user-presence-to-string (slack-user-find room team)))))

(defmethod slack-im-user-dnd-status ((room slack-im))
  (with-slots (team-id) room
    (slack-user-dnd-status-to-string (slack-user-find room
                                                      (slack-team-find team-id)))))

(defmethod slack-room-name ((room slack-im))
  (with-slots (user team-id) room
    (slack-user-name user (slack-team-find team-id))))

(defmethod slack-room-display-name ((room slack-im))
  "To Display emoji in minibuffer configure `emojify-inhibit-in-buffer-functions'"
  (let* ((team (slack-team-find (oref room team-id)))
         (status (slack-user-status (oref room user) team))
         (room-name (or (and status
                             (format "%s %s"
                                     (slack-room-name room)
                                     status))
                        (slack-room-name room))))
    (if slack-display-team-name
        (format "%s - %s"
                (oref (slack-room-team room) name)
                room-name)
      room-name)))

(defun slack-im-user-name (im team)
  (with-slots (user) im
    (slack-user-name user team)))

(defun slack-im-names (team)
  (with-slots (ims) team
    (slack-room-names ims
                      #'(lambda (ims)
                          (cl-remove-if #'(lambda (im) (not (oref im is-open)))
                                        ims)))))

(defmethod slack-room-buffer-name ((room slack-im))
  (concat slack-im-buffer-name
          " : "
          (slack-room-display-name room)))

(defun slack-im-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (candidates (cl-loop for team in (list team)
                              for ims = (cl-remove-if #'(lambda (im)
                                                          (not (oref im is-open)))
                                                      (oref team ims))
                              nconc ims))
         (room (slack-room-select candidates)))
    (slack-room-display room team)))

(defun slack-user-equal-p (a b)
  (string= (plist-get a :id) (plist-get b :id)))

(defun slack-user-pushnew (user team)
  (with-slots (users) team
    (cl-pushnew user users :test #'slack-user-equal-p)))

(defun slack-im-update-room-list (users team &optional after-success)
  (cl-labels ((on-update-room-list
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-im-update-room-list")
                (mapc #'(lambda (u) (slack-user-pushnew u team))
                      (append users nil))
                (slack-merge-list (oref team ims)
                                  (mapcar #'(lambda (d)
                                              (slack-room-create d team 'slack-im))
                                          (plist-get data :ims)))
                (if after-success
                    (funcall after-success team))
                (slack-request-dnd-team-info team)
                (mapc #'(lambda (room)
                          (slack-room-info-request room team))
                      (oref team ims))
                (slack-log "Slack Im List Updated" team :level 'info))))
    (slack-room-list-update slack-im-list-url
                            #'on-update-room-list
                            team
                            :sync nil)))

(defun slack-im-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((on-list-update
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-im-list-update")
           (let* ((members (append (plist-get data :members) nil)))
             (slack-im-update-room-list members team after-success)))))
      (slack-request
       (slack-request-create
        slack-user-list-url
        team
        :success #'on-list-update)))))

(defmethod slack-room-update-mark-url ((_room slack-im))
  slack-im-update-mark-url)

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
           (slack-request-create
            slack-im-close-url
            team
            :type "POST"
            :params (list (cons "channel" (oref selected id)))
            :success #'on-success))))))

(defun slack-im-open (&optional user after-success)
  (interactive)
  (let* ((team (slack-team-select))
         (user (or user (slack-select-from-list
                            ((slack-user-name-alist
                              team
                              :filter #'(lambda (users) (cl-remove-if #'slack-user-hidden-p users)))
                             "Select User: ")))))
    (cl-labels
        ((on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-im-open")
           (if (plist-get data :already_open)
               (let ((im (slack-room-find (plist-get (plist-get data :channel) :id) team)))
                 (oset im is-open t)
                 (message "Direct Message Channel with %s Already Open"
                          (slack-user-name (oref im user) team))))
           (when (functionp after-success)
             (funcall after-success)))))
      (slack-request
       (slack-request-create
        slack-im-open-url
        team
        :type "POST"
        :params (list (cons "user" (plist-get user :id)))
        :success #'on-success)))))

(defmethod slack-room-label-prefix ((room slack-im))
  (format "%s "
          (or
           (slack-im-user-dnd-status room)
           (slack-im-user-presence room))))

(defmethod slack-room-get-info-url ((_room slack-im))
  slack-im-open-url)

(defmethod slack-room-update-info ((room slack-im) data team)
  (let ((new-room (slack-room-create (plist-get data :channel)
                                     team
                                     'slack-im)))

    (slack-merge room new-room)))

(defmethod slack-room-info-request-params ((room slack-im))
  (list (cons "user" (oref room user))
        (cons "return_im" "true")))

(defmethod slack-room-get-members ((room slack-im))
  (list (oref room user)))

(defun slack-im-find-by-user-id (user-id team)
  (cl-find-if #'(lambda (im) (string= user-id (oref im user)))
              (oref team ims)))

(defmethod slack-room-history-url ((_room slack-im))
  slack-im-history-url)

(defmethod slack-room-replies-url ((_room slack-im))
  "https://slack.com/api/im.replies")

(provide 'slack-im)
;;; slack-im.el ends here
