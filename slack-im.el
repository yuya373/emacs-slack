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
(require 'slack-user)
(require 'slack-request)
(require 'slack-conversations)

(defvar slack-buffer-function)
(defvar slack-display-team-name)
(defvar slack-completing-read-function)

(defconst slack-im-update-mark-url "https://slack.com/api/im.mark")

(defclass slack-im (slack-room)
  ((user :initarg :user :initform "")
   (is-open :initarg :is_open :initform t)
   (is-user-deleted :initarg :is_user_deleted :initform nil)))

(cl-defmethod slack-merge ((this slack-im) other)
  (cl-call-next-method)
  (oset this user (oref other user))
  (oset this is-open (oref other is-open))
  (oset this is-user-deleted (oref other is-user-deleted)))

(cl-defmethod slack-room-open-p ((room slack-im))
  (oref room is-open)
  (not (oref room is-user-deleted)))

(cl-defmethod slack-im-user-presence ((room slack-im) team)
  (or (slack-user-presence-to-string (slack-user-find room team)
                                     team)
      " "))

(cl-defmethod slack-im-user-dnd-status ((room slack-im) team)
  (or (slack-user-dnd-status-to-string (slack-user-find room
                                                    team)
                                       team)
      " "))

(cl-defmethod slack-room-name ((room slack-im) team)
  (with-slots (user) room
    (slack-user-name user team)))

(cl-defmethod slack-room-display-name ((room slack-im) team)
  "To Display emoji in minibuffer configure `emojify-inhibit-in-buffer-functions'"
  (let* ((status (slack-user-status (oref room user) team))
         (room-name (or (and status
                             (format "%s %s"
                                     (slack-room-name room team)
                                     status))
                        (slack-room-name room team))))
    (if slack-display-team-name
        (format "%s - %s"
                (slack-team-name team)
                room-name)
      room-name)))

(defun slack-im-user-name (im team)
  (with-slots (user) im
    (slack-user-name user team)))

(defun slack-im-names (team)
  (cl-labels
      ((filter (ims)
               (cl-remove-if #'(lambda (im) (not (oref im is-open)))
                             ims)))
    (slack-room-names (slack-team-ims team)
                      team
                      #'filter)))

(cl-defmethod slack-room-update-mark-url ((_room slack-im))
  slack-im-update-mark-url)

(defun slack-im-open ()
  (interactive)
  (let* ((team (slack-team-select))
         (user (slack-select-from-list
                   ((slack-user-name-alist
                     team
                     :filter #'(lambda (users)
                                 (cl-remove-if #'slack-user-hidden-p users)))
                    "Select User: "))))
    (slack-conversations-open team
                              :user-ids (list (plist-get user :id)))))

(cl-defmethod slack-room-label-prefix ((room slack-im) team)
  (format "%s%s"
          (slack-im-user-dnd-status room team)
          (slack-im-user-presence room team)))

(cl-defmethod slack-room-get-members ((room slack-im))
  (list (oref room user)))

(defun slack-im-find-by-user-id (user-id team)
  (cl-find-if #'(lambda (im) (string= user-id (oref im user)))
              (slack-team-ims team)))

(cl-defmethod slack-room--has-unread-p ((this slack-im) counts)
  (slack-counts-im-unread-p counts this))

(cl-defmethod slack-room-mention-count ((this slack-im) team)
  (with-slots (counts) team
    (if counts
        (slack-counts-im-mention-count counts this)
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-im) count team)
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-im-set-mention-count counts this count)))

(cl-defmethod slack-room-set-has-unreads ((this slack-im) value team)
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-im-set-has-unreads counts this value)))

(cl-defmethod slack-room--update-latest ((this slack-im) counts ts)
  (slack-counts-im-update-latest counts this ts))

(cl-defmethod slack-room--latest ((this slack-im) counts)
  (slack-counts-im-latest counts this))

(provide 'slack-im)
;;; slack-im.el ends here
