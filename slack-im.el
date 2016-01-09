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

(defgroup slack-im nil
  "Slack Direct Message."
  :prefix "slack-im-"
  :group 'slack)

(defvar slack-ims)
(defvar slack-users)
(defvar slack-token)
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

(defmethod slack-im-user-presence ((room slack-im))
  (with-slots ((user-id user)) room
    (let ((user (slack-user-find user-id)))
      (slack-user-presence-to-string user))))

(defmethod slack-room-name ((room slack-im))
  (with-slots (user) room
    (slack-user-name user)))

(defun slack-im-user-name (im)
  (with-slots (user) im
    (slack-user-name user)))

(defun slack-im-names ()
  (mapcar #'(lambda (im) (cons (slack-im-user-name im) im))
          slack-ims))

;; choose user-name list and open im
;; (defun slack-im-open (user-name)
;;   (let* ((user (slack-user-find-by-name user-name))
;;          (user-id (gethash "id" user)))
;;     (cl-labels ((on-im-open
;;                  (&key data &allow-other-keys)
;;                  (unless (gethash "ok" data)
;;                    (error "slack-im-open failed"))
;;                  (slack-im-history
;;                   (gethash "id"
;;                            (gethash "channel" data)))))
;;       (slack-request
;;        slack-im-open-url
;;        :params (list (cons "token" slack-token)
;;                      (cons "user" user-id))
;;        :success #'on-im-open))))

(defmethod slack-room-buffer-name ((room slack-im))
  (let ((user-name (slack-user-name (oref room user))))
    (concat slack-im-buffer-name " : " user-name)))

(defmethod slack-room-buffer-header ((_room slack-im))
  (concat "Direct Message: " "\n"))


(defun slack-im-select ()
  (interactive)
  (slack-room-select slack-ims))

(defun slack-user-equal-p (a b)
  (string= (plist-get a :id) (plist-get b :id)))

(defun slack-user-pushnew (user)
  (cl-pushnew user slack-users :test #'slack-user-equal-p))

(defun slack-im-update-room-list (users)
  (cl-labels ((on-update-room-list
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-im-update-room-list")
                (append users nil)
                (mapc #'slack-user-pushnew users)
                (setq slack-ims (mapcar #'slack-im-create
                                        (plist-get data :ims)))
                (message "Slack Im List Updated"))))
    (slack-room-list-update slack-im-list-url
                            #'on-update-room-list
                            :sync nil)))

(cl-defun slack-user-on-list-update (&key data &allow-other-keys)
  (slack-request-handle-error (data "slack-im-list-update")
                              (let ((users (plist-get data :members)))
                                (slack-im-update-room-list users))))


(defun slack-im-list-update ()
  (interactive)
  (slack-request
   slack-user-list-url
   :params (list (cons "token" slack-token))
   :success #'slack-user-on-list-update
   :sync nil))

(defconst slack-im-update-mark-url "https://slack.com/api/im.mark")

(defmethod slack-room-update-mark-url ((_room slack-im))
  slack-im-update-mark-url)

(defmethod slack-room-history-url ((_room slack-im))
  slack-im-history-url)

(provide 'slack-im)
;;; slack-im.el ends here
