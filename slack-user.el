;;; slack-user.el ---slack user interface            -*- lexical-binding: t; -*-

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

(require 'slack-request)
(require 'slack-room)

(defun slack-user-find (id team)
  (with-slots (users) team
    (cl-find-if (lambda (user)
                  (string= id (plist-get user :id)))
                users)))

(defun slack-user-find-by-name (name team)
  (with-slots (users) team
    (cl-find-if (lambda (user)
                  (string= name (plist-get user :name)))
                users)))

(defun slack-user-get-id (name team)
  (let ((user (slack-user-find-by-name name team)))
    (if user
        (plist-get user :id))))

(defun slack-user-name (id team)
  (let ((user (slack-user-find id team)))
    (if user
        (plist-get user :name))))

(defun slack-user-names (team)
  (with-slots (users) team
    (mapcar (lambda (u) (cons (plist-get u :name) u))
            users)))

(defun slack-user-presence-to-string (user)
  (if (string= (plist-get user :presence) "active")
      "* "
    "  "))

(provide 'slack-user)
;;; slack-user.el ends here
