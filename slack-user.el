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

(defvar slack-users)
(defvar slack-user-name nil)

(defun slack-user-find (id)
  (cl-find-if (lambda (user)
             (string= id (plist-get user :id)))
           slack-users))

(defun slack-user-find-by-name (name)
  (cl-find-if (lambda (user)
             (string= name (plist-get user :name)))
           slack-users))

(defun slack-user-name (id)
  (let ((user (slack-user-find id)))
    (if user
        (plist-get user :name))))

(defun slack-my-user-id ()
  (plist-get (slack-user-find-by-name slack-user-name)
             :id))

(defun slack-user-names ()
  (mapcar (lambda (u) (plist-get u :name))
          slack-users))

(provide 'slack-user)
;;; slack-user.el ends here
