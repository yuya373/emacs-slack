;;; slack-user-message.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
(require 'slack-message)

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (id :initarg :id)
   (inviter :initarg :inviter)))

(defclass slack-reply-broadcast-message (slack-user-message) ())

(cl-defmethod slack-message-sender-id ((m slack-user-message))
  (oref m user))

(cl-defmethod slack-thread-message-p ((_this slack-reply-broadcast-message))
  t)

(defvar slack-user-message-keymap
  (let ((keymap (make-sparse-keymap)))
    keymap))

(cl-defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(cl-defmethod slack-message-user-status ((this slack-user-message) team)
  (slack-user-status (slack-message-sender-id this)
                     team))

(cl-defmethod slack-user-find ((this slack-user-message) team)
  (let ((user-id (slack-message-sender-id this)))
    (slack-user--find user-id team)))

(cl-defmethod slack-message-profile-image ((m slack-user-message) team)
  (slack-user-image (slack-user-find m team) team))

(cl-defmethod slack-message-display-thread-sign-p ((this slack-reply-broadcast-message) team)
  nil)

(cl-defmethod slack-message-body ((m slack-reply-broadcast-message) team)
  (let ((s (cl-call-next-method)))
    (unless (slack-string-blankp s)
      (format "%s%s"
              (if (eq major-mode 'slack-thread-message-buffer-mode)
                  ""
                "Replied to a thread: \n")
              s))))

(cl-defmethod slack-message-visible-p ((this slack-reply-broadcast-message) team)
  t)

(provide 'slack-user-message)
;;; slack-user-message.el ends here
