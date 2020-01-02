;;; slack-typing.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@archlinux>
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
(require 'slack-buffer)
(require 'slack-team)

(defclass slack-typing ()
  ((room-id :initarg :room-id :initform nil)
   (limit :initarg :limit :initform nil)
   (users :initarg :users :initform nil)))

(defclass slack-typing-user ()
  ((limit :initarg :limit :initform nil)
   (user-name :initarg :user-name :initform nil)))

(defun slack-typing-user-create (user-name limit)
  "Create `slack-typing-user' instance from USER-NAME and LIMIT."
  (make-instance 'slack-typing-user
                 :limit limit
                 :user-name user-name))

(defun slack-typing-create (room limit &rest user-names)
  "Create `slack-typing' instance from ROOM and LIMIT.
If USER-NAMES provided, also create `slack-typing-user' instances."
  (let ((users (mapcar #'(lambda (user-name)
                           (slack-typing-user-create user-name limit))
                       user-names)))
    (make-instance 'slack-typing
                   :room-id (oref room id)
                   :limit limit
                   :users users)))

(cl-defmethod slack-equalp ((this slack-typing-user) other)
  (string= (oref this user-name)
           (oref other user-name)))

(cl-defmethod slack-typing-add-user ((this slack-typing) user limit)
  (let ((new-user (slack-typing-user-create user limit)))
    (oset this users (cons new-user
                           (cl-remove-if #'(lambda (old-user)
                                             (slack-equalp new-user old-user))
                                         (oref this users))))))

(cl-defmethod slack-typing-set-limit ((this slack-typing) limit)
  (oset this limit limit))

(defun slack-typing-display (team-id)
  "Display currentrly typing users according to TEAM and it's `slack-typing' instance."
  (let ((team (slack-team-find team-id)))
    (with-slots (typing typing-timer) team
      (let ((current (float-time)))
        (if (or (null typing)
                (and typing-timer
                     (timerp typing-timer)
                     (< (oref typing limit) current)))
            (progn
              (and typing-timer
                   (cancel-timer typing-timer))
              (setq typing-timer nil)
              (setq typing nil)
              (message ""))
          (with-slots (users room-id) typing
            (slack-if-let* ((room (slack-room-find room-id team))
                            (buf (slack-buffer-find 'slack-message-buffer team room))
                            (show-typing-p (slack-buffer-show-typing-p
                                            (get-buffer (slack-buffer-name buf)))))
                (let ((visible-users (cl-remove-if
                                      #'(lambda (u) (< (oref u limit) current))
                                      users)))
                  (slack-log
                   (format "%s is typing..."
                           (mapconcat #'(lambda (u) (oref u user-name))
                                      visible-users
                                      ", "))
                   team
                   :level 'info)))))))))


(provide 'slack-typing)
;;; slack-typing.el ends here
