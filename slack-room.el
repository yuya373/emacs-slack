;;; slack-room.el ---slack generic room interface    -*- lexical-binding: t; -*-

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

(defclass slack-room ()
  ((id :initarg :id)
   (created :initarg :created)
   (has-pins :initarg :has_pins)
   (is-open :initarg :is_open)
   (last-read :initarg :last_read)
   (latest :initarg :latest :initform nil)
   (unread-count :initarg :unread_count)
   (unread-count-display :initarg :unread_count_display)
   (messages :initarg :messages :initform ())))

(defun slack-room-find (id)
  (cond
   ((string-prefix-p "G" id) (slack-group-find id))
   ((string-prefix-p "D" id) (slack-im-find id))))

(defmethod slack-room-subscribedp ((room slack-room))
  nil)

(defmethod slack-room-set-messages ((room slack-room) messages)
  (oset room messages messages))

(defun slack-room-on-history (data room)
  (unless (plist-get data :ok)
    (error "%s" data))
  (let* ((datum (plist-get data :messages))
         (messages (slack-message-create-with-room
                      datum room)))
    (slack-room-set-messages room messages)))

(cl-defmacro slack-room-request-update (room-id url success)
  `(slack-request
    ,url
    :params (list (cons "token" ,slack-token)
                   (cons "channel" ,room-id))
    :success ,success))

(cl-defmacro slack-room-make-buffer (name func &key test (update nil))
  (let ((room (gensym)))
    `(let ((,room (cdr (cl-assoc ,name (funcall ,func)
                                 :test ,test))))
       (with-slots (messages latest) ,room
         (if (or ,update (not messages))
             (slack-room-history ,room))
         (funcall slack-buffer-function
                  (slack-buffer-create
                   (slack-room-buffer-name ,room)
                   ,room
                   (slack-room-buffer-header ,room)
                   messages))))))

(cl-defmacro slack-room-select-from-list ((prompt list) &body body)
  "Bind selected from `slack-room-read-list' to selected."
  `(let ((selected (slack-room-read-list ,prompt ,list)))
     ,@body))

(defun slack-room-read-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s" prompt)
                     choices nil t nil nil choices)))

(defmethod slack-room-update-messages ((room slack-room) m)
  (with-slots (messages) room
    (cl-pushnew m messages :test #'slack-message-equal)
    (oset room latest (oref m ts))))

(provide 'slack-room)
;;; slack-room.el ends here
