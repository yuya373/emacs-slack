;;; slack-room.el --- slack generic room interface    -*- lexical-binding: t; -*-

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
(require 'slack-request)
(require 'slack-message)

(defvar slack-token)
(defvar slack-current-room)
(defclass slack-room ()
  ((id :initarg :id)
   (created :initarg :created)
   (has-pins :initarg :has_pins)
   (is-open :initarg :is_open)
   (last-read :initarg :last_read)
   (latest :initarg :latest :initform nil)
   (unread-count :initarg :unread_count)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (messages :initarg :messages :initform ())))

(cl-defgeneric slack-room-name (room))
(cl-defgeneric slack-room-history (room))
(cl-defgeneric slack-room-buffer-header (room))

(defmethod slack-room-subscribedp ((_room slack-room))
  nil)

(defmethod slack-room-set-messages ((room slack-room) messages)
  (oset room messages messages))

(defun slack-room-on-history (data room)
  (unless (plist-get data :ok)
    (error "%s" data))
  (cl-labels ((create-message-with-room
               (payload)
               (slack-message-create payload :room room)))
    (let* ((datum (plist-get data :messages))
           (messages (mapcar #'create-message-with-room datum)))
      (slack-room-update-last-read room
                                   (slack-message :ts "0"))
      (slack-room-set-messages room messages))))

(cl-defmacro slack-room-request-update (room-id url success)
  `(slack-request
    ,url
    :params (list (cons "token" ,slack-token)
                   (cons "channel" ,room-id))
    :success ,success))

(cl-defmacro slack-room-make-buffer (name list &key test (update nil))
  (let ((room (cl-gensym)))
    `(let ((,room (cdr (cl-assoc ,name ,list :test ,test))))
       (slack-room-make-buffer-with-room ,room :update ,update))))

(cl-defun slack-room-make-buffer-with-room (room &key update)
  (with-slots (messages latest) room
    (if (or update (< (length messages) 1))
        (slack-room-history room))
    (funcall slack-buffer-function
             (slack-buffer-create room
                                  (slack-room-buffer-header room)
                                  (slack-room-get-messages room)))))

(defun slack-room-get-messages (room)
  (mapcar #'slack-message-to-string (oref room messages)))

(defun slack-room-select (rooms)
  (let* ((list (slack-room-names rooms))
         (candidates (mapcar #'car list)))
    (slack-room-select-from-list
     (candidates "Select Channel: ")
     (slack-room-make-buffer selected
                             list
                             :test #'string=
                             :update nil))))

(cl-defmacro slack-room-select-from-list ((candidates prompt) &body body)
  "Bind selected from `slack-room-read-list' to selected."
  `(let ((selected (slack-room-read-list ,prompt ,candidates)))
     ,@body))

(defun slack-room-read-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s" prompt)
                     choices nil t nil nil choices)))

(defmethod slack-room-update-messages ((room slack-room) m)
  (with-slots (messages) room
    (cl-pushnew m messages :test #'slack-message-equal)
    (oset room latest (oref m ts))))

(cl-defun slack-room-list-update (url success &key (sync t))
  (slack-request
   url
   :params (list (cons "token" slack-token))
   :success success
   :sync sync))

(defun slack-room-update-message ()
  (interactive)
  (unless (and (boundp 'slack-current-room) slack-current-room)
    (error "Call From Slack Room Buffer"))
  (slack-room-history slack-current-room)
  (slack-buffer-create slack-current-room
                       (slack-room-buffer-header slack-current-room)
                       (slack-room-get-messages slack-current-room)))

(defun slack-room-find-message (room ts)
  (cl-find-if #'(lambda (m) (string= ts (oref m ts)))
              (oref room messages)))

(defmethod slack-room-name-with-unread-count ((room slack-room))
  (with-slots (name unread-count-display) room
    (if (< 0 unread-count-display)
        (concat name " (" (number-to-string unread-count-display) ")")
      name)))

(defun slack-room-names (rooms)
  (mapcar #'(lambda (room)
              (cons (slack-room-name-with-unread-count room)
                    room))
          rooms))

(defmethod slack-room-name ((room slack-room))
  (oref room name))

(defmethod slack-room-update-last-read ((room slack-room) msg)
  (with-slots (ts) msg
    (oset room last-read ts)))

(defmethod slack-room-latest-messages ((room slack-room))
  (with-slots (last-read messages) room
    (cdr (cl-remove-if #'(lambda (m)
                           (string< (oref m ts) last-read))
                  (cl-sort (copy-seq messages)
                           #'string<
                           :key #'(lambda (m) (oref m ts)))))))

(defmethod slack-room-inc-unread-count ((room slack-room))
  (incf (oref room unread-count-display)))

(provide 'slack-room)
;;; slack-room.el ends here
