;;; slack-thread.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-message)

(defclass slack-thread ()
  ((thread-ts :initarg :thread_ts :initform "")
   (messages :initarg :messages :initform '())
   (has-unreads :initarg :has_unreads :initform nil)
   (mention-count :initarg :mention_count :initform 0)
   (reply-count :initarg :reply_count :initform 0)
   (replies :initarg :replies :initform '())
   (active :initarg :active :initform t)
   ))

(defmethod slack-thread-to-string ((m slack-message) team)
  (with-slots (thread) m
    (if thread
        (progn
          (propertize
           (format "%s reply from %s"
                   (oref thread reply-count)
                   (mapconcat #'identity
                              (cl-remove-duplicates
                               (mapcar #'(lambda (reply)
                                           (slack-user-name
                                            (plist-get reply :user)
                                            team))
                                       (oref thread replies))
                               :test #'string=)
                              " "))
           'face '(:underline t)))
      "")))

(defun slack-thread-create (thread-ts)
  (when thread-ts
    (make-instance 'slack-thread :thread_ts thread-ts)))

(defmethod slack-thread-set-messages ((thread slack-thread) messages)
  (let ((count (length messages)))
    (oset thread messages messages)
    (oset thread reply-count count)
    (oset thread replies (mapcar #'(lambda (m) (list :user (slack-message-get-user-id m)
                                                     :ts (oref m ts)))
                                 messages))))

(defmethod slack-thread-add-message ((thread slack-thread) msg)
  (with-slots (messages) thread
    (cl-pushnew msg messages :test #'slack-message-equal)
    (setq messages (slack-room-sort-messages (copy-sequence messages)))))

(provide 'slack-thread)
;;; slack-thread.el ends here
