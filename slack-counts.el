;;; slack-counts.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  南優也

;; Author: 南優也 <yuya373@minamiyuuyanoMacBook-Pro.local>
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
(require 'slack-team)
(require 'slack-request)

(defconst slack-client-counts-url "https://slack.com/api/client.counts")

(defclass slack-counts-base ()
  ((has-unreads :initarg :has_unreads :type boolean :initform nil)
   (mention-count :initarg :mention_count :type number :initform 0)))

(defclass slack-counts-threads (slack-counts-base) ())

(defclass slack-counts-conversation (slack-counts-base)
  ((id :initarg :id :type string)
   (latest :initarg :latest :type string)))

(defclass slack-counts ()
  ((threads :initarg :threads :type slack-counts-threads)
   (channels :initarg :channels :type (or null list) :initform nil) ;; include groups
   (mpims :initarg :mpims :type (or null list) :initform nil)
   (ims :initarg :ims :type (or null list) :initform nil)))

(defun slack-create-counts-threads (payload)
  (make-instance 'slack-counts-threads
                 :has_unreads (eq t (plist-get payload :has_unreads))
                 :mention_count (plist-get payload :mention_count)))

(defun slack-create-counts-conversation (payload)
  (make-instance 'slack-counts-conversation
                 :id (plist-get payload :id)
                 :has_unreads (eq t (plist-get payload :has_unreads))
                 :mention_count (plist-get payload :mention_count)
                 :latest (plist-get payload :latest)))

(defun slack-create-counts (payload)
  (make-instance 'slack-counts
                 :threads (slack-create-counts-threads
                           (plist-get payload :threads))
                 :channels (mapcar #'slack-create-counts-conversation
                                   (plist-get payload :channels))
                 :mpims (mapcar #'slack-create-counts-conversation
                                (plist-get payload :mpims))
                 :ims (mapcar #'slack-create-counts-conversation
                              (plist-get payload :ims))))

(defun slack-client-counts (team after-success)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-client-counts")
                 (let ((counts (slack-create-counts data)))
                   (funcall after-success counts)))))
    (slack-request
     (slack-request-create
      slack-client-counts-url
      team
      :type "POST"
      :params (list (cons "thread_counts_by_channel" "true"))
      :success #'success))))

(cl-defmethod slack-counts-update ((team slack-team))
  (slack-client-counts team
                       #'(lambda (counts) (oset team counts counts))))

(provide 'slack-counts)
;;; slack-counts.el ends here
