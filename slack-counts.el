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

(cl-defmethod slack-counts-summary ((this slack-counts))
  (with-slots (threads channels mpims ims) this
    (cl-labels
        ((counts-summary (counts)
                         (let ((total-count 0)
                               (unreads nil))
                           (cl-loop for count in counts
                                    do (with-slots (has-unreads mention-count) count
                                         (cl-incf total-count mention-count)
                                         (if (and has-unreads
                                                  (null unreads))
                                             (setq unreads t))))
                           (cons unreads total-count))))
      (let ((channel-summary (counts-summary channels))
            (mpim-summary (counts-summary mpims))
            (im-summary (counts-summary ims)))
        (list (cons 'thread (cons (oref threads has-unreads)
                                  (oref threads mention-count)))
              (cons 'channel channel-summary)
              (cons 'mpim mpim-summary)
              (cons 'im im-summary))))))

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

(defun slack-counts-find (conversation-counts id)
  (cl-find-if #'(lambda (count)
                  (string= id (oref count id)))
              conversation-counts))

(defmacro slack-counts-with (counts id &rest found)
  (declare (indent 2) (debug t))
  `(slack-if-let* ((count (slack-counts-find ,counts ,id)))
       (progn
         ,@found)))

(cl-defmethod slack-counts-im-unread-p ((this slack-counts) im)
  (with-slots (ims) this
    (slack-counts-with ims (oref im id)
      (oref count has-unreads))))

(cl-defmethod slack-counts-channel-unread-p ((this slack-counts) channel)
  (with-slots (channels) this
    (slack-counts-with channels (oref channel id)
      (oref count has-unreads))))

(cl-defmethod slack-counts-mpim-unread-p ((this slack-counts) mpim)
  (with-slots (mpims) this
    (slack-counts-with mpims (oref mpim id)
      (oref count has-unreads))))

(cl-defmethod slack-counts-im-mention-count ((this slack-counts) im)
  (with-slots (ims) this
    (or (slack-counts-with ims (oref im id)
          (oref count mention-count))
        0)))

(cl-defmethod slack-counts-channel-mention-count ((this slack-counts) channel)
  (with-slots (channels) this
    (or (slack-counts-with channels (oref channel id)
          (oref count mention-count))
        0)))

(cl-defmethod slack-counts-mpim-mention-count ((this slack-counts) mpim)
  (with-slots (mpims) this
    (or (slack-counts-with mpims (oref mpim id)
          (oref count mention-count))
        0)))

(cl-defmethod slack-counts-im-set-mention-count ((this slack-counts) im value)
  (with-slots (ims) this
    (slack-counts-with ims (oref im id)
      (oset count mention-count value))))

(cl-defmethod slack-counts-channel-set-mention-count ((this slack-counts) channel value)
  (with-slots (channels) this
    (slack-counts-with channels (oref channel id)
      (oset count mention-count value))))

(cl-defmethod slack-counts-mpim-set-mention-count ((this slack-counts) mpim value)
  (with-slots (mpims) this
    (slack-counts-with mpims (oref mpim id)
      (oset count mention-count value))))

(cl-defmethod slack-counts-im-set-has-unreads ((this slack-counts) im value)
  (with-slots (ims) this
    (slack-counts-with ims (oref im id)
      (oset count has-unreads value))))

(cl-defmethod slack-counts-channel-set-has-unreads ((this slack-counts) channel value)
  (with-slots (channels) this
    (slack-counts-with channels (oref channel id)
      (oset count has-unreads value))))

(cl-defmethod slack-counts-mpim-set-has-unreads ((this slack-counts) mpim value)
  (with-slots (mpims) this
    (slack-counts-with mpims (oref mpim id)
      (oset count has-unreads value))))

(cl-defmethod slack-counts-should-update-latest-p ((this slack-counts-conversation) ts)
  (with-slots (latest) this
    (string< latest ts)))

(cl-defmethod slack-counts-conversation-update-latest ((this slack-counts-conversation) ts)
  (when (slack-counts-should-update-latest-p this ts)
        (oset this latest ts)))

(cl-defmethod slack-counts-im-update-latest ((this slack-counts) im ts)
  (with-slots (ims) this
    (slack-counts-with ims (oref im id)
      (slack-counts-conversation-update-latest count ts))))

(cl-defmethod slack-counts-channel-update-latest ((this slack-counts) channel ts)
  (with-slots (channels) this
    (slack-counts-with channels (oref channel id)
      (slack-counts-conversation-update-latest count ts))))

(cl-defmethod slack-counts-mpim-update-latest ((this slack-counts) mpim ts)
  (with-slots (mpims) this
    (slack-counts-with mpims (oref mpim id)
      (slack-counts-conversation-update-latest count ts))))

(cl-defmethod slack-counts-im-latest ((this slack-counts) im)
  (with-slots (ims) this
    (slack-counts-with ims (oref im id)
      (oref count latest))))

(cl-defmethod slack-counts-channel-latest ((this slack-counts) channel)
  (with-slots (channels) this
    (slack-counts-with channels (oref channel id)
      (oref count latest))))

(cl-defmethod slack-counts-mpim-latest ((this slack-counts) mpim)
  (with-slots (mpims) this
    (slack-counts-with mpims (oref mpim id)
      (oref count latest))))

(cl-defmethod slack-counts-update-threads ((this slack-counts) has-unreads mention-count)
  (with-slots (threads) this
    (oset threads has-unreads has-unreads)
    (oset threads mention-count mention-count))
  (slack-update-modeline))

(provide 'slack-counts)
;;; slack-counts.el ends here
