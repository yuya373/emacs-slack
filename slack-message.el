;;; slack-message.el --- slack-message                -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'subr-x)
(require 'slack-util)
(require 'slack-reaction)
(require 'slack-request)
(require 'slack-team)
(require 'slack-block)
(require 'slack-unescape)
(require 'slack-message-faces)

(defvar slack-current-buffer)

(defcustom slack-message-custom-delete-notifier nil
  "Custom notification function for deleted message.\ntake 3 Arguments.\n(lambda (MESSAGE ROOM TEAM) ...)."
  :type 'function
  :group 'slack)

(defconst slack-message-pins-add-url "https://slack.com/api/pins.add")
(defconst slack-message-pins-remove-url "https://slack.com/api/pins.remove")
(defconst slack-message-stars-add-url "https://slack.com/api/stars.add")
(defconst slack-message-stars-remove-url "https://slack.com/api/stars.remove")

(defclass slack-message ()
  ((type :initarg :type :type string)
   (subtype :initarg :subtype)
   (channel :initarg :channel :initform nil)
   (ts :initarg :ts :type string :initform "")
   (text :initarg :text :type (or null string) :initform nil)
   (attachments :initarg :attachments :type (or null list) :initform nil)
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean :initform nil)
   (pinned-to :initarg :pinned_to :type (or null list))
   (deleted-at :initarg :deleted-at :initform nil)
   (hidden :initarg :hidden :initform nil)
   (files :initarg :files :initform '())
   (edited :initarg :edited :initform nil)
   (is-ephemeral :initarg :is_ephemeral :initform nil)
   (blocks :initarg :blocks :type (or null list) :initform nil)
   ;; thread
   (thread-ts :initarg :thread_ts :initform nil)
   (latest-reply :initarg :latest_reply :initform "" :type string)
   (last-read :initarg :last_read :initform "" :type string)
   (replies :initarg :replies :initform nil :type (or null list))
   (reply-count :initarg :reply_count :initform 0 :type number)
   (reply-users :initarg :reply_users :initform '() :type list)
   (reply-users-count :initarg :reply_users_count :initform 0 :type number)
   (subscribed :initarg :subscribed :initform nil :type boolean)
   ;; file comment (Deprecated)
   (comment :initarg :comment :initform nil :type (or null list))
   ))

(defclass slack-message-edited ()
  ((user :initarg :user :type string)
   (ts :initarg :ts :type string)))

(cl-defmethod slack-message-edited-at ((this slack-message))
  (with-slots (edited) this
    (when edited
      (oref edited ts))))

(cl-defmethod slack-message-equal ((m slack-message) n)
  (string= (slack-ts m) (slack-ts n)))

(cl-defmethod slack-message-sender-name ((m slack-message) team)
  (let ((user (or (and (slot-exists-p m 'user)
                       (slot-boundp m 'user)
                       (oref m user))
                  (and (slot-boundp m 'comment)
                       (plist-get (oref m comment) :user)))))
    (if user
        (slack-user-name user team)
      "User Not Found")))

(cl-defmethod slack-message-sender-id ((_this slack-message))
  "")

(cl-defmethod slack-ts ((this slack-message))
  (oref this ts))

(defun slack-ts-to-time (ts)
  (seconds-to-time (string-to-number ts)))

(defun slack-message-time-stamp (message)
  (slack-ts-to-time (slack-ts message)))

(cl-defmethod slack-user-find ((_this slack-message) _team)
  nil)

(cl-defmethod slack-message-star-added ((m slack-message))
  (oset m is-starred t))

(cl-defmethod slack-message-star-removed ((m slack-message))
  (oset m is-starred nil))

(cl-defmethod slack-message-star-api-params ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-reaction-delete ((this slack-message) reaction)
  (with-slots (reactions) this
    (setq reactions (slack-reaction-delete reaction reactions))))

(cl-defmethod slack-reaction-push ((this slack-message) reaction)
  (oset this reactions (append (oref this reactions)
                               (list reaction))))

(cl-defmethod slack-reaction-find ((m slack-message) reaction)
  (slack-reaction--find (oref m reactions) reaction))

(cl-defmethod slack-message-reactions ((this slack-message))
  (oref this reactions))

(cl-defmethod slack-message-get-param-for-reaction ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-message-get-text ((m slack-message) team)
  (or (mapconcat #'identity
                 (cl-remove-if #'(lambda (block-message)
                                   (< (length block-message) 1))
                               (mapcar #'(lambda (bl)
                                           (slack-block-to-mrkdwn bl (list :team team)))
                                       (oref m blocks)))
                 "\n\n")
      (slack-unescape (oref m text) team)))

(cl-defmethod slack-thread-message-p ((this slack-message))
  (and (oref this thread-ts)
       (not (string= (slack-ts this) (oref this thread-ts)))))

(cl-defmethod slack-message-thread-parentp ((m slack-message))
  (let* ((thread-ts (slack-thread-ts m)))
    (when thread-ts
      (string= (slack-ts m) thread-ts))))

(cl-defmethod slack-message-pinned-to-room-p ((this slack-message) room)
  (cl-find (oref room id)
           (oref this pinned-to)
           :test #'string=))

(cl-defmethod slack-message-user-ids ((this slack-message))
  (let ((result (append (oref this reply-users) nil))
        (sender-id (slack-message-sender-id this))
        (texts (append (mapcar #'(lambda (e) (oref e text))
                               (oref this attachments))
                       (list (oref this text)))))
    (unless (slack-string-blankp sender-id)
      (push sender-id result))

    (when (oref this blocks)
      (dolist (bl (oref this blocks))
        (let ((class-name (eieio-object-class-name bl)))
          (when (eq class-name 'slack-section-layout-block)
            (push (oref bl text) texts)
            (dolist (el (oref bl fields))
              (push (oref el text) texts)))

          (when  (eq class-name 'slack-context-layout-block)
            (dolist (el (oref bl elements))
              (when (eq (eieio-object-class-name el)
                        'slack-text-message-composition-object)
                (push (oref el text)
                      texts))))

          (when (eq (eieio-object-class-name bl)
                    'slack-rich-text-block)
            (dolist (el (oref bl elements))
              (when (or (eq (eieio-object-class-name el)
                            'slack-rich-text-section)
                        (eq (eieio-object-class-name el)
                            'slack-rich-text-preformatted)
                        (eq (eieio-object-class-name el)
                            'slack-rich-text-quote)
                        (eq (eieio-object-class-name el)
                            'slack-rich-text-block-element))
                (dolist (el (oref el elements))
                  (when (eq (eieio-object-class-name el)
                            'slack-rich-text-text-element)
                    (push (oref el text) texts))
                  (when (eq (eieio-object-class-name el)
                            'slack-rich-text-user-element)
                    (push (oref el user-id) texts)))))))))
    (dolist (text texts)
      (when text
        (let ((start 0))
          (while (and (< start (length text))
                      (string-match slack-message-user-regexp
                                    text
                                    start))
            (let ((user-id (match-string 1 text)))
              (when user-id
                (push user-id result)))
            (setq start (match-end 0))))))
    result))

(cl-defmethod slack-message-visible-p ((this slack-message) team)
  (if (slack-team-visible-threads-p team)
      t
    (not (slack-thread-message-p this))))

(cl-defmethod slack-thread-ts ((this slack-message))
  (oref this thread-ts))

(cl-defmethod slack-message-handle-thread-subscribed ((this slack-message) payload)
  (oset this subscribed t)
  (oset this last-read (plist-get payload :last_read)))

(cl-defmethod slack-message-ephemeral-p ((this slack-message))
  (oref this is-ephemeral))

(cl-defmethod slack-message-subscribed-thread-message-p ((this slack-message) room)
  (and (slack-thread-message-p this)
       (slack-if-let* ((parent (slack-room-find-message room (slack-thread-ts this))))
           (oref parent subscribed))))

(cl-defmethod slack-message-profile-image ((_this slack-message) _team)
  nil)

(cl-defmethod slack-message-user-status ((_this slack-message) _team)
  "")

(cl-defmethod slack-message-header ((this slack-message) team)
  (let* ((name (slack-message-sender-name this team))
         (status (slack-message-user-status this team))
         (edited-at (slack-format-ts (slack-message-edited-at this)))
         (deleted-at (slack-format-ts (oref this deleted-at))))
    (concat (slack-if-let* ((render-image-p slack-render-image-p)
                            (image (slack-message-profile-image this team)))
                (concat (propertize "image" 'display image 'face 'slack-profile-image-face)
                        " ")
              "")
            (slack-message-put-header-property (concat name
                                                       (if (slack-string-blankp status)
                                                           ""
                                                         (concat " " status))
                                                       (if deleted-at
                                                           (concat " deleted_at: " deleted-at)
                                                         "")
                                                       (if edited-at
                                                           (concat " edited_at: " edited-at)
                                                         "")))
            (if (slack-message-starred-p this)
                " :star:"
              ""))))

(cl-defmethod slack-message-starred-p ((m slack-message))
  (oref m is-starred))

(cl-defmethod slack-message-display-thread-sign-p ((this slack-message) team)
  (and (slack-team-visible-threads-p team)
       (not (null (oref this thread-ts)))
       (not (string= (oref this thread-ts) (slack-ts this)))
       (not (eq major-mode 'slack-thread-message-buffer-mode))))

(cl-defmethod slack-message-body ((m slack-message) team)
  (let ((use-blocks-p (and (not (oref team disable-block-format))
                           (oref m blocks))))
    (if use-blocks-p
        (slack-unescape (mapconcat #'(lambda (bl)
                                       (slack-block-to-string bl (list :team team)))
                                   (oref m blocks)
                                   "\n\n")
                        team)
      (if (oref m text)
          (propertize (slack-unescape (oref m text) team)
                      'face 'slack-message-output-text
                      'slack-text-type 'mrkdwn)
        ""))))

(cl-defmethod slack-room-find ((this slack-message) team)
  (slack-room-find (oref this channel) team))

(cl-defmethod slack-message-replies ((this slack-message) room)
  (slack-if-let* ((ids (oref this replies)))
      (slack-room-sorted-messages room ids)))

(defun slack-message-set-replies (room ts messages &optional append-p)
  (let ((message (slack-room-find-message room ts))
        (replies (mapcar #'(lambda (m) (slack-ts m)) messages)))
    (oset message replies (cl-remove-if #'(lambda (timestamp) (string= ts timestamp))
                                        (if append-p (append (oref message replies) replies)
                                          replies)))))

(provide 'slack-message)
;;; slack-message.el ends here
