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
(require 'lui)
(require 'slack-util)
(require 'slack-request)
(require 'slack-message)
(require 'slack-user)
(require 'slack-counts)
;; (require 'slack-team)
(declare-function slack-team-select "slack-team")
(declare-function slack-team-name "slack-team")
;; (require 'slack-buffer)
(declare-function slack-buffer-room "slack-buffer")

(defface slack-room-unread-face
  '((t (:weight bold)))
  ;; '((t (:box (:line-width 1 :style released-button))))
  "Face used to mark a room as unread when selecting channels."
  :group 'slack)

(defvar slack-buffer-function)
(defvar slack-completing-read-function)
(defvar slack-display-team-name)
(defvar slack-current-buffer)
(defvar slack-buffer-create-on-notify)

(defclass slack-room ()
  ((name :initarg :name :type (or null string) :initform nil)
   (id :initarg :id)
   (created :initarg :created)
   (unread-count :initarg :unread_count :initform 0 :type integer)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (messages :initarg :messages :initform ())
   (last-read :initarg :last_read :type string :initform "0")
   (members :initarg :members :type list :initform '())
   (members-loaded-p :type boolean :initform nil)))


(cl-defgeneric slack-room-name (room team))
(cl-defgeneric slack-room-update-mark-url (room))

(cl-defmethod slack-equalp ((this slack-room) other)
  (string= (oref this id)
           (oref other id)))

(cl-defmethod slack-merge ((this slack-room) other)
  "except MESSAGES"
  (oset this members (oref other members))
  (oset this members-loaded-p (oref other members-loaded-p))
  (oset this name (oref other name))
  (oset this id (oref other id))
  (oset this created (oref other created))
  (oset this unread-count (oref other unread-count))
  (oset this unread-count-display (oref other unread-count-display))
  (unless (string= "0" (oref other last-read))
    (oset this last-read (oref other last-read))))

(defun slack-room-create (payload class)
  (let* ((attributes (slack-collect-slots class payload)))
    (apply #'make-instance class attributes)))

(cl-defmethod slack-room-subscribedp ((_room slack-room) _team)
  nil)

(cl-defmethod slack-room-buffer-name ((room slack-room) team)
  (concat "*Slack*"
          " : "
          (slack-room-display-name room team)))

(cl-defmethod slack-room-hidden-p ((room slack-room))
  (slack-room-hiddenp room))

(defun slack-room-hiddenp (room)
  (or (not (slack-room-member-p room))
      (slack-room-archived-p room)
      (not (slack-room-open-p room))))

(defun slack-room-names (rooms team &optional filter collecter)
  (cl-labels
      ((latest-ts (room)
                  (slack-room-latest room team))
       (sort-rooms (rooms)
                   (nreverse (cl-sort (append rooms nil)
                                      #'string< :key #'latest-ts))))
    (cl-loop for room in (sort-rooms (if filter
                                         (funcall filter rooms)
                                       rooms))
             as label = (slack-room-label room team)
             collect (if (functionp collecter)
                         (funcall collecter label room)
                       (cons label room)))))

(defun slack-room-select (rooms team)
  (let* ((alist (slack-room-names
                 rooms team #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p rs)))))
    (slack-select-from-list (alist "Select Channel: "))))

(defun slack-room-find-message (room ts)
  (cl-find-if #'(lambda (m) (string= ts (slack-ts m)))
              (oref room messages)
              :from-end t))

(defun slack-room-find-thread-parent (room thread-message)
  (slack-room-find-message room (oref thread-message thread-ts)))

(defun slack-room-find-thread (room ts)
  (let ((message (slack-room-find-message room ts)))
    (when message
      (slack-message-thread message room))))

(cl-defmethod slack-room-display-name ((room slack-room) team)
  (let ((room-name (slack-room-name room team)))
    (if slack-display-team-name
        (format "%s - %s"
                (slack-team-name team)
                room-name)
      room-name)))

(cl-defmethod slack-room-label-prefix ((_room slack-room) _team)
  "  ")

(cl-defmethod slack-room-mention-count-display ((room slack-room) team)
  (let ((count (slack-room-mention-count room team)))
    (if (< 0 count) (format "(%s)" count) "")))

(cl-defmethod slack-room-mention-count ((this slack-room) team)
  (with-slots (counts) team
    (if counts
        (slack-counts-channel-mention-count counts this)
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-room) count team)
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-channel-set-mention-count counts
                                              this
                                              count)))

(cl-defmethod slack-room-set-has-unreads ((this slack-room) value team)
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-channel-set-has-unreads counts this value)))

(cl-defmethod slack-room-label ((room slack-room) team)
  (let ((str (format "%s %s%s"
                     (slack-room-label-prefix room team)
                     (slack-room-display-name room team)
                     (slack-room-mention-count-display room team))))
    (if (slack-room-has-unread-p room team)
        (propertize str 'face 'slack-room-unread-face)
      str)))

(cl-defmethod slack-room-name ((room slack-room) _team)
  (oref room name))

(defun slack-room-sort-messages (messages)
  (cl-sort messages #'string< :key #'slack-ts))

(defun slack-room-reject-thread-message (messages)
  (cl-remove-if #'(lambda (m) (and (not (eq (eieio-object-class-name m)
                                            'slack-reply-broadcast-message))
                                   (slack-thread-message-p m)))
                messages))

(cl-defmethod slack-room-sorted-messages ((room slack-room))
  (with-slots (messages) room
    (slack-room-sort-messages (copy-sequence messages))))

(cl-defmethod slack-room-set-prev-messages ((room slack-room) prev-messages team)
  (let ((messages (append (oref room messages) prev-messages)))
    (slack-room-set-messages room messages team)))

(defalias 'slack-room-prepend-messages 'slack-room-set-prev-messages)

(cl-defmethod slack-room-append-messages ((room slack-room) new-messages team)
  (let ((messages (append new-messages (oref room messages))))
    (slack-room-set-messages room messages team)))

(cl-defmethod slack-room-latest ((this slack-room) team)
  (with-slots (counts) team
    (or (when counts
          (slack-room--latest this counts))
        "0")))

(cl-defmethod slack-room--latest ((this slack-room) counts)
  (slack-counts-channel-latest counts this))

(cl-defmethod slack-room-update-latest ((room slack-room) message team)
  (when message
    (with-slots (counts) team
      (when counts
        (slack-room--update-latest room counts (slack-ts message))))))

(cl-defmethod slack-room--update-latest ((this slack-room) counts ts)
  (slack-counts-channel-update-latest counts this ts))

(cl-defmethod slack-room-push-message ((room slack-room) message)
  (with-slots (messages) room
    (setq messages
          (cl-remove-if #'(lambda (n) (slack-message-equal message n))
                        messages))
    (push message messages)))

(cl-defmethod slack-room-set-messages ((room slack-room) messages team)
  (let* ((sorted (slack-room-sort-messages
                  (cl-delete-duplicates messages
                                        :test #'slack-message-equal)))
         (latest (car (last sorted))))
    (oset room messages sorted)
    (slack-room-update-latest room latest team)))

(cl-defmethod slack-room-prev-messages ((room slack-room) from)
  (with-slots (messages) room
    (cl-remove-if #'(lambda (m)
                      (or (string< from (slack-ts m))
                          (string= from (slack-ts m))))
                  (slack-room-sort-messages (copy-sequence messages)))))

(cl-defmethod slack-room-update-mark ((room slack-room) team ts)
  (cl-labels ((on-update-mark (&key data &allow-other-keys)
                              (slack-request-handle-error
                               (data "slack-room-update-mark"))))
    (with-slots (id) room
      (slack-request
       (slack-request-create
        (slack-room-update-mark-url room)
        team
        :type "POST"
        :params (list (cons "channel"  id)
                      (cons "ts"  ts))
        :success #'on-update-mark)))))

(defun slack-current-room-or-select (room-alist-func &optional select)
  (if (and (not select)
           (bound-and-true-p slack-current-buffer)
           (slot-boundp slack-current-buffer 'room))
      (slack-buffer-room slack-current-buffer)
    (let* ((room-alist (if (functionp room-alist-func)
                           (funcall room-alist-func)
                         room-alist-func)))
      (slack-select-from-list
          (room-alist "Select Channel: ")))))

(cl-defmethod slack-room-member-p ((_room slack-room)) t)

(cl-defmethod slack-room-archived-p ((_room slack-room)) nil)

(cl-defmethod slack-room-open-p ((_room slack-room)) t)

(cl-defmethod slack-room-equal-p ((room slack-room) other)
  (string= (oref room id) (oref other id)))

(cl-defmethod slack-room-inc-unread-count ((room slack-room))
  (cl-incf (oref room unread-count-display)))

(cl-defmethod slack-room-get-members ((room slack-room))
  (oref room members))

(cl-defmethod slack-user-find ((room slack-room) team)
  (slack-user--find (oref room user) team))

(cl-defmethod slack-room-member-p ((_this slack-room))
  t)

(cl-defmethod slack-message-thread ((this slack-reply-broadcast-message) room)
  (let ((message (slack-room-find-message room
                                          (or (oref this broadcast-thread-ts)
                                              (oref this thread-ts)))))
    (slack-message-thread message room)))

(defun slack-room-find (id team)
  (if (and id team)
      (cl-labels ((find-room (room)
                             (string= id (oref room id))))
        (cond
         ((string-prefix-p "C" id) (cl-find-if #'find-room
                                               (oref team channels)))
         ((string-prefix-p "G" id) (cl-find-if #'find-room
                                               (oref team groups)))
         ((string-prefix-p "D" id) (cl-find-if #'find-room
                                               (oref team ims)))
         ((string-prefix-p "Q" id) (cl-find-if #'find-room
                                               (oref team search-results)))))))

(cl-defmethod slack-room-has-unread-p ((this slack-room) team)
  (with-slots (counts) team
    (when counts
      (slack-room--has-unread-p this counts))))

(cl-defmethod slack-room--has-unread-p ((this slack-room) counts)
  (slack-counts-channel-unread-p counts this))

(cl-defmethod slack-mpim-p ((_this slack-room))
  nil)

(provide 'slack-room)
;;; slack-room.el ends here
