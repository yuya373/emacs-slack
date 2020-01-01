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
(require 'slack-user)
(require 'slack-counts)

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
  ((id :initarg :id)
   (created :initarg :created)
   (unread-count :initarg :unread_count :initform 0 :type integer)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (message-ids :initform '() :type list)
   (messages :initform (make-hash-table :test 'equal :size 300))
   (last-read :initarg :last_read :type string :initform "0")))

(cl-defgeneric slack-room-name (room team))
(cl-defgeneric slack-room-update-mark-url (room))

(cl-defmethod slack-equalp ((this slack-room) other)
  (string= (oref this id)
           (oref other id)))

(cl-defmethod slack-merge ((this slack-room) other)
  "except MESSAGES"
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
  (with-slots (messages) room
    (gethash ts messages)))

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

(cl-defmethod slack-room-sorted-messages ((room slack-room) &optional message-ids)
  (with-slots (messages) room
    (let ((ids (or message-ids (oref room message-ids)))
          (ret))
      (cl-loop for id in (reverse ids)
               do (slack-if-let* ((message (gethash id messages)))
                      (push message ret)))
      ret)))

(cl-defmethod slack-room-latest ((this slack-room) team)
  (with-slots (counts) team
    (or (when counts
          (slack-room--latest this counts))
        "0")))

(cl-defmethod slack-room--latest ((this slack-room) counts)
  (slack-counts-channel-latest counts this))

(cl-defmethod slack-room--update-latest ((this slack-room) counts ts)
  (slack-counts-channel-update-latest counts this ts))

(cl-defmethod slack-room-delete-message ((this slack-room) ts)
  (remhash ts (oref this messages))
  (oset this
        message-ids
        (cl-remove-if #'(lambda (e) (string= ts e))
                      (oref this message-ids))))

(cl-defmethod slack-room-push-message ((this slack-room) message team)
  (let ((ts (slack-ts message)))
    (puthash ts message (oref this messages))
    (cl-pushnew ts (oref this message-ids)
                :test #'string=)
    (oset this message-ids
          (cl-sort (oref this message-ids) #'string<))

    (slack-if-let* ((counts (oref team counts)))
        (slack-room--update-latest this counts ts))))

(cl-defmethod slack-room-clear-messages ((room slack-room))
  (oset room messages (make-hash-table :test 'equal :size 300))
  (oset room message-ids '()))

(cl-defmethod slack-room-set-messages ((room slack-room) messages team)
  (cl-loop for m in messages
           do (let ((ts (slack-ts m)))
                (puthash ts m (oref room messages))
                (cl-pushnew ts (oref room message-ids)
                            :test #'string=)))
  (oset room
        message-ids
        (cl-sort (oref room message-ids) #'string<))

  (slack-if-let* ((counts (oref team counts))
                  (latest (car (last (oref room message-ids)))))
      (slack-room--update-latest room counts latest)))

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

(cl-defmethod slack-room-member-p ((_room slack-room)) t)

(cl-defmethod slack-room-archived-p ((_room slack-room)) nil)

(cl-defmethod slack-room-open-p ((_room slack-room)) t)

(cl-defmethod slack-room-equal-p ((room slack-room) other)
  (string= (oref room id) (oref other id)))

(cl-defmethod slack-room-inc-unread-count ((room slack-room))
  (cl-incf (oref room unread-count-display)))

(cl-defmethod slack-user-find ((room slack-room) team)
  (slack-user--find (oref room user) team))

(cl-defmethod slack-room-member-p ((_this slack-room))
  t)

(cl-defmethod slack-room-find ((id string) team)
  (if (and id team)
      (cl-labels ((find-room (room)
                             (string= id (oref room id))))
        (cond
         ((string-prefix-p "Q" id) (cl-find-if #'find-room (oref team search-results)))
         (t
          (or (gethash id (oref team channels))
              (gethash id (oref team groups))
              (gethash id (oref team ims))))))))

(cl-defmethod slack-room-has-unread-p ((this slack-room) team)
  (with-slots (counts) team
    (when counts
      (slack-room--has-unread-p this counts))))

(cl-defmethod slack-room--has-unread-p ((this slack-room) counts)
  (slack-counts-channel-unread-p counts this))

(cl-defmethod slack-mpim-p ((_this slack-room))
  nil)

(cl-defmethod slack-room-members ((_this slack-room))
  nil)

(cl-defmethod slack-room-set-members ((_this slack-room) _members))

(cl-defmethod slack-room-members-loaded-p ((_this slack-room))
  nil)

(cl-defmethod slack-room-members-loaded ((_this slack-room)))

(cl-defmethod slack-team-set-room ((this slack-team) room)
  (cl-case (eieio-object-class-name room)
    (slack-channel (slack-team-set-channels this (list room)))
    (slack-group (slack-team-set-groups this (list room)))
    (slack-im (slack-team-set-ims this (list room)))))

(cl-defmethod slack-team-set-channels ((this slack-team) channels)
  (let ((table (oref this channels)))
    (cl-loop for channel in channels
             do (slack-if-let* ((old (gethash (oref channel id) table)))
                    (slack-merge old channel)
                  (puthash (oref channel id) channel table)))))

(cl-defmethod slack-team-set-groups ((this slack-team) groups)
  (let ((table (oref this groups)))
    (cl-loop for group in groups
             do (slack-if-let* ((old (gethash (oref group id) table)))
                    (slack-merge old group)
                  (puthash (oref group id) group table)))))

(cl-defmethod slack-team-set-ims ((this slack-team) ims)
  (let ((table (oref this ims)))
    (cl-loop for im in ims
             do (slack-if-let* ((old (gethash (oref im id) table)))
                    (slack-merge old im)
                  (puthash (oref im id) im table)))))

(provide 'slack-room)
;;; slack-room.el ends here
