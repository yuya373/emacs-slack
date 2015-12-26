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
(defvar slack-buffer-function)
(defconst slack-room-pins-list-url "https://slack.com/api/pins.list")

(defclass slack-room ()
  ((id :initarg :id)
   (created :initarg :created)
   (has-pins :initarg :has_pins)
   (is-open :initarg :is_open)
   (last-read :initarg :last_read :type string :initform "0")
   (latest :initarg :latest :initform nil)
   (unread-count :initarg :unread_count)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (messages :initarg :messages :initform ())))

(defgeneric slack-room-name (room))
(defgeneric slack-room-history (room &optional oldest))
(defgeneric slack-room-buffer-header (room))
(defgeneric slack-room-update-mark-url (room))

(defmethod slack-room-subscribedp ((_room slack-room))
  nil)

(defmethod slack-room-set-prev-messages ((room slack-room) m)
  (oset room messages (cl-delete-duplicates (append (oref room messages) m)
                                            :test #'slack-message-equal)))

(defmethod slack-room-set-messages ((room slack-room) m)
  (oset room messages m))

(cl-defmacro slack-room-request-update (room url &optional latest)
  `(cl-labels
       ((create-message-with-room (payload)
                                  (slack-message-create payload :room ,room))
        (on-request-update (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-room-request-update")
                            (let* ((datum (plist-get data :messages))
                                   (messages (mapcar #'create-message-with-room datum)))
                              (slack-room-update-last-read room
                                                           (slack-message :ts "0"))
                              (if ,latest
                                  (slack-room-set-prev-messages ,room messages)
                                (slack-room-set-messages ,room messages))))))
     (slack-request
      ,url
      :params (list (cons "token" ,slack-token)
                    (cons "channel" (oref ,room id))
                    (if ,latest
                        (cons "latest" ,latest)))
      :success #'on-request-update)))

(cl-defmacro slack-room-make-buffer (name list &key test (update nil))
  (let ((room (cl-gensym)))
    `(let ((,room (cdr (cl-assoc ,name ,list :test ,test))))
       (slack-room-make-buffer-with-room ,room :update ,update))))

(cl-defun slack-room-make-buffer-with-room (room &key update)
  (with-slots (messages latest) room
    (if (or update (< (length messages) 1))
        (slack-room-history room))
    (funcall slack-buffer-function
             (slack-buffer-create room))))

(cl-defmacro slack-select-from-list ((candidates prompt) &body body)
  "Bind candidates from selected."
  `(let ((selected (let ((completion-ignore-case t))
                     (completing-read (format "%s" ,prompt)
                                      ,candidates nil t nil nil ,candidates))))
     ,@body))

(defun slack-room-select (rooms)
  (let* ((list (slack-room-names rooms))
         (candidates (mapcar #'car list)))
    (slack-select-from-list
     (candidates "Select Channel: ")
     (slack-room-make-buffer selected
                             list
                             :test #'string=
                             :update nil))))


(defmethod slack-room-update-message ((room slack-room) m)
  (with-slots (messages) room
    (cl-pushnew m messages :test #'slack-message-equal)
    (oset room latest (oref m ts))))

(cl-defun slack-room-list-update (url success &key (sync t))
  (slack-request
   url
   :params (list (cons "token" slack-token))
   :success success
   :sync sync))

(defun slack-room-update-messages ()
  (interactive)
  (unless (and (boundp 'slack-current-room) slack-current-room)
    (error "Call From Slack Room Buffer"))
    (slack-room-history slack-current-room)
    (slack-buffer-create slack-current-room
                         #'(lambda (room)
                             (let ((inhibit-read-only t))
                               (delete-region (point-min)
                                              (marker-position lui-output-marker)))
                             (slack-buffer-insert-messages room))))

(defun slack-room-load-prev-messages ()
  (interactive)
  (let* ((cur-point (point))
         (msg-beg (next-single-property-change cur-point 'ts))
         (ts (get-text-property msg-beg 'ts))
         (line (thing-at-point 'line))
         (oldest (ignore-errors (get-text-property 0 'oldest line))))
    (slack-room-history slack-current-room oldest)
    (slack-buffer-create
     slack-current-room
     #'(lambda (room)
         (let ((inhibit-read-only t)
               (loading-message-end (next-single-property-change
                                     cur-point
                                     'oldest))
               (prev-messages (slack-room-prev-messages room oldest)))
           (delete-region (point-min) loading-message-end)
           (set-marker lui-output-marker (point-min))
           (if prev-messages
               (progn
                 (slack-buffer-insert-previous-link (cl-first prev-messages))
                 (mapc (lambda (m)
                         (lui-insert (slack-message-to-string m)))
                       prev-messages))
             (insert "(no more messages)")))
         (slack-buffer-recover-lui-output-marker)
         (goto-char (text-property-any (point-min) (point-max) 'ts ts))))))

(defun slack-room-find-message (room ts)
  (cl-find-if #'(lambda (m) (string= ts (oref m ts)))
              (oref room messages)))

(defmethod slack-room-name-with-unread-count ((room slack-room))
  (let ((name (slack-room-name room)))
    (with-slots (unread-count-display) room
    (if (< 0 unread-count-display)
        (concat name " (" (number-to-string unread-count-display) ")")
      name))))

(defun slack-room-names (rooms)
  (sort (mapcar #'(lambda (room)
                    (cons (slack-room-name-with-unread-count room)
                          room))
                rooms)
        #'(lambda (a b) (> (oref (cdr a) unread-count-display)
                           (oref (cdr b) unread-count-display)))))

(defmethod slack-room-name ((room slack-room))
  (oref room name))

(defmethod slack-room-update-last-read ((room slack-room) msg)
  (with-slots (ts) msg
    (oset room last-read ts)))

(defmethod slack-room-latest-messages ((room slack-room))
  (with-slots (last-read messages) room
    (cl-remove-if #'(lambda (m)
                      (or (string< (oref m ts) last-read)
                          (string= (oref m ts) last-read)))
                  (cl-sort (copy-sequence messages)
                           #'string<
                           :key #'(lambda (m) (oref m ts))))))

(defmethod slack-room-prev-messages ((room slack-room) from)
  (with-slots (messages) room
    (cl-remove-if #'(lambda (m)
                      (or (string< from (oref m ts))
                          (string= from (oref m ts))))
                  (cl-sort (copy-sequence messages)
                           #'string<
                           :key #'(lambda (m) (oref m ts))))))

(defmethod slack-room-update-mark ((room slack-room) msg)
  (cl-labels ((on-update-mark (&key data &allow-other-keys)
                              (slack-request-handle-error
                               (data "slack-room-update-mark"))))
    (with-slots (ts) msg
      (with-slots (id) room
        (slack-request
         (slack-room-update-mark-url room)
         :type "POST"
         :params (list (cons "token"  slack-token)
                       (cons "channel"  id)
                       (cons "ts"  ts))
         :success #'on-update-mark
         :sync nil)))))

(defun slack-room-pins-list ()
  (interactive)
  (unless (boundp 'slack-current-room)
    (error "Call from slack room buffer"))
  (let* ((room slack-current-room)
         (channel (oref room id)))
    (cl-labels ((on-pins-list (&key data &allow-other-keys)
                              (slack-request-handle-error
                               (data "slack-room-pins-list")
                               (slack-room-on-pins-list (plist-get data :items)
                                                        room))))
      (slack-request
       slack-room-pins-list-url
       :params (list (cons "token" slack-token)
                     (cons "channel" channel))
       :success #'on-pins-list
       :sync nil))))

(defun slack-room-on-pins-list (items room)
  (cl-labels ((buffer-name (room)
                           (concat "*Slack - Pinned Items*"
                                   " : "
                                   (slack-room-name room))))
    (let* ((messages (mapcar #'slack-message-create
                           (mapcar #'(lambda (i) (plist-get i :message))
                                   items)))
         (buf-header (propertize "Pinned Items"
                                 'face '(:underline t
                                         :weight bold))))
    (funcall slack-buffer-function
             (slack-buffer-create-info
              (buffer-name room)
              #'(lambda () (insert buf-header)
                  (insert "\n\n")
                  (mapc #'(lambda (m) (insert
                                       (slack-message-to-string m)))
                        messages)))))))

(defun slack-select-rooms ()
  (interactive)
  (slack-room-select (append slack-ims slack-groups slack-channels)))

(provide 'slack-room)
;;; slack-room.el ends here
