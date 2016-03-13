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

(defvar slack-current-room-id)
(defvar slack-current-team-id)
(defvar slack-buffer-function)
(defconst slack-room-pins-list-url "https://slack.com/api/pins.list")

(defclass slack-room ()
  ((name :initarg :name :type string)
   (id :initarg :id)
   (created :initarg :created)
   (has-pins :initarg :has_pins)
   (is-open :initarg :is_open)
   (last-read :initarg :last_read :type string :initform "0")
   (latest :initarg :latest :initform nil)
   (unread-count :initarg :unread_count)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (messages :initarg :messages :initform ())
   (team-id :initarg :team-id)))

(defgeneric slack-room-name (room))
(defgeneric slack-room-history (room team &optional oldest after-success sync))
(defgeneric slack-room-update-mark-url (room))

(defmethod slack-room-subscribedp ((_room slack-room) _team)
  nil)

(defmethod slack-room-buffer-name ((room slack-room))
  (concat "*Slack*"
          " : "
          (slack-room-name-with-team-name room)))

(defmethod slack-room-set-prev-messages ((room slack-room) m)
  (oset room messages (cl-delete-duplicates (append (oref room messages) m)
                                            :test #'slack-message-equal)))

(defmethod slack-room-set-messages ((room slack-room) m)
  (oset room messages m))

(cl-defmacro slack-room-request-update (room team url latest after-success sync)
  `(cl-labels
       ((create-message-with-room (payload)
                                  (slack-message-create payload
                                                        :room ,room))
        (on-request-update
         (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-room-request-update")
          (let* ((datum (plist-get data :messages))
                 (messages (mapcar #'create-message-with-room
                                   datum)))
            (if ,latest
                (slack-room-set-prev-messages ,room messages)
              (slack-room-set-messages ,room messages)
              (let ((m (slack-message "msg" :ts "0")))
                (slack-room-update-last-read room m)))
            (if (and ,after-success
                     (functionp ,after-success))
                (funcall ,after-success))))))
     (slack-request
      ,url
      ,team
      :params (list (cons "channel" (oref ,room id))
                    (if ,latest
                        (cons "latest" ,latest)))
      :success #'on-request-update
      :sync (if ,sync t nil))))

(cl-defmacro slack-room-make-buffer (name list &key test (update nil) team)
  (let ((room-id (cl-gensym)))
    `(let ((,room-id (oref (cdr (cl-assoc ,name ,list :test ,test))
                           id)))
       (slack-room-make-buffer-with-room (slack-room-find ,room-id ,team)
                                         ,team
                                         :update ,update))))

(cl-defun slack-room-make-buffer-with-room (room team &key update)
  (with-slots (messages latest) room
    (if (or update (< (length messages) 1))
        (slack-room-history room team))
    (funcall slack-buffer-function
             (slack-buffer-create room team))))

(cl-defmacro slack-select-from-list ((alist prompt) &body body)
  "Bind candidates from selected."
  (let ((key (cl-gensym)))
    `(let* ((,key (let ((completion-ignore-case t))
                    (completing-read (format "%s" ,prompt)
                                     ,alist nil t)))
            (selected (cdr (assoc ,key alist))))
       ,@body)))

(defun slack-extract-from-list (selected candidates)
  (cdr (cl-assoc selected candidates :test #'string=)))

(defun slack-room-select (rooms)
  (let* ((alist (slack-room-names
                 rooms
                 #'(lambda (rs)
                     (cl-remove-if #'(lambda (r)
                                       (or (not (slack-room-member-p r))
                                           (slack-room-archived-p r)))
                                   rs)))))
    (slack-select-from-list
     (alist "Select Channel: ")
     (slack-room-make-buffer-with-room
      selected
      (slack-team-find (oref selected team-id))
      :update nil))))

(defmethod slack-room-update-message ((room slack-room) m)
  (unless (object-of-class-p m 'slack-message)
    (error (format "%s" m)))
  (with-slots (messages latest) room
    (when (< 0 (length messages))
      (cl-pushnew m messages :test #'slack-message-equal))
    (if (string< (oref latest ts) (oref m ts))
        (setq latest m))))

(cl-defun slack-room-list-update (url success team &key (sync t))
  (slack-request
   url
   team
   :success success
   :sync sync))

(defun slack-room-update-messages ()
  (interactive)
  (unless (and (boundp 'slack-current-room-id)
               (boundp 'slack-current-team-id))
    (error "Call From Slack Room Buffer"))
  (let* ((team (slack-team-find slack-current-team-id))
         (room (slack-room-find slack-current-room-id team))
         (cur-point (point)))
    (slack-room-history room team)
    (slack-buffer-create room
                         team
                         #'(lambda (room team)
                             (let ((inhibit-read-only t))
                               (delete-region (point-min)
                                              (marker-position lui-output-marker)))
                             (slack-buffer-insert-messages room team)
                             (goto-char cur-point)))))

(defun slack-room-load-prev-messages ()
  (interactive)
  (cl-labels
      ((render-prev-messages
        (current-room current-team cur-point oldest ts)
        (slack-buffer-create
         current-room
         current-team
         #'(lambda (room team)
             (let ((inhibit-read-only t)
                   (loading-message-end (text-property-any
                                         (point-min) (point-max)
                                         'ts oldest))
                   (prev-messages (slack-room-prev-messages room oldest)))
               (delete-region (point-min) loading-message-end)
               (set-marker lui-output-marker (point-min))
               (if prev-messages
                   (progn
                     (slack-buffer-insert-previous-link
                      (cl-first prev-messages))
                     (mapc (lambda (m)
                             (slack-buffer-insert m team t))
                           prev-messages))
                 (insert "(no more messages)\n")))
             (slack-buffer-recover-lui-output-marker)
             (goto-char (text-property-any (point-min) (point-max) 'ts ts))))))
    (let* ((cur-point (point))
           (msg-beg (next-single-property-change cur-point 'ts))
           (ts (get-text-property msg-beg 'ts))
           (line (thing-at-point 'line))
           (oldest (ignore-errors (get-text-property 0 'oldest line)))
           (current-team (slack-team-find slack-current-team-id))
           (current-room (slack-room-find slack-current-room-id
                                          current-team)))
      (slack-room-history current-room
                          current-team
                          oldest
                          #'(lambda () (render-prev-messages
                                        current-room
                                        current-team
                                        cur-point oldest ts))))))

(defun slack-room-find-message (room ts)
  (cl-find-if #'(lambda (m) (string= ts (oref m ts)))
              (oref room messages)
              :from-end t))

(defmethod slack-room-unread-count ((room slack-room))
  (with-slots (unread-count-display) room
    (if (< 0 unread-count-display)
        (concat "(" (number-to-string unread-count-display) ")")
      "")))

(defmethod slack-room-latest-ts ((room slack-room))
  (with-slots (latest) room
    (if latest
        (oref latest ts)
      "0"))
  ;; (let ((sorted (slack-room-sorted-messages room)))
  ;;   (if sorted
  ;;       (oref (car (last sorted)) ts)
  ;;     "0"))
  )

(defmethod slack-room-name-with-team-name ((room slack-room))
  (with-slots (team-id name) room
    (let ((team (slack-team-find team-id)))
      (format "%s - %s" (oref team name) name))))

(defmacro slack-room-names (rooms &optional filter)
  `(cl-labels
       ((sort-rooms (l)
                    (nreverse (cl-sort l
                                       #'string<
                                       :key #'(lambda (r)
                                                (slack-room-latest-ts
                                                 (cdr r))))))
        (build-label (room)
                     (concat (im-presence room)
                             (format "%s %s"
                                     (slack-room-name-with-team-name room)
                                     (slack-room-unread-count room))))
        (im-presence (room)
                     (if (object-of-class-p room 'slack-im)
                         (slack-im-user-presence room)
                       "  "))
        (build-cons (room)
                    (cons (build-label room) room)))
     (sort-rooms
      (cl-loop for room in (if ,filter
                               (funcall ,filter ,rooms)
                             ,rooms)
               collect (cons (build-label room) room))
      ;; (mapcar #'build-cons
      ;;         (if ,filter
      ;;             (funcall ,filter ,rooms)
      ;;           ,rooms))
      )))

(defmethod slack-room-name ((room slack-room))
  (oref room name))

(defmethod slack-room-update-last-read ((room slack-room) msg)
  (with-slots (ts) msg
    (oset room last-read ts)))

(defmethod slack-room-latest-messages ((room slack-room) messages)
  (with-slots (last-read) room
    (cl-remove-if #'(lambda (m)
                      (or (string< (oref m ts) last-read)
                          (string= (oref m ts) last-read)))
                  messages)))

(defmethod slack-room-sorted-messages ((room slack-room))
  (with-slots (messages) room
    (cl-sort (copy-sequence messages)
             #'string<
             :key #'(lambda (m) (oref m ts)))))

(defmethod slack-room-prev-messages ((room slack-room) from)
  (with-slots (messages) room
    (cl-remove-if #'(lambda (m)
                      (or (string< from (oref m ts))
                          (string= from (oref m ts))))
                  (cl-sort (copy-sequence messages)
                           #'string<
                           :key #'(lambda (m) (oref m ts))))))

(defmethod slack-room-update-mark ((room slack-room) team msg)
  (cl-labels ((on-update-mark (&key data &allow-other-keys)
                              (slack-request-handle-error
                               (data "slack-room-update-mark"))))
    (with-slots (ts) msg
      (with-slots (id) room
        (slack-request
         (slack-room-update-mark-url room)
         team
         :type "POST"
         :params (list (cons "channel"  id)
                       (cons "ts"  ts))
         :success #'on-update-mark
         :sync nil)))))

(defun slack-room-pins-list ()
  (interactive)
  (unless (and (bound-and-true-p slack-current-room-id)
               (bound-and-true-p slack-current-team-id))
    (error "Call from slack room buffer"))
  (let* ((team (slack-team-find slack-current-team-id))
         (room (slack-room-find slack-current-room-id
                                team))
         (channel (oref room id)))
    (cl-labels ((on-pins-list (&key data &allow-other-keys)
                              (slack-request-handle-error
                               (data "slack-room-pins-list")
                               (slack-room-on-pins-list
                                (plist-get data :items)
                                room team))))
      (slack-request
       slack-room-pins-list-url
       team
       :params (list (cons "channel" channel))
       :success #'on-pins-list
       :sync nil))))

(defun slack-room-on-pins-list (items room team)
  (cl-labels ((buffer-name (room)
                           (concat "*Slack - Pinned Items*"
                                   " : "
                                   (slack-room-name room team))))
    (let* ((messages (mapcar #'slack-message-create
                             (mapcar #'(lambda (i)
                                         (plist-get i :message))
                                     items)))
           (buf-header (propertize "Pinned Items"
                                   'face '(:underline
                                           t
                                           :weight bold))))
      (funcall slack-buffer-function
               (slack-buffer-create-info
                (buffer-name room)
                #'(lambda ()
                    (insert buf-header)
                    (insert "\n\n")
                    (mapc #'(lambda (m) (insert
                                         (slack-message-to-string m)))
                          messages)))
               team))))

(defun slack-select-rooms ()
  (interactive)
  (let ((team (slack-team-select)))
    (with-slots (ims groups channels) team
      (slack-room-select (append ims groups channels) team))))

(defun slack-create-room (url team success)
  (slack-request
   url
   team
   :type "POST"
   :params (list (cons "name" (read-from-minibuffer "Name: ")))
   :success success
   :sync nil))

(defun slack-room-rename (url room-list-func)
  (cl-labels
      ((on-rename-success (&key data &allow-other-keys)
                          (slack-request-handle-error
                           (data "slack-room-rename"))))
    (let* ((team (slack-team-select))
           (room-list (funcall room-list-func team))
           (candidates (mapcar #'car room-list))
           (room (slack-select-from-list
                  (candidates "Select Channel: ")
                  (slack-extract-from-list selected room-list)))
           (name (read-from-minibuffer "New Name: ")))
      (slack-request
       url
       team
       :params (list (cons "channel" (oref room id))
                     (cons "name" name))
       :success #'on-rename-success
       :sync nil))))

(defmacro slack-current-room-or-select (room-list)
  `(if (and (boundp 'slack-current-room-id)
            (boundp 'slack-current-team-id))
       (slack-room-find slack-current-room-id
                        (slack-team-find slack-current-team-id))
     (let* ((list ,room-list)
            (candidates (mapcar #'car list)))
       (slack-select-from-list
        (candidates "Select Channel: ")
        (slack-extract-from-list selected list)))))

(defmacro slack-room-invite (url room-list-func)
  `(cl-labels
       ((on-group-invite (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-room-invite")
                          (if (plist-get data :already_in_group)
                              (message "User already in group")
                            (message "Invited!")))))
     (let* ((team (slack-team-select))
            (room (slack-current-room-or-select
                   (funcall ,room-list-func team)))
            (users (slack-user-names team))
            (user-id (slack-select-from-list
                      ((mapcar #'car users) "Select User: ")
                      (slack-extract-from-list selected users))))
       (slack-request
        ,url
        team
        :params (list (cons "channel" (oref room id))
                      (cons "user" user-id))
        :success #'on-group-invite
        :sync nil))))

(defmethod slack-room-member-p ((_room slack-room))
  t)

(defmethod slack-room-archived-p ((_room slack-room))
  nil)

(defmethod slack-room-equal-p ((room slack-room) other)
  (with-slots (id) room
    (with-slots ((other-id id)) other
      (string= id other-id))))

(defun slack-room-deleted (id team)
  (let ((room (slack-room-find id team)))
    (cond
     ((object-of-class-p room 'slack-channel)
      (with-slots (channels) team
        (setq channels (cl-delete-if #'(lambda (c) (slack-room-equal-p room c))
                                     channels)))
      (message "Channel: %s deleted" (slack-room-name room team))))))

(cl-defun slack-room-request-with-id (url id team success)
  (slack-request
   url
   team
   :params (list (cons "channel" id))
   :success success
   :sync nil))

(defmethod slack-room-history ((room slack-room) team
                               &optional
                               oldest
                               after-success
                               async)
  (slack-room-request-update room
                             team
                             (slack-room-history-url room)
                             oldest
                             after-success
                             (if async nil t)))

(provide 'slack-room)
;;; slack-room.el ends here
