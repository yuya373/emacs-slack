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
   (last-read :initarg :last_read :type string :initform "0")
   (latest :initarg :latest)
   (oldest :initarg :oldest)
   (unread-count :initarg :unread_count)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (messages :initarg :messages :initform ())
   (team-id :initarg :team-id)))

(defgeneric slack-room-name (room))
(defgeneric slack-room-history (room team &optional oldest after-success sync))
(defgeneric slack-room-update-mark-url (room))

(defun slack-room-create (payload team class)
  (cl-labels
      ((prepare (p)
                (plist-put p :members
                           (append (plist-get p :members) nil))
                (plist-put p :latest
                           (slack-message-create (plist-get p :latest)))
                (plist-put p :team-id (oref team id))
                p))
    (let ((attributes (slack-collect-slots class (prepare payload))))
      (apply #'make-instance class attributes))))

(defmethod slack-room-subscribedp ((_room slack-room) _team)
  nil)

(defmethod slack-room-buffer-name ((room slack-room))
  (concat "*Slack*"
          " : "
          (slack-room-name-with-team-name room)))

(cl-defmacro slack-room-request-update (room team url latest after-success sync)
  `(cl-labels
       ((on-request-update
         (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-room-request-update")
          (let* ((datum (plist-get data :messages))
                 (messages
                  (cl-loop for data across datum
                           collect (slack-message-create data :room ,room))))
            (if ,latest
                (slack-room-set-prev-messages ,room messages)
              (slack-room-set-messages ,room messages)
              (slack-room-update-last-read
               room
               (make-instance 'slack-message :ts "0")))
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
            (selected (cdr (cl-assoc ,key ,alist :test #'string=))))
       ,@body
       selected)))

(defun slack-room-select (rooms)
  (let* ((alist (slack-room-names
                 rooms
                 #'(lambda (rs)
                     (cl-remove-if #'(lambda (r)
                                       (or (not (slack-room-member-p r))
                                           (slack-room-archived-p r)
                                           (not (slack-room-open-p r))))
                                   rs)))))
    (slack-select-from-list
     (alist "Select Channel: ")
     (slack-room-make-buffer-with-room
      selected
      (slack-team-find (oref selected team-id))
      :update nil))))

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
    (slack-buffer-create
     room team :insert-func
     #'(lambda (room team)
         (slack-buffer-widen
          (let ((inhibit-read-only t))
            (delete-region (point-min) (marker-position lui-output-marker))))
         (slack-buffer-insert-previous-link room)
         (slack-buffer-insert-messages room team)
         (goto-char cur-point)))))

(defmethod slack-room-render-prev-messages ((room slack-room) team
                                            oldest ts)
  (slack-buffer-create
   room team
   :insert-func
   #'(lambda (room team)
       (slack-buffer-widen
        (let ((inhibit-read-only t)
              (loading-message-end
               (slack-buffer-ts-eq (point-min) (point-max) oldest)))
          (delete-region (point-min) loading-message-end)
          (slack-buffer-insert-prev-messages room team oldest)))
       (slack-buffer-goto ts))))

(defmethod slack-room-prev-link-info ((room slack-room))
  (with-slots (oldest) room
    (if oldest
        (oref oldest ts))))

(defun slack-room-load-prev-messages ()
  (interactive)
  (let* ((cur-point (point))
         (ts (get-text-property (next-single-property-change cur-point 'ts)
                                'ts))
         (oldest (ignore-errors (get-text-property 0 'oldest
                                                   (thing-at-point 'line))))
         (current-team (slack-team-find slack-current-team-id))
         (current-room (slack-room-find slack-current-room-id
                                        current-team)))
    (slack-room-history current-room
                        current-team
                        oldest
                        #'(lambda ()
                            (slack-room-render-prev-messages current-room
                                                             current-team
                                                             oldest ts)))))

(defun slack-room-find-message (room ts)
  (cl-find-if #'(lambda (m) (string= ts (oref m ts)))
              (oref room messages)
              :from-end t))

(defmethod slack-room-name-with-team-name ((room slack-room))
  (with-slots (team-id name) room
    (let ((team (slack-team-find team-id)))
      (format "%s - %s" (oref team name) name))))

(defmacro slack-room-names (rooms &optional filter)
  `(cl-labels
       ((latest-ts (room)
                   (with-slots (latest) room
                     (if latest (oref latest ts) "0")))
        (unread-count (room)
                      (with-slots (unread-count-display) room
                        (if (< 0 unread-count-display)
                            (concat "("
                                    (number-to-string unread-count-display)
                                    ")")
                          "")))
        (sort-rooms (rooms)
                    (nreverse
                     (cl-sort rooms #'string<
                              :key #'(lambda (name-with-room) (latest-ts (cdr name-with-room))))))
        (build-label (room)
                     (concat (im-presence room)
                             (format "%s %s"
                                     (slack-room-name-with-team-name room)
                                     (unread-count room))))
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
               collect (cons (build-label room) room)))))

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

(defun slack-room-sort-messages (messages)
  (cl-sort messages
           #'string<
           :key #'(lambda (m) (oref m ts))))

(defmethod slack-room-sorted-messages ((room slack-room))
  (with-slots (messages) room
    (slack-room-sort-messages (copy-sequence messages))))

(defmethod slack-room-set-prev-messages ((room slack-room) prev-messages)
  (slack-room-set-messages
   room
   (cl-delete-duplicates (append (oref room messages)
                                 prev-messages)
                         :test #'slack-message-equal)))

(defmethod slack-room-set-messages ((room slack-room) m)
  (let ((sorted (slack-room-sort-messages m)))
    (oset room oldest (car sorted))
    (oset room messages sorted)
    (oset room latest (car (last sorted)))))

(defmethod slack-room-prev-messages ((room slack-room) from)
  (with-slots (messages) room
    (cl-remove-if #'(lambda (m)
                      (or (string< from (oref m ts))
                          (string= from (oref m ts))))
                  (slack-room-sort-messages (copy-sequence messages)))))

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
                                   (slack-room-name-with-team-name room))))
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
    (slack-room-select
     (cl-loop for team in (list team)
              append (with-slots (groups ims channels) team
                       (append ims groups channels))))))

(defun slack-create-room (url team success)
  (slack-request
   url
   team
   :type "POST"
   :params (list (cons "name" (read-from-minibuffer "Name: ")))
   :success success
   :sync nil))

(defun slack-room-rename (url room-alist-func)
  (cl-labels
      ((on-rename-success (&key data &allow-other-keys)
                          (slack-request-handle-error
                           (data "slack-room-rename"))))
    (let* ((team (slack-team-select))
           (room-alist (funcall room-alist-func team))
           (room (slack-select-from-list
                  (room-alist "Select Channel: ")))
           (name (read-from-minibuffer "New Name: ")))
      (slack-request
       url
       team
       :params (list (cons "channel" (oref room id))
                     (cons "name" name))
       :success #'on-rename-success
       :sync nil))))

(defmacro slack-current-room-or-select (room-alist-func)
  `(if (and (boundp 'slack-current-room-id)
            (boundp 'slack-current-team-id))
       (slack-room-find slack-current-room-id
                        (slack-team-find slack-current-team-id))
     (let* ((room-alist (funcall ,room-alist-func)))
       (slack-select-from-list
        (room-alist "Select Channel: ")))))

(defmacro slack-room-invite (url room-alist-func)
  `(cl-labels
       ((on-group-invite (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-room-invite")
                          (if (plist-get data :already_in_group)
                              (message "User already in group")
                            (message "Invited!")))))
     (let* ((team (slack-team-select))
            (room (slack-current-room-or-select
                   #'(lambda ()
                       (funcall ,room-alist-func team
                                #'(lambda (rooms)
                                    (cl-remove-if #'slack-room-archived-p
                                                  rooms))))))
            (user-id (plist-get (slack-select-from-list
                                 ((slack-user-names team)
                                  "Select User: ")) :id)))
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

(defmethod slack-room-open-p ((_room slack-room))
  t)

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
      (message "Channel: %s deleted"
               (slack-room-name-with-team-name room))))))

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

(defmethod slack-room-inc-unread-count ((room slack-room))
  (cl-incf (oref room unread-count-display)))

(defun slack-room-find-by-name (name team)
  (cl-labels
      ((find-by-name (rooms name)
                     (cl-find-if #'(lambda (e) (string= name
                                                        (slack-room-name e)))
                                 rooms)))
    (or (find-by-name (oref team groups) name)
        (find-by-name (oref team channels) name)
        (find-by-name (oref team ims) name))))

(provide 'slack-room)
;;; slack-room.el ends here
