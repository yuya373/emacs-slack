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
(require 'slack-pinned-item)

(defvar slack-buffer-function)
(defconst slack-room-pins-list-url "https://slack.com/api/pins.list")

(defclass slack-room ()
  ((name :initarg :name :type (or null string) :initform nil)
   (id :initarg :id)
   (created :initarg :created)
   (latest :initarg :latest)
   (unread-count :initarg :unread_count :initform 0 :type integer)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (messages :initarg :messages :initform ())
   (last-read :initarg :last_read :type string :initform "0")
   (members :initarg :members :type list :initform '())))


(defgeneric slack-room-name (room team))
(defgeneric slack-room-history (room team &optional oldest after-success sync))
(defgeneric slack-room-update-mark-url (room))

(defmethod slack-equalp ((this slack-room) other)
  (string= (oref this id)
           (oref other id)))

(defmethod slack-merge ((this slack-room) other)
  "except MESSAGES"
  (oset this name (oref other name))
  (oset this id (oref other id))
  (oset this created (oref other created))
  (oset this latest (oref other latest))
  (oset this unread-count (oref other unread-count))
  (oset this unread-count-display (oref other unread-count-display))
  (unless (string= "0" (oref other last-read))
    (oset this last-read (oref other last-read))))

(defun slack-room-create (payload team class)
  (cl-labels
      ((prepare (p)
                (plist-put p :members
                           (append (plist-get p :members) nil))
                p))
    (let* ((attributes (slack-collect-slots class (prepare payload)))
           (room (apply #'make-instance class attributes)))
      (oset room latest (slack-message-create (plist-get payload :latest) team :room room))
      room)))

(defmethod slack-room-subscribedp ((_room slack-room) _team)
  nil)

(defmethod slack-room-buffer-name ((room slack-room) team)
  (concat "*Slack*"
          " : "
          (slack-room-display-name room team)))

(cl-defmacro slack-select-from-list ((alist prompt &key initial) &body body)
  "Bind candidates from selected."
  (declare (indent 2) (debug t))
  (let ((key (cl-gensym)))
    `(let* ((,key (let ((completion-ignore-case t))
                    (funcall slack-completing-read-function (format "%s" ,prompt)
                             ,alist nil t ,initial)))
            (selected (cdr (cl-assoc ,key ,alist :test #'string=))))
       ,@body
       selected)))

(defmethod slack-room-hidden-p ((room slack-room))
  (slack-room-hiddenp room))

(defun slack-room-hiddenp (room)
  (or (not (slack-room-member-p room))
      (slack-room-archived-p room)
      (not (slack-room-open-p room))))

(defmacro slack-room-names (rooms team &optional filter collecter)
  `(cl-labels
       ((latest-ts (room)
                   (with-slots (latest) room
                     (if latest (slack-ts latest) "0")))
        (sort-rooms (rooms)
                    (nreverse (cl-sort rooms #'string< :key #'latest-ts))))
     (cl-loop for room in (sort-rooms (if ,filter
                                          (funcall ,filter ,rooms)
                                        ,rooms))
              as label = (slack-room-label room team)
              collect (if (functionp ,collecter)
                          (funcall ,collecter label room)
                        (cons label room)))))

(defun slack-room-select (rooms team)
  (let* ((alist (slack-room-names
                 rooms team #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p rs)))))
    (slack-select-from-list (alist "Select Channel: "))))

(cl-defun slack-room-list-update (url success team &key (sync t))
  (slack-request
   (slack-request-create
    url
    team
    :success success)))

(defun slack-room-find-message (room ts)
  (cl-find-if #'(lambda (m) (string= ts (slack-ts m)))
              (oref room messages)
              :from-end t))

(defun slack-room-find-thread-parent (room thread-message)
  (slack-room-find-message room (oref thread-message thread-ts)))

(defmethod slack-message-thread ((this slack-message) _room)
  (oref this thread))

(defmethod slack-message-thread ((this slack-reply-broadcast-message) room)
  (let ((message (slack-room-find-message room
                                          (or (oref this broadcast-thread-ts)
                                              (oref this thread-ts)))))
    (slack-message-thread message room)))

(defun slack-room-find-thread (room ts)
  (let ((message (slack-room-find-message room ts)))
    (when message
      (slack-message-thread message room))))

(defmethod slack-room-display-name ((room slack-room) team)
  (let ((room-name (slack-room-name room team)))
    (if slack-display-team-name
        (format "%s - %s"
                (slack-team-name team)
                room-name)
      room-name)))

(defmethod slack-room-label-prefix ((_room slack-room) _team)
  "  ")

(defmethod slack-room-unread-count-str ((room slack-room))
  (with-slots (unread-count-display) room
    (if (< 0 unread-count-display)
        (concat " ("
                (number-to-string unread-count-display)
                ")")
      "")))

(defmethod slack-room-label ((room slack-room) team)
  (format "%s%s%s"
          (slack-room-label-prefix room team)
          (slack-room-display-name room team)
          (slack-room-unread-count-str room)))

(defmethod slack-room-name ((room slack-room) _team)
  (oref room name))

(defun slack-room-sort-messages (messages)
  (cl-sort messages #'string< :key #'slack-ts))

(defun slack-room-reject-thread-message (messages)
  (cl-remove-if #'(lambda (m) (and (not (eq (eieio-object-class-name m)
                                            'slack-reply-broadcast-message))
                                   (slack-thread-message-p m)))
                messages))

(defmethod slack-room-sorted-messages ((room slack-room))
  (with-slots (messages) room
    (slack-room-sort-messages (copy-sequence messages))))

(defmethod slack-room-set-prev-messages ((room slack-room) prev-messages)
  (slack-room-set-messages room
                           (append (oref room messages)
                                   prev-messages)))

(defmethod slack-room-append-messages ((room slack-room) messages)
  (slack-room-set-messages room
                           (append messages (oref room messages))))

(defmethod slack-room-update-latest ((room slack-room) message)
  (when (and message
             (not (slack-thread-message-p message)))
    (with-slots (latest) room
      (if (or (null latest)
              (string< (slack-ts latest) (slack-ts message)))
          (setq latest message)))))

(defmethod slack-room-push-message ((room slack-room) message)
  (with-slots (messages) room
    (setq messages
          (cl-remove-if #'(lambda (n) (slack-message-equal message n))
                        messages))
    (push message messages)))

(defmethod slack-room-set-messages ((room slack-room) messages)
  (let* ((sorted (slack-room-sort-messages
                  (cl-delete-duplicates messages
                                        :test #'slack-message-equal)))
         (oldest (car sorted))
         (latest (car (last sorted))))
    (oset room messages sorted)
    (slack-room-update-latest room latest)))

(defmethod slack-room-prev-messages ((room slack-room) from)
  (with-slots (messages) room
    (cl-remove-if #'(lambda (m)
                      (or (string< from (slack-ts m))
                          (string= from (slack-ts m))))
                  (slack-room-sort-messages (copy-sequence messages)))))

(defmethod slack-room-update-mark ((room slack-room) team ts)
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

(defun slack-room-pins-list ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-pins-list buf)))

(defun slack-select-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels)))
                team)))
    (slack-room-display room team)))

(defun slack-create-room (url team success)
  (slack-request
   (slack-request-create
    url
    team
    :type "POST"
    :params (list (cons "name" (read-from-minibuffer "Name: ")))
    :success success)))

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
       (slack-request-create
        url
        team
        :params (list (cons "channel" (oref room id))
                      (cons "name" name))
        :success #'on-rename-success)))))

(defmacro slack-current-room-or-select (room-alist-func &optional select)
  `(if (and (not ,select)
            (bound-and-true-p slack-current-buffer)
            (slot-boundp slack-current-buffer 'room))
       (oref slack-current-buffer room)
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
        (slack-request-create
         ,url
         team
         :params (list (cons "channel" (oref room id))
                       (cons "user" user-id))
         :success #'on-group-invite)))))

(defmethod slack-room-member-p ((_room slack-room)) t)

(defmethod slack-room-archived-p ((_room slack-room)) nil)

(defmethod slack-room-open-p ((_room slack-room)) t)

(defmethod slack-room-equal-p ((room slack-room) other)
  (string= (oref room id) (oref other id)))

(defun slack-room-deleted (id team)
  (let ((room (slack-room-find id team)))
    (cond
     ((object-of-class-p room 'slack-channel)
      (with-slots (channels) team
        (setq channels (cl-delete-if #'(lambda (c) (slack-room-equal-p room c))
                                     channels)))
      (message "Channel: %s deleted"
               (slack-room-display-name room team))))))

(cl-defun slack-room-request-with-id (url id team success)
  (slack-request
   (slack-request-create
    url
    team
    :params (list (cons "channel" id))
    :success success)))

(defmethod slack-room-inc-unread-count ((room slack-room))
  (cl-incf (oref room unread-count-display)))

(defun slack-room-find-by-name (name team)
  (cl-labels
      ((find-by-name (rooms name)
                     (cl-find-if #'(lambda (e) (string= name
                                                        (slack-room-name e team)))
                                 rooms)))
    (or (find-by-name (oref team groups) name)
        (find-by-name (oref team channels) name)
        (find-by-name (oref team ims) name))))

(defmethod slack-room-info-request-params ((room slack-room))
  (list (cons "channel" (oref room id))))

(defmethod slack-room-create-info-request ((room slack-room) team)
  (cl-labels
      ((on-success
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-room-info-request"
               #'(lambda (e)
                   (if (not (string= e "user_disabled"))
                       (message "Failed to request slack-room-info-request: %s" e))))
         (slack-room-update-info room data team))))
    (slack-request-create
     (slack-room-get-info-url room)
     team
     :params (slack-room-info-request-params room)
     :success #'on-success)))

(defmethod slack-room-info-request ((room slack-room) team)
  (slack-request
   (slack-room-create-info-request room team)))

(defmethod slack-room-get-members ((room slack-room))
  (oref room members))

(defun slack-room-user-select ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-user-profile buf)))

(defun slack-select-unread-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (cl-remove-if
                                   #'(lambda (room)
                                       (not (< 0 (oref room
                                                       unread-count-display))))
                                   (append ims groups channels))))
                team)))
    (slack-room-display room team)))

(defmethod slack-user-find ((room slack-room) team)
  (slack-user--find (oref room user) team))

(defun slack-room-display (room team)
  (cl-labels
      ((open (buf)
             (slack-buffer-display buf)))
    (let* ((buf (slack-buffer-find (or (and (eq (eieio-object-class-name room)
                                                'slack-file-room)
                                            'slack-file-list-buffer)
                                       'slack-message-buffer)
                                   room team)))
      (if buf (open buf)
        (message "No Message in %s, fetching from server..." (slack-room-name room team))
        (slack-room-history-request
         room team
         :after-success #'(lambda (&rest _ignore)
                            (open (slack-create-message-buffer room team))))))))

(defmethod slack-room-update-buffer ((this slack-room) team message replace)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer this team)))
      (slack-buffer-update buffer message :replace replace)
    (and slack-buffer-create-on-notify
         (slack-room-history-request
          this team
          :after-success #'(lambda (&rest _ignore)
                             (tracking-add-buffer
                              (slack-buffer-buffer
                               (slack-create-message-buffer this team))))))))

(cl-defmethod slack-room-history-request ((room slack-room) team &key oldest latest count after-success async)
  (cl-labels
      ((on-request-update
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-room-request-update")
         (let ((messages
                 (cl-loop for message in (plist-get data :messages)
                          collect (slack-message-create message team :room room)))
               (has-more (not (eq :json-false (plist-get data :has_more)))))
           (if oldest (slack-room-set-prev-messages room messages)
             (if latest (slack-room-append-messages room messages)
               (slack-room-set-messages room messages)))
           (if (and after-success (functionp after-success))
               (funcall after-success has-more))))))
    (slack-request
     (slack-request-create
      (slack-room-history-url room)
      team
      :params (list (cons "channel" (oref room id))
                    (if oldest (cons "latest" oldest))
                    (if latest (cons "oldest" latest))
                    (cons "count" (number-to-string (or count 100))))
      :success #'on-request-update))))

(defmethod slack-room-member-p ((this slack-room))
  t)

(provide 'slack-room)
;;; slack-room.el ends here
