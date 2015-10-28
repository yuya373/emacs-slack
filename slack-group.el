;;; slack-group.el ---slack private group interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuya Minami

;; Author: Yuya Minami
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

(defgroup slack-group nil
  "Slack private groups."
  :prefix "slack-group-"
  :group 'slack)

(defvar slack--group-open-url "https://slack.com/api/groups.open")
(defvar slack-group-history-url "https://slack.com/api/groups.history")
(defvar slack-group-buffer-name "*Slack - Private Group*")
(defvar slack-update-group-list-url "https://slack.com/api/groups.list")
(defvar slack-room-subscription '())

(defclass slack-room ()
  ((id :initarg :id)
   (created :initarg :created)
   (has-pins :initarg :has_pins)
   (is-open :initarg :is_open)
   (last-read :initarg :last_read)
   (latest :initarg :latest)
   (unread-count :initarg :unread_count)
   (unread-count-display :initarg :unread_count_display)
   (messages :initarg :messages :initform nil)))

(defclass slack-group (slack-room)
  ((name :initarg :name :type string)
   (is-group :initarg :is_group)
   (creator :initarg :creator)
   (is-archived :initarg :is_archived)
   (is-mpim :initarg :is_mpim)
   (members :initarg :members :type list)
   (topic :initarg :topic)
   (purpose :initarg :purpose)))

(defun slack-group-create (payload)
  (plist-put payload :members (append (plist-get payload :members) nil))
  (apply #'slack-group "group"
         (slack-collect-slots 'slack-group payload)))

(defun slack-room-find (id)
  (cond
   ((string-prefix-p "G" id) (slack-group-find id))
   ((string-prefix-p "D" id) (slack-im-find id))))

(defun slack-group-find (id)
  (find-if (lambda (group) (string= id (oref group id)))
           slack-groups))

(defun slack-room-name-by-id (id)
  (let ((room (slack-room-find id)))
    (slack-room-name room)))

(defmethod slack-room-name ((room slack-group))
  (oref room name))

(defun slack-group-find-by-name (name)
  (find-if (lambda (group) (string= name (oref group name)))
           slack-groups))

(defun slack-group-names ()
  (mapcar (lambda (group)
            (cons (oref group name) group))
          slack-groups))

(defmethod slack-room-subscribedp ((room slack-room))
  nil)

(defmethod slack-room-subscribedp ((room slack-group))
  (with-slots (name) room
    (and name
         (memq (intern name) slack-room-subscription))))

(defmethod slack-room-buffer-name ((room slack-group))
  (concat slack-group-buffer-name " : " (slack-room-name room)))

(defmethod slack-room-buffer-header ((room slack-group))
  (concat "Private Group: " (slack-room-name room) "\n"))

;; (defun slack-update-group-list ()
;;   (interactive)
;;   (cl-labels ((on-update-group-list
;;                (&key data &allow-other-keys)
;;                (unless (gethash "ok" data)
;;                  (error "slack-update-group-list failed"))
;;                (setf slack-groups (gethash "groups" data))))
;;     (slack-request
;;      slack-update-group-list-url
;;      :params (list (cons "token" slack-token))
;;      :success #'on-update-group-list)))

(defun slack-group-update-history (group)
  (cl-labels ((on-group-update (&key data &allow-other-keys)
                               (slack-group-on-update-history
                                data group)))
    (slack-request
     slack-group-history-url
     :params (list (cons "token" slack-token)
                   (cons "channel" (oref group id)))
     :success #'on-group-update)))

(defun slack-group-on-update-history (data group)
  (unless (plist-get data :ok)
    (error "%s" data))
  (let* ((messages (plist-get data :messages))
         (s-messages (slack-message-create-with-room messages group)))
    (slack-room-set-messages group s-messages)))

(defmethod slack-room-set-messages ((room slack-room) messages)
  (oset room messages messages))

(defun slack-group-select (name)
  (interactive (list (slack-group-read-list
                      "Select Group: "
                      (mapcar #'car (slack-group-names)))))
  (let* ((room (cdr (cl-assoc name (slack-group-names) :test #'string=))))
    (slack-group-update-history room)
    (switch-to-buffer-other-window
     (slack-buffer-create (slack-room-buffer-name room)
                          (oref room id)
                          (slack-room-buffer-header room)
                          (oref room messages)))))

(defun slack-group-read-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s: " prompt)
                     choices nil t nil nil choices)))

(provide 'slack-group)
;;; slack-group.el ends here
