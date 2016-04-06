;;; slack-file.el ---  handle files                  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

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
(require 'slack-room)

(defconst slack-file-list-url "https://slack.com/api/files.list")
(defconst slack-file-upload-url "https://slack.com/api/files.upload")
(defconst slack-file-delete-url "https://slack.com/api/files.delete")

(defclass slack-file (slack-message)
  ((id :initarg :id)
   (created :initarg :created)
   (name :initarg :name)
   (size :initarg :size)
   (public :initarg :public)
   (filetype :initarg :filetype)
   (user :initarg :user)
   (preview :initarg :preview)
   (initial-comment :initarg :initial_comment :initform nil)
   (permalink :initarg :permalink)
   (channels :initarg :channels :type list)
   (groups :initarg :groups :type list)
   (ims :initarg :ims :type list)
   (username :initarg :username)))

(defclass slack-file-room (slack-room) ())

(defun slack-file-room-obj (team)
  (with-slots (file-room) team
    (if file-room
        file-room
      (setq file-room (slack-file-room "file-room"
                                       :name "Files"
                                       :id "F"
                                       :team-id (oref team id)
                                       :created (format-time-string "%s")
                                       :last_read "0"
                                       :latest nil
                                       :unread_count 0
                                       :unread_count_display 0
                                       :messages '())))))

(defun slack-file-create (payload)
  (plist-put payload :channels (append (plist-get payload :channels) nil))
  (plist-put payload :groups (append (plist-get payload :groups) nil))
  (plist-put payload :ims (append (plist-get payload :ims) nil))
  (plist-put payload :reactions (append (plist-get payload :reactions) nil))
  (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
  (plist-put payload :ts (number-to-string (plist-get payload :timestamp)))
  (let ((file (apply #'slack-file "file"
                     (slack-collect-slots 'slack-file payload))))
    (oset file reactions
          (mapcar #'slack-reaction-create (plist-get payload :reactions)))
    file))

(defmethod slack-message-equal ((f slack-file) other)
  (string= (oref f id) (oref other id)))

(defmethod slack-file-pushnew ((f slack-file) team)
  (let ((room (slack-file-room-obj team)))
    (with-slots (messages) room
      (cl-pushnew f messages
                  :test #'slack-message-equal))))

(defmethod slack-message-body ((file slack-file) team)
  (with-slots (initial-comment) file
    (let ((body (plist-get initial-comment :comment)))
      (slack-message-unescape-string body team))))

(defmethod slack-message-to-string ((file slack-file) team)
  (with-slots (ts name size filetype permalink user initial-comment reactions)
      file
    (let* ((header (slack-user-name user team))
           (body (format "name: %s\nsize: %s\ntype: %s\n%s\n"
                         name size filetype permalink))
           (reactions-str (slack-message-reactions-to-string
                           reactions)))
      (slack-message-put-header-property header)
      (slack-message-put-text-property body)
      (slack-message-put-reactions-property reactions-str)
      (let ((message
             (concat header "\n" body
                     (if initial-comment
                         (format "comment: %s\n%s\n"
                                 (slack-user-name
                                  (plist-get initial-comment :user)
                                  team)
                                 (slack-message-body file team)))
                     (if reactions-str
                         (concat "\n" reactions-str "\n")))))
        (put-text-property 0 (length message) 'ts ts message)
        message))))

(defmethod slack-room-update-mark ((_room slack-file-room) _team _msg))

(defun slack-file-create-buffer (team)
  (funcall slack-buffer-function
           (slack-buffer-create (slack-file-room-obj team)
                                team
                                :type 'info)))

(defun slack-file-list ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-file-room-obj team)))
    (with-slots (messages) room
      (if messages
          (slack-file-create-buffer team)
        (slack-room-history room team nil
                            #'(lambda ()
                                (slack-file-create-buffer team)))))))

(defmethod slack-room-history ((room slack-file-room) team
                               &optional
                               oldest
                               after-success
                               async)
  (cl-labels
      ((on-file-list
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-file-list")
         (let ((files (cl-loop for e across (plist-get data :files)
                               collect (slack-file-create e))))
           (if oldest
               (slack-room-set-prev-messages room files)
             (slack-room-update-last-read room
                                          (make-instance 'slack-message
                                                         :ts "0"))
             (slack-room-set-messages room files)))
         (if after-success
             (funcall after-success)))))
    (slack-request
     slack-file-list-url
     team
     :params (list (if oldest
                       (cons "ts_to" oldest)))
     :success #'on-file-list
     :sync (if async nil t))))

(defun slack-file-upload ()
  (interactive)
  (cl-labels
      ((on-file-upload (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-file-upload")))
       (select-channels (channels acc)
                        (let ((selected (completing-read "Select Channel: "
                                                         channels nil t)))
                          (if (< 0 (length selected))
                              (select-channels channels (push selected acc))
                            acc)))
       (channel-id (selected channels)
                   (oref (cdr (cl-assoc selected channels :test #'string=))
                         id)))
    (let* ((team (slack-team-select))
           (channels (slack-room-names
                      (append (oref team ims)
                              (oref team channels)
                              (oref team groups))))
           (target-channels (select-channels channels '()))
           (channel-ids (mapconcat #'(lambda (selected)
                                       (channel-id selected channels))
                                   (cl-delete-if #'null target-channels)
                                   ","))
           (buf (find-file-noselect
                 (car (find-file-read-args
                       "Select File: "
                       (confirm-nonexistent-file-or-buffer)))))
           (filename (read-from-minibuffer "Filename: "
                                           (file-name-nondirectory
                                            (buffer-file-name buf))))
           (filetype (read-from-minibuffer "Filetype: "
                                           (file-name-extension
                                            (buffer-file-name buf))))
           (initial-comment (read-from-minibuffer "Message: ")))
      (slack-request
       slack-file-upload-url
       team
       :type "POST"
       :params (list (cons "filename" filename)
                     (cons "channels" channel-ids)
                     (cons "filetype" filetype)
                     (if initial-comment
                         (cons "initial_comment" initial-comment)))
       :files (list (cons "file" buf))
       :headers (list (cons "Content-Type" "multipart/form-data"))
       :success #'on-file-upload
       :sync nil))))

(defun slack-file-delete ()
  (interactive)
  (cl-labels
      ((on-file-delete (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-file-delete"))))
    (let* ((team (slack-team-select))
           (files (oref (slack-file-room-obj team) messages))
           (your-files (cl-remove-if #'(lambda (f)
                                         (not (string= (oref f user)
                                                       (oref team self-id))))
                                     files))
           (candidates (mapcar #'(lambda (f)
                                   (cons (concat
                                          (slack-message-time-to-string (oref f ts))
                                          " "
                                          (oref f name))
                                         f))
                               your-files))
           (selected (completing-read "Select File: " candidates))
           (deleting-file (cdr (cl-assoc selected candidates :test #'string=))))
      (slack-request
       slack-file-delete-url
       team
       :params (list (cons "file" (oref deleting-file id)))
       :sync nil
       :success #'on-file-delete))))

(provide 'slack-file)
;;; slack-file.el ends here
