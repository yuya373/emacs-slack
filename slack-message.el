;;; slack-message.el ---slack-message                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami(require 'eieio) <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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

(defclass slack-message ()
  ((type :initarg :type :type string)
   (room :initarg :room :initform nil)
   (subtype :initarg :subtype)
   (channel :initarg :channel :initform nil)
   (ts :initarg :ts :type string)
   (text :initarg :text)
   (item-type :initarg :item_type)
   (attachments :initarg :attachments :type (or null list))
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean)
   (pinned-to :initarg :pinned_to :type (or null list))))

(defclass slack-file-message (slack-message)
  ((file :initarg :file)
   ;; (bot-id :initarg :bot_id :type (or null string))
   ;; (username :initarg :username)
   ;; (display-as-bot :initarg :display_as_bot)
   (upload :initarg :upload)
   (user :initarg :user)))

(defclass slack-file ()
  ((id :initarg :id)
   (created :initarg :created)
   (timestamp :initarg :timestamp)
   (name :initarg :name)
   (size :initarg :size)
   (public :initarg :public)
   (url :initarg :url)
   (url-download :initarg :url_download)
   (url-private :initarg :url_private)
   (channels :initarg :channels :type list)
   (groups :initarg :groups :type list)
   (ims :initarg :ims :type list)
   (reactions :initarg :reactions :type list)
   (username :initarg :username)
   (bot-id :initarg :bot_id)
   (ts :initarg :ts :type string)))

(defun slack-message-decode-payload (payload)
  (cl-labels ((decode (e)
                      (if (stringp e)
                          (slack-message-decode-string e)
                        e)))
    (mapcar #'decode payload)))

(defun slack-attachment-create (payload)
  (plist-put payload :fields (append (plist-get payload :fields) nil))
  (let ((decoded (slack-message-decode-payload payload)))
    (apply #'slack-attachment "attachment"
         (slack-collect-slots 'slack-attachment decoded))))

(defmethod slack-message-set-attachments ((m slack-message) payload)
  (let ((attachments (plist-get payload :attachments)))
    (if (< 0 (length attachments))
        (oset m attachments
              (mapcar #'slack-attachment-create attachments)))))

(defun slack-message-decode-string (text)
  (decode-coding-string text 'utf-8-unix))

(defmethod slack-message-set-attributes ((m slack-message) payload)
  (let* ((decoded-payload (slack-message-decode-payload payload))
         (text (plist-get decoded-payload :text)))
    (oset m text text)
    (slack-message-set-attachments m decoded-payload)
    m))

(cl-defun slack-message-create (m &key room)
  (plist-put m :reactions (append (plist-get m :reactions) nil))
  (plist-put m :attachments (append (plist-get m :attachments) nil))
  (plist-put m :pinned_to (append (plist-get m :pinned_to) nil))
  (plist-put m :room room)
  (let ((subtype (plist-get m :subtype)))
    (cond
     ((plist-member m :reply_to)
      (apply #'slack-reply "reply"
             (slack-collect-slots 'slack-reply m)))
     ((and subtype (string-prefix-p "file" subtype))
      (apply #'slack-file-message "file-msg"
             (slack-collect-slots 'slack-file-message m)))
     ((plist-member m :user)
      (apply #'slack-user-message "user-msg"
             (slack-collect-slots 'slack-user-message m)))
     ((plist-member m :bot_id)
      (apply #'slack-bot-message "bot-msg"
             (slack-collect-slots 'slack-bot-message m))))))

(defun slack-message-create-with-room (payloads room)
  (cl-labels ((setup (payload)
                    (slack-message-set-attributes
                     (slack-message-create payload :room room)
                     payload)))
      (mapcar #'setup payloads)))

(defun slack-message-set (room messages)
  (let ((messages (mapcar #'slack-message-create messages)))
    (puthash "messages" messages room)))

(defmethod slack-message-equal ((m slack-message) n)
  (and (string= (oref m ts) (oref n ts))
       (string= (oref m text) (oref n text))))

(defmethod slack-message-update ((m slack-message))
  (with-slots (room channel) m
    (let ((room (or room (slack-room-find channel))))
      (when room
        (slack-message-popup-tip m room)
        (slack-message-notify-buffer m room)
        (slack-room-update-messages room m)
        (slack-buffer-update (slack-room-buffer-name room)
                             m)))))

(provide 'slack-message)
;;; slack-message.el ends here
