;;; slack-bot-message.el --- bot message class        -*- lexical-binding: t; -*-

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
(require 'slack-message)
(require 'slack-message-formatter)
(require 'slack-util)
(require 'slack-bot)
(require 'slack-image)

(cl-defmethod slack-message-bot-id ((this slack-bot-message))
  (oref this bot-id))

(cl-defmethod slack-bot-name ((m slack-bot-message) team)
  (or (unless (slack-string-blankp (oref m username))
        (oref m username))
      (when (slot-boundp m 'bot-id)
        (let ((bot (slack-find-bot (oref m bot-id) team)))
          (plist-get bot :name)))
      "Unknown Bot"))

(cl-defmethod slack-message-to-alert ((m slack-bot-message) team)
  (let ((text (if (slot-boundp m 'text)
                  (oref m text))))
    (with-slots (attachments) m
      (if (and text (< 0 (length text)))
          (slack-message-unescape-string text team)
        (let ((attachment-string (mapconcat #'slack-attachment-to-alert attachments " ")))
          (slack-message-unescape-string attachment-string team))))))

(cl-defmethod slack-message-sender-name ((m slack-bot-message) team)
  (slack-bot-name m team))

(cl-defmethod slack-message-sender-id ((m slack-bot-message))
  (oref m bot-id))

(defun slack-bot-image-url (bot size)
  (let ((icons (plist-get bot :icons)))
    (cond
     ((eq size 36) (plist-get icons :image_36))
     ((eq size 48) (plist-get icons :image_48))
     ((eq size 72) (plist-get icons :image_72))
     (t (plist-get icons :image_36)))))

(defun slack-bot-fetch-image (bot size team)
  (let* ((image-url (slack-bot-image-url bot size))
         (file-path (and image-url (slack-profile-image-path image-url team))))
    (when file-path
      (if (file-exists-p file-path) file-path
        (slack-url-copy-file image-url file-path))
      file-path)))

(cl-defun slack-bot-image (bot team &optional (size 36))
  (when bot
    (let ((image (slack-bot-fetch-image bot size team)))
      (when image
        (create-image image nil nil :ascent 80)))))

(cl-defmethod slack-bot-find ((m slack-bot-message) team)
  (or (and (slot-boundp m 'bot-id)
           (slack-find-bot (slack-message-sender-id m) team))
      (slack-user-find-by-name (oref m username) team)))

(cl-defmethod slack-message-profile-image ((m slack-bot-message) team)
  (let ((bot (slack-bot-find m team)))
    (slack-bot-image bot team)))

(provide 'slack-bot-message)
;;; slack-bot-message.el ends here
