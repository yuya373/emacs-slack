;;; slack-bot-message.el ---bot message class        -*- lexical-binding: t; -*-

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

(defclass slack-bot-message (slack-message)
  ((bot-id :initarg :bot_id :type string)
   (username :initarg :username)
   (icons :initarg :icons)))

(defclass slack-attachment ()
  ((fallback :initarg :fallback :type string)
   (title :initarg :title :initform nil)
   (title-link :initarg :title_link :initform nil)
   (pretext :initarg :pretext :initform nil)
   (text :initarg :text :initform nil)
   (author-name :initarg :author_name)
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :type (or null list))
   (image-url :initarg :image_url)
   (thumb-url :initarg :thumb_url)))

(defmethod slack-message-to-string ((m slack-bot-message))
  (with-slots (text attachments bot-id) m
    (let* ((name (slack-bot-name bot-id))
           (time (slack-message-time-to-string m))
           (attachment-string (mapconcat #'slack-attachment-to-string
                                         attachments "\n"))
           (header (concat name "\t" time)))
      (slack-message-put-header-property header)
      (slack-message-put-text-property attachment-string)
      (concat header "\n" text "\n" attachment-string "\n"))))


(defmethod slack-attachment-to-string ((a slack-attachment))
  (with-slots (fallback text pretext title) a
      (if text
          (concat title pretext text)
        fallback)))

(provide 'slack-bot-message)
;;; slack-bot-message.el ends here
