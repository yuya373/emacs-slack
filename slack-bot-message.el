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

(defvar slack-bots)

(defun slack-find-bot (id)
  (cl-find-if (lambda (bot)
             (string= id (plist-get bot :id)))
           slack-bots))

(defmethod slack-bot-name ((m slack-bot-message))
  (if (slot-boundp m 'bot-id)
      (let ((bot (slack-find-bot (oref m bot-id))))
        (if bot
            (plist-get bot :name)))
    (oref m username)))

(defmethod slack-message-to-alert ((m slack-bot-message))
  (with-slots (text attachments) m
    (if (< 0 (length text))
          (slack-message-unescape-string text)
      (let ((attachment-string (mapconcat #'slack-attachment-to-alert attachments " ")))
        (slack-message-unescape-string attachment-string)))))

(defmethod slack-message-sender-name ((m slack-bot-message))
  (slack-bot-name m))

(defmethod slack-attachment-to-string ((a slack-attachment))
  (with-slots (fallback text pretext title title-link) a
    (mapconcat #'identity
               (cl-remove-if #'null
                             (list pretext title title-link text fallback))
               "\n")))

(defmethod slack-attachment-to-alert ((a slack-attachment))
  (oref a fallback))

(provide 'slack-bot-message)
;;; slack-bot-message.el ends here
