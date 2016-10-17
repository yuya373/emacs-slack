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

(defun slack-find-bot (id team)
  (with-slots (bots) team
    (cl-find-if (lambda (bot)
                  (string= id (plist-get bot :id)))
                bots)))

(defmethod slack-bot-name ((m slack-bot-message) team)
  (if (slot-boundp m 'bot-id)
      (let ((bot (slack-find-bot (oref m bot-id) team)))
        (if bot
            (plist-get bot :name)
          (oref m username)))
    (oref m username)))

(defmethod slack-message-to-alert ((m slack-bot-message) team)
  (let ((text (if (slot-boundp m 'text)
                  (oref m text))))
    (with-slots (attachments) m
      (if (and text (< 0 (length text)))
          (slack-message-unescape-string text team)
        (let ((attachment-string (mapconcat #'slack-attachment-to-alert attachments " ")))
          (slack-message-unescape-string attachment-string team))))))

(defmethod slack-message-sender-name ((m slack-bot-message) team)
  (slack-bot-name m team))

(defmethod slack-attachment-to-string ((a slack-attachment))
  (with-slots (fallback text pretext title title-link) a
    (if (and pretext text)
        (mapconcat #'identity
                   (cl-remove-if #'null (list pretext title title-link text))
                   "\n\n")
      fallback)))

(defmethod slack-attachment-to-string((a slack-shared-message))
  (with-slots (fallback text author-name ts channel-name color from-url) a
    (let* ((header-property '(:weight bold))
           (footer-property '(:height 0.8))
           (pad-property '(:weight ultra-bold))
           (pad (propertize "|" 'face pad-property))
           (header (concat pad "\t"
                           (propertize author-name 'face header-property)))
           (body (format "%s\t%s" pad (mapconcat #'identity
                                                 (split-string text "\n")
                                                 (format "\n\t%s\t" pad))))
           (footer (concat pad "\t"
                           (propertize
                            (format "%s %s" channel-name (slack-message-time-to-string ts))
                            'face footer-property))))
      (format "\t%s\n \t%s\n \t%s"
              header
              body
              footer))))

(defmethod slack-attachment-to-alert ((a slack-attachment))
  (oref a fallback))

(provide 'slack-bot-message)
;;; slack-bot-message.el ends here
