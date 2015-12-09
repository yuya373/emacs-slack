;;; slack-message-formatter.el --- format message text  -*- lexical-binding: t; -*-

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
(require 'slack-user)
(require 'slack-room)

(defface slack-message-output-text
  '((t (:weight normal :height 0.9)))
  "Face used to text message."
  :group 'slack)

(defface slack-message-output-header
  '((t (:foreground "#FFA000"
        :weight bold
        :height 1.0
        :underline t)))
  "Face used to text message."
  :group 'slack)

(defface slack-message-output-reaction
  '((t (:overline t)))
  "Face used to reactions."
  :group 'slack)

(defun slack-message-put-header-property (header)
  (put-text-property 0 (length header)
                       'face 'slack-message-output-header header))

(defun slack-message-put-text-property (text)
  (put-text-property 0 (length text)
                       'face 'slack-message-output-text text))

(defun slack-message-put-reactions-property (text)
  (put-text-property 0 (length text)
                     'face 'slack-message-output-reaction text))

(defun slack-message-put-hard (text)
  (put-text-property 0 (length text) 'hard t text))

(defun slack-message-time-to-string (ts)
  (if ts
      (format-time-string "%Y-%m-%d %H:%M:%S"
                          (seconds-to-time (string-to-number ts)))))

(defun slack-message-reactions-to-string (reactions)
  (if reactions
      (mapconcat #'slack-reaction-to-string reactions " ")))

(defmethod slack-message-to-string ((m slack-message))
  (with-slots (text) m
    (let ((ts (slack-message-time-to-string (oref m ts)))
          (text (slack-message-unescape-string text)))
      (slack-message-put-header-property ts)
      (slack-message-put-text-property text)
      (concat ts "\n" text "\n"))))

(defmethod slack-message-to-alert ((m slack-message))
  (with-slots (text) m
    (slack-message-unescape-string text)))

(defun slack-message-unescape-string (text)
  (let* ((and-unescpaed
          (replace-regexp-in-string "&amp;" "&" text))
         (lt-unescaped
          (replace-regexp-in-string "&lt;" "<" and-unescpaed))
         (gt-unescaped
          (replace-regexp-in-string "&gt;" ">" lt-unescaped)))
    (slack-message-unescape-command
     (slack-message-unescape-user-id
      (slack-message-unescape-channel gt-unescaped)))))

(defun slack-message-unescape-user-id (text)
  (let ((user-regexp "<@\\(U.*?\\)>"))
    (cl-labels ((unescape-user-id (text)
                                  (concat "@" (or (slack-message-replace-user-name text)
                                                  (slack-user-name (match-string 1 text))
                                                  (match-string 1 text)))))
      (replace-regexp-in-string user-regexp
                                #'unescape-user-id
                                text t))))

(defun slack-message-replace-user-name (text)
  (let ((user-name-regexp "<@U.*?|\\(.*?\\)>"))
    (cl-labels ((replace-user-id-with-name (text)
                                          (match-string 1 text)))
      (if (string-match-p user-name-regexp text)
        (replace-regexp-in-string user-name-regexp
                                          #'replace-user-id-with-name
                                          text)))))

(defun slack-message-unescape-command (text)
  (let ((command-regexp "<!\\(.*?\\)>"))
    (cl-labels ((unescape-command
                 (text)
                 (concat "@" (match-string 1 text))))
      (replace-regexp-in-string command-regexp
                                #'unescape-command
                                text))))

(defun slack-message-unescape-channel (text)
  (let ((channel-regexp "<#\\(C.*?\\)|\\(.*?\\)>"))
    (cl-labels ((unescape-channel
                 (text)
                 (concat "@" (or (match-string 2 text)
                                 (slack-room-find
                                  (match-string 1 text))
                                 (match-string 1 text)))))
      (replace-regexp-in-string channel-regexp
                                #'unescape-channel
                                text t))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
