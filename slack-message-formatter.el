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

(defface slack-message-deleted-face
  '((t (:strike-through t)))
  "Face used to deleted message."
  :group 'slack)

(defun slack-message-put-header-property (header)
  (if header
      (propertize header 'face 'slack-message-output-header)))

(defun slack-message-put-text-property (text)
  (if text
      (propertize text 'face 'slack-message-output-text)))

(defun slack-message-put-reactions-property (text)
  (if text
      (propertize text 'face 'slack-message-output-reaction)))

(defun slack-message-put-hard (text)
  (if text
      (propertize text 'hard t)))

(defun slack-message-put-deleted-property (text)
  (if text
      (propertize text 'face 'slack-message-deleted-face)))

(defmethod slack-message-propertize ((m slack-message) text)
  text)

(defun slack-message-time-to-string (ts)
  (if ts
      (format-time-string "%Y-%m-%d %H:%M:%S"
                          (seconds-to-time (string-to-number ts)))))

(defun slack-message-reactions-to-string (reactions)
  (if reactions
      (concat "\n" (mapconcat #'slack-reaction-to-string reactions " "))))

(defmethod slack-message-header ((m slack-message) team)
  (slack-message-sender-name m team))

(defun slack-format-message (header body attachment-body reactions)
  (let ((messages (list header body attachment-body reactions)))
    (concat (mapconcat #'identity
               (cl-remove-if #'(lambda (e) (< (length e) 1)) messages)
               "\n")
            "\n")))

(defmethod slack-message-to-string ((m slack-message) team)
  (let ((text (if (slot-boundp m 'text)
                  (oref m text))))
    (let* ((header (slack-message-put-header-property
                    (slack-message-header m team)))
           (row-body (slack-message-body m team))
           (attachment-body (slack-message-attachment-body m team))
           (body (if (oref m deleted-at)
                     (slack-message-put-deleted-property row-body)
                   (slack-message-put-text-property row-body)))
           (reactions-str
            (slack-message-put-reactions-property
             (slack-message-reactions-to-string (oref m reactions)))))
      (slack-message-propertize
       m (slack-format-message header body attachment-body reactions-str)))))

(defmethod slack-message-body ((m slack-message) team)
  (with-slots (text) m
    (slack-message-unescape-string text team)))

(defmethod slack-message-attachment-body ((m slack-message) team)
  (with-slots (attachments) m
    (let ((body (mapconcat #'slack-attachment-to-string attachments "\n")))
      (if (< 0 (length body))
          (slack-message-unescape-string body team)))))

(defmethod slack-message-to-alert ((m slack-message) team)
  (with-slots (text) m
    (slack-message-unescape-string text team)))

(defun slack-message-unescape-string (text team)
  (when text
    (let* ((and-unescpaed
            (replace-regexp-in-string "&amp;" "&" text))
           (lt-unescaped
            (replace-regexp-in-string "&lt;" "<" and-unescpaed))
           (gt-unescaped
            (replace-regexp-in-string "&gt;" ">" lt-unescaped)))
      (slack-message-unescape-command
       (slack-message-unescape-user-id
        (slack-message-unescape-channel gt-unescaped)
        team)))))

(defun slack-message-unescape-user-id (text team)
  (let ((user-regexp "<@\\(U.*?\\)>"))
    (cl-labels ((unescape-user-id
                 (text)
                 (concat "@" (or
                              (slack-message-replace-user-name text)
                              (slack-user-name (match-string 1 text) team)
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
                 (concat "#" (or (match-string 2 text)
                                 (slack-room-find
                                  (match-string 1 text))
                                 (match-string 1 text)))))
      (replace-regexp-in-string channel-regexp
                                #'unescape-channel
                                text t))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
