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
(require 'slack-util)
(require 'slack-user)
(require 'slack-room)
(require 'slack-message)
(require 'slack-user-message)
(require 'slack-usergroup)
(require 'slack-thread)
(require 'slack-file)
(require 'slack-attachment)
(require 'slack-image)
(require 'slack-block)
(require 'slack-unescape)
(require 'slack-bot-message)
(require 'slack-message-faces)

(defvar slack-current-buffer)

(defcustom slack-visible-thread-sign ":left_speech_bubble: "
  "Used to thread message sign if visible-threads mode."
  :type 'string
  :group 'slack)

(defun slack-message-time-to-string (ts)
  (when ts
    (when (stringp ts)
      (setf ts (string-to-number ts)))
    (format-time-string "%Y-%m-%d %H:%M:%S"
                        (seconds-to-time ts))))

(defun slack-format-message (&rest args)
  (let ((messages args))
    (mapconcat #'identity
               (cl-remove-if #'(lambda (e) (or (null e)
                                               (< (length e) 1)))
                             messages)
               "\n")))

(cl-defmethod slack-message-to-string ((m slack-message) team)
  (let* ((header (slack-message-header m team))
         (attachment (mapconcat #'(lambda (attachment)
                                    (slack-message-to-string attachment team))
                                (oref m attachments)
                                "\n"))
         (body (format "%s%s"
                       (if (slack-message-display-thread-sign-p m team)
                           slack-visible-thread-sign
                         "")
                       (slack-message-body m team)))
         (files (let ((ts (slack-ts m)))
                  (mapconcat #'(lambda (file)
                                 (slack-message-to-string file ts team))
                             (oref m files)
                             "\n\n")))
         (reactions (mapconcat #'slack-reaction-to-string
                               (slack-message-reactions m)
                               " "))
         (thread (slack-thread-to-string m team)))
    (slack-format-message (propertize header
                                      'slack-message-header t)
                          (if (oref m deleted-at)
                              (slack-message-put-deleted-property body)
                            body)
                          files
                          attachment
                          reactions
                          thread)))

(cl-defmethod slack-message-to-alert ((m slack-message) team)
  (with-slots (text attachments files) m
    (let ((alert-text
           (cond
            ((and text (< 0 (length text))) text)
            ((and attachments (< 0 (length attachments)))
             (mapconcat #'slack-attachment-to-alert attachments " "))
            ((and files (< 0 (length files)))
             (mapconcat #'(lambda (file) (oref file title)) files " ")))))
      (slack-unescape alert-text team))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
