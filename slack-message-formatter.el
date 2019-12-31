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

(defvar slack-current-buffer)
(defface slack-profile-image-face
  '((t ()))
  "Face used to profile image."
  :group 'slack)

(defface slack-message-output-text
  '((t (:weight normal)))
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
  '((t (:box (:line-width 1 :style released-button))))
  "Face used to reactions."
  :group 'slack)

(defface slack-message-deleted-face
  '((t (:strike-through t)))
  "Face used to deleted message."
  :group 'slack)

(defface slack-attachment-header
  '((t (:inherit slack-message-output-header)))
  "Face used to shared message header."
  :group 'slack)

(defface slack-attachment-footer
  '((t (:height 0.8)))
  "Face used to shared message footer."
  :group 'slack)

(defface slack-attachment-pad
  '((t (:weight ultra-bold)))
  "Face used to shared message pad."
  :group 'slack)

(defface slack-attachment-field-title
  '((t (:weight bold :height 1.0)))
  "Face used to attachment field title."
  :group 'slack)

(defface slack-channel-button-face
  '((t (:underline t :foreground "cyan")))
  "Face used to channel button."
  :group 'slack)

(defface slack-message-mention-face
  '((t (:background "#073642" :foreground "#2aa198")))
  "Face used to mention."
  :group 'slack)

(defface slack-message-mention-me-face
  '((t (:background "#073642" :foreground "#b58900")))
  "Face used to mention."
  :group 'slack)

(defface slack-message-mention-keyword-face
  '((t (:background "#073642" :foreground "#859900")))
  "Face used to @here, @channel, @everyone mention."
  :group 'slack)

(defcustom slack-date-formats
  '((date_num . "%Y-%m-%d")
    (date . "%B %d, %Y")
    (date_short . "%b %d, %Y")
    (date_long . "%A %B %d, %Y")
    (date_pretty . "%B %d, %Y")
    (date_short_pretty . "%b %d, %Y")
    (date_long_pretty . "%A %B %d, %Y")
    (time . "%H:%M")
    (time_secs . "%H:%M:%S"))
  "Date formats for Slack's date token.
this format string passed to `format-time-string' function.
see \"Formatting dates\" section in https://api.slack.com/docs/message-formatting"
  :type '(repeat (cons symbol string))
  :group 'slack)

(defcustom slack-visible-thread-sign ":left_speech_bubble: "
  "Used to thread message sign if visible-threads mode."
  :type 'string
  :group 'slack)

(defun slack-message-put-header-property (header)
  (if header
      (propertize header 'face 'slack-message-output-header)))

(defun slack-message-put-text-property (text)
  (if text
      (propertize text 'face 'slack-message-output-text)))

(defun slack-message-put-hard (text)
  (if text
      (propertize text 'hard t)))

(defun slack-message-put-deleted-property (text)
  (if text
      (propertize text 'face 'slack-message-deleted-face)))

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

(cl-defmethod slack-message--inspect ((this slack-message) room team)
  (format "RAW: %s\nROOM: %s\nUSER: %s\nBOT: %S\nMESSAGE: %s\nATTACHMENTS: %s - %s\nFILES: %s - %s"
          (oref this text)
          (oref room id)
          (oref this user)
          (and (slot-exists-p this 'bot-id)
               (slot-boundp this 'bot-id)
               (oref this bot-id))
          (eieio-object-class this)
          (length (oref this attachments))
          (mapcar (lambda (e) (format "\n(CLASS: %s\nTITLE: %s\nPRETEXT: %s\nTEXT: %s\nIMAGE: %s\nTHUMBNAIL: %s\nFILES:%s)"
                                      (eieio-object-class e)
                                      (slack-unescape-channel
                                       (or (oref e title) "")
                                       team)
                                      (oref e pretext)
                                      (oref e text)
                                      (oref e image-url)
                                      (oref e thumb-url)
                                      (length (oref e files))))
                  (oref this attachments))
          (length (oref this files))
          (mapcar (lambda (e) (format "(TITLE: %s)"
                                      (oref e title)))
                  (oref this files))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
