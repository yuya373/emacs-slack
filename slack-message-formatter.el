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

(defface slack-profile-image-face
  '((t (:background "#fff")))
  "Face used to profile image."
  :group 'slack)

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
  '((t (:box (:line-width 1 :style released-button))))
  "Face used to reactions."
  :group 'slack)

(defface slack-message-deleted-face
  '((t (:strike-through t)))
  "Face used to deleted message."
  :group 'slack)

(defface slack-attachment-header
  '((t (:weight bold)))
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

(defcustom slack-date-formats
  '((date_num . "%Y-%m-%d")
    (date . "%B %d,%Y")
    (date_short . "%b %d,%Y")
    (date_long . "%A %B %d,%Y")
    (date_pretty . "%B %d,%Y")
    (date_short_pretty . "%b %d,%Y")
    (date_long_pretty . "%A %B %d,%Y")
    (time . "%H:%M")
    (time_secs . "%H:%M:%S"))
  "Date formats for Slack's date token.
this format string passed to `format-time-string' function.
see \"Formatting dates\" section in https://api.slack.com/docs/message-formatting"
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

(defmethod slack-message-header ((m slack-message) team)
  (slack-message-sender-name m team))

(defmethod slack-message-starred-p ((m slack-message))
  (oref m is-starred))

(defmethod slack-message-starred-p ((m slack-file-message))
  (oref (oref m file) is-starred))

(defmethod slack-message-starred-str ((m slack-message))
  (if (slack-message-starred-p m)
      ":star:"
    ""))

(defun slack-format-message (&rest args)
  (let ((messages args))
    (mapconcat #'identity
               (cl-remove-if #'(lambda (e) (< (length e) 1)) messages)
               "\n")))

(defmethod slack-message-profile-image ((m slack-message) team)
  (slack-user-image (slack-user-find m team) team))

(defmethod slack-message-header-with-image ((m slack-message) header team)
  (let ((image (slack-message-profile-image m team)))
    (if image
        (format "%s %s" (propertize "image"
                                    'display image
                                    'face 'slack-profile-image-face)
                header)
      header)))

(defun slack-message-header-to-string (m team)
  (let ((header (format "%s %s"
                        (slack-message-put-header-property
                         (slack-message-header m team))
                        (slack-message-starred-str m))))
    (if (slack-team-display-profile-imagep team)
        (slack-message-header-with-image m header team)
      header)))

(defmethod slack-message-body-to-string ((m slack-message) team)
  (let ((raw-body (slack-message-body m team)))
    (if (oref m deleted-at)
        (slack-message-put-deleted-property raw-body)
      (slack-message-put-text-property raw-body))))


(defun slack-format-reactions (reactions team)
  (concat "\n"
          (mapconcat #'(lambda (r) (slack-reaction-to-string r team))
                     reactions
                     " ")))

(defmethod slack-message-reaction-to-string ((m slack-message) team)
  (let ((reactions (slack-message-reactions m)))
    (when reactions
      (slack-format-reactions reactions team))))

(defmethod slack-message-to-string ((m slack-message) team)
  (let ((text (if (slot-boundp m 'text) (oref m text))))
    (let* ((header (slack-message-header-to-string m team))
           (attachment-body (slack-message-attachment-body m team))
           (body (slack-message-body-to-string m team))
           (reactions (slack-message-reaction-to-string m team))
           (thread (slack-thread-to-string m team)))
      (slack-format-message header body attachment-body reactions thread))))

(defmethod slack-message-body ((m slack-message) team)
  (with-slots (text) m
    (slack-message-unescape-string text team)))

(defmethod slack-message-body ((m slack-reply-broadcast-message) team)
  (format "Replied to a thread: \n%s"
          (slack-message-unescape-string (oref m text) team)))

(defmethod slack-team-display-image-inlinep ((_m slack-message) team)
  (slack-team-display-attachment-image-inlinep team))

(defmethod slack-message-attachment-body ((m slack-message) team)
  (with-slots (attachments) m
    (let ((body (mapconcat #'(lambda (attachment)
                               (slack-message-to-string attachment team))
                           attachments "\n\t-\n")))
      (if (< 0 (length body))
          (slack-message-unescape-string (format "\n%s" body) team)))))

(defmethod slack-message-to-alert ((m slack-message) team)
  (with-slots (text attachments) m
    (if (and text (< 0 (length text)))
        (slack-message-unescape-string text team)
      (let ((attachment-string (mapconcat #'slack-attachment-to-alert
                                          attachments " ")))
        (slack-message-unescape-string attachment-string team)))))

(defun slack-message-unescape-string (text team)
  (when text
    (let* ((and-unescpaed
            (replace-regexp-in-string "&amp;" "&" text))
           (lt-unescaped
            (replace-regexp-in-string "&lt;" "<" and-unescpaed))
           (gt-unescaped
            (replace-regexp-in-string "&gt;" ">" lt-unescaped)))
      (slack-message-unescape-date-format
       (slack-message-unescape-command
        (slack-message-unescape-user-id
         (slack-message-unescape-channel gt-unescaped team)
         team))))))

(defun slack-message-unescape-user-id (text team)
  (let ((user-regexp "<@\\(U.*?\\)>"))
    (cl-labels ((unescape-user-id
                 (text)
                 (concat "@" (or
                              (slack-message-replace-user-name text)
                              (let ((user (slack-user--find (match-string 1 text) team)))
                                (plist-get user :name))
                              (match-string 1 text)))))
      (replace-regexp-in-string user-regexp
                                #'unescape-user-id
                                text t t))))

(defun slack-message-replace-user-name (text)
  (let ((user-name-regexp "<@U.*?|\\(.*?\\)>"))
    (cl-labels ((replace-user-id-with-name (text)
                                           (match-string 1 text)))
      (if (string-match-p user-name-regexp text)
          (replace-regexp-in-string user-name-regexp
                                    #'replace-user-id-with-name
                                    text nil t)))))

(defun slack-message-unescape-date-format (text)
  (let ((date-regexp "<!date^\\([[:digit:]]*\\)^\\(.*?\\)\\(\\^.*\\)?|\\(.*\\)>")
        (time-format-regexp "{\\(.*?\\)}"))
    (cl-labels
        ((unescape-date-string
          (text)
          (let* ((time (match-string 1 text))
                 (format-string (match-string 2 text))
                 (link (match-string 3 text))
                 (fallback (match-string 4 text)))
            (replace-regexp-in-string time-format-regexp
                                      #'(lambda (text)
                                          (unescape-datetime-format time
                                                                    link
                                                                    text
                                                                    fallback))
                                      format-string)))
         (unescape-datetime-format
          (unix-time link text fallback)
          (let* ((match (match-string 1 text))
                 (template (cl-assoc (intern match) slack-date-formats)))
            (if template
                (slack-linkfy
                 (format-time-string (cdr template)
                                     (float-time (string-to-number unix-time)))
                 (and link (substring link 1 (length link))))
              fallback))))
      (replace-regexp-in-string date-regexp
                                #'unescape-date-string
                                text nil t))))

(defun slack-message-unescape-command (text)
  (let ((command-regexp "<!\\(.*?\\)>"))
    (cl-labels ((unescape-command
                 (text)
                 (let ((match (match-string 1 text)))
                   (if (string-prefix-p "date" match)
                       (format "<!%s>" match)
                     (concat "@" match)))))
      (replace-regexp-in-string command-regexp
                                #'unescape-command
                                text nil t))))

(defun slack-message-unescape-channel (text team)
  (let ((channel-regexp "<#\\(C.*?\\)\\(|.*?\\)?>"))
    (cl-labels ((unescape-channel
                 (text)
                 (let ((name (match-string 2 text))
                       (id (match-string 1 text)))
                   (concat "#" (or (and name (substring name 1))
                                   (slack-if-let* ((room (slack-room-find id team)))
                                       (oref room name)
                                     id))))))
      (replace-regexp-in-string channel-regexp
                                #'unescape-channel
                                text t))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
