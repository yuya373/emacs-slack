;;; slack-unescape.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-util)
(require 'slack-log)
(require 'slack-user)
(require 'slack-room)

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

(defvar slack-channel-button-keymap)
(defvar slack-message-user-regexp "<@\\([WU].*?\\)\\(|.*?\\)?>")

(defun slack-unescape-&<> (text)
  (cl-labels
      ((replace (text)
                (cond
                 ((match-string 1 text) "&")
                 ((match-string 2 text) "<")
                 ((match-string 3 text) ">"))))
    (replace-regexp-in-string "\\(&amp;\\)\\|\\(&lt;\\)\\|\\(&gt;\\)"
                              #'replace text t t)))

(defun slack-unescape-!date (text &optional zone)
  (let ((date-regexp "<!date^\\([[:digit:]]*\\)^\\(.*?\\)\\(\\^.*\\)?|\\(.*?\\)>")
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
                                     (float-time (string-to-number unix-time))
                                     zone)
                 (and link (substring link 1 (length link))))
              fallback))))
      (replace-regexp-in-string date-regexp
                                #'unescape-date-string
                                text nil t))))

(defun slack-unescape-!subteam (text)
  (let ((regexp "<!subteam^\\(.*?\\)|\\(.*?\\)>"))
    (cl-labels
        ((replace (text)
                  (propertize (match-string 2 text)
                              'slack-defer-face
                              'slack-message-mention-keyword-face)))
      (replace-regexp-in-string regexp #'replace text t t))))

(defun slack-unescape-variable (text)
  (let ((regexp "<!\\(.*?\\)>")
        (commands '("channel" "here" "everyone")))
    (cl-labels
        ((split-by-|
          (text)
          (let ((variable nil) (label nil) (found nil))
            (cl-loop for c in (string-to-list text)
                     do (if (eq ?| c) (setq found t)
                          (if found
                              (push c label)
                            (push c variable))))
            (list variable label)))
         (replace
          (text)
          (let ((match (match-string 1 text)))
            (propertize (if (string= "here|here" match)
                            "@here"
                          (slack-if-let*
                              ((command (cl-find match commands :test #'string=)))
                              (format "@%s" command)
                            (cl-destructuring-bind (variable label)
                                (split-by-| match)
                              (or (and label (format "<%s>"
                                                     (mapconcat #'char-to-string
                                                                (reverse label)
                                                                "")))
                                  (format "<%s>"
                                          (mapconcat #'char-to-string
                                                     (reverse variable)
                                                     ""))))))
                        'slack-defer-face
                        'slack-message-mention-keyword-face))))
      (replace-regexp-in-string regexp #'replace text t t))))

(defun slack-unescape-! (text)
  (slack-unescape-variable
   (slack-unescape-!date
    (slack-unescape-!subteam text))))

(defun slack-unescape-@ (text team)
  (cl-labels ((replace
               (text)
               (let* ((user-id (match-string 1 text))
                      (label (match-string 2 text))
                      (face (if (string= user-id (oref team self-id))
                                'slack-message-mention-me-face
                              'slack-message-mention-face)))
                 (propertize
                  (concat "@" (or (slack-if-let* ((user (slack-user--find user-id
                                                                          team)))
                                      (slack-user--name user team)
                                    (progn
                                      (slack-log (format "User not found. ID: %S" user-id) team)
                                      nil))
                                  (and label (substring label 1))
                                  "<Unknown USER>"))
                  'slack-defer-face face))))
    (replace-regexp-in-string slack-message-user-regexp
                              #'replace
                              text t t)))

(defun slack-unescape-channel (text team)
  (let ((channel-regexp "<#\\(C.*?\\)\\(|.*?\\)?>"))
    (cl-labels ((unescape-channel
                 (text)
                 (let ((name (match-string 2 text))
                       (id (match-string 1 text)))
                   (propertize (concat "#" (or (and name (substring name 1))
                                               (slack-if-let* ((room (slack-room-find id team)))
                                                   (slack-room-name room team)
                                                 (slack-log (format "Channel not found. ID: %S" id) team)
                                                 "<Unknown CHANNEL>")))
                               'room-id id
                               'keymap slack-channel-button-keymap
                               'slack-defer-face 'slack-channel-button-face
                               ))))
      (replace-regexp-in-string channel-regexp
                                #'unescape-channel
                                text t))))

(defun slack-unescape (text team)
  (when text
    (if (< 0 (length text))
        (slack-unescape-!
         (slack-unescape-@
          (slack-unescape-channel
           (slack-unescape-&<> text)
           team)
          team))
      text)))

(provide 'slack-unescape)
;;; slack-unescape.el ends here
