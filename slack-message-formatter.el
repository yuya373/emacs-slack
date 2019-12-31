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

(defvar slack-current-buffer)
(defvar slack-channel-button-keymap)

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

(cl-defmethod slack-message-reaction-to-string ((m slack-message))
  (let ((reactions (slack-message-reactions m)))
    (when reactions
      (mapconcat #'slack-reaction-to-string
                 reactions
                 " "))))

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
         (reactions (slack-message-reaction-to-string m))
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
      (slack-message-unescape-string alert-text team))))

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
                            (destructuring-bind (variable label)
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

(defun slack-message-unescape-string (text team)
  (when text
    (slack-unescape-!
     (slack-unescape-@
      (slack-unescape-channel
       (slack-unescape-&<> text)
       team)
      team))))

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
                                                   (oref room name)
                                                 (slack-log (format "Channel not found. ID: %S" id) team)
                                                 "<Unknown CHANNEL>")))
                               'room-id id
                               'keymap slack-channel-button-keymap
                               'slack-defer-face 'slack-channel-button-face
                               ))))
      (replace-regexp-in-string channel-regexp
                                #'unescape-channel
                                text t))))

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
          (mapcar (lambda (e) (format "\n(CLASS: %s\nTITLE: %s\nPRETEXT: %s\nTEXT: %s\nIMAGE: %s\nTHUMBNAIL: %s)"
                                      (eieio-object-class e)
                                      (slack-unescape-channel
                                       (or (oref e title) "")
                                       team)
                                      (oref e pretext)
                                      (oref e text)
                                      (oref e image-url)
                                      (oref e thumb-url)))
                  (oref this attachments))
          (length (oref this files))
          (mapcar (lambda (e) (format "(TITLE: %s)"
                                      (oref e title)))
                  (oref this files))))

(cl-defmethod slack-message-to-string ((attachment slack-attachment) team)
  (with-slots
      (fallback text ts color from-url footer fields pretext actions files) attachment
    (let* ((pad-raw (propertize "|" 'face 'slack-attachment-pad))
           (pad (or (and color (propertize pad-raw 'face (list :foreground (concat "#" color))))
                    pad-raw))
           (mrkdwn-in (oref attachment mrkdwn-in))
           (header (let ((h (slack-attachment-header attachment)))
                     (unless (slack-string-blankp h)
                       (format "%s\t%s" pad h))))
           (pretext (and pretext (format "%s\t%s" pad pretext)))
           (body (and text (format "%s\t%s" pad (mapconcat #'identity
                                                           (split-string text "\n")
                                                           (format "\n\t%s\t" pad)))))
           (fields (if fields (mapconcat #'(lambda (field)
                                             (slack-attachment-field-to-string field
                                                                               (format "\t%s" pad)))
                                         fields
                                         (format "\n\t%s\n" pad))))
           (actions (if actions
                        (format "%s\t%s"
                                pad
                                (mapconcat #'(lambda (action)
                                               (slack-attachment-action-to-string
                                                action
                                                attachment
                                                team))
                                           actions
                                           " "))))
           (footer (if footer
                       (format "%s\t%s"
                               pad
                               (propertize
                                (format "%s%s" footer
                                        (or (and ts (format "|%s" (slack-message-time-to-string ts)))
                                            ""))
                                'face 'slack-attachment-footer))))
           (image (slack-image-string (slack-image-spec attachment)
                                      (format "\t%s\t" pad)))
           (files (when files
                    (format "%s\t%s"
                            pad
                            (mapconcat #'(lambda (file)
                                           (if (slack-file-hidden-by-limit-p file)
                                               (slack-file-hidden-by-limit-message file)
                                             (let* ((title (slack-file-title file))
                                                    (type (slack-file-type file))
                                                    (id (oref file id))
                                                    (footer (format "%s %s"
                                                                    (slack-file-size file)
                                                                    type)))
                                               (format "%s\n\t%s\t%s"
                                                       (slack-file-link-info id title)
                                                       pad
                                                       (propertize footer
                                                                   'face
                                                                   'slack-attachment-footer)))))
                                       files
                                       (format "\n\t%s\n" pad))))))
      (slack-message-unescape-string
       (slack-format-message
        (or (and header (format "\t%s" header)) "")
        (or (and pretext (format "\t%s" (if (cl-find-if #'(lambda (e) (string= "pretext" e))
                                                        mrkdwn-in)
                                            (propertize pretext 'slack-text-type 'mrkdwn)
                                          pretext)))
            "")
        (or (and body (format "\t%s" (if (cl-find-if #'(lambda (e) (string= "text" e))
                                                     mrkdwn-in)
                                         (propertize body 'slack-text-type 'mrkdwn)
                                       body)))
            "")
        (or (and fields fields) "")
        (or (and actions (format "\t%s" actions)) "")
        (or (and files (format "\t%s" files)) "")
        (or (and footer (format "\t%s" footer)) "")
        (or (and image (< 0 (length image))
                 (format "\n\t%s\t%s" pad image)) ""))
       team))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
