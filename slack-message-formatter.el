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
(require 'slack-usergroup)
(require 'slack-thread)
(require 'slack-file)
(require 'slack-attachment)
(require 'slack-image)

(defvar slack-current-buffer)
(defvar slack-channel-button-keymap)
(defvar slack-expand-email-keymap)

(defface slack-profile-image-face
  '((t ()))
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

(defface slack-channel-button-face
  '((t (:underline t :foreground "cyan")))
  "Face used to channel button."
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

(cl-defmethod slack-message-header ((m slack-message) team)
  (slack-message-sender-name m team))

(cl-defmethod slack-message-starred-p ((m slack-message))
  (oref m is-starred))

(cl-defmethod slack-message-starred-str ((m slack-message))
  (if (slack-message-starred-p m)
      ":star:"
    ""))

(defun slack-format-message (&rest args)
  (let ((messages args))
    (mapconcat #'identity
               (cl-remove-if #'(lambda (e) (< (length e) 1)) messages)
               "\n")))

(cl-defmethod slack-message-profile-image ((m slack-message) team)
  (slack-user-image (slack-user-find m team) team))

(cl-defmethod slack-message-header-with-image ((m slack-message) header team)
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

(cl-defmethod slack-message-body-to-string ((m slack-message) team)
  (let ((raw-body (slack-message-body m team)))
    (if (oref m deleted-at)
        (slack-message-put-deleted-property raw-body)
      (slack-message-put-text-property raw-body))))


(cl-defmethod slack-message-reaction-to-string ((m slack-message))
  (let ((reactions (slack-message-reactions m)))
    (when reactions
      (mapconcat #'slack-reaction-to-string
                 reactions
                 " "))))

(cl-defmethod slack-message-to-string ((m slack-message) team)
  (let* ((header (slack-message-header-to-string m team))
         (attachment-body (slack-message-attachment-body m team))
         (body (slack-message-body-to-string m team))
         (files (mapconcat #'(lambda (file)
                               (format "%s\n"
                                       (slack-message-to-string file
                                                                (slack-ts m)
                                                                team)))
                           (oref m files) "\n"))
         (reactions (slack-message-reaction-to-string m))
         (thread (slack-thread-to-string m team)))
    (slack-format-message (propertize header
                                      'slack-message-header t)
                          body
                          files
                          attachment-body reactions thread)))

(cl-defmethod slack-message-body ((m slack-message) team)
  (with-slots (text) m
    (slack-message-unescape-string text team)))

(cl-defmethod slack-message-body ((m slack-reply-broadcast-message) team)
  (format "Replied to a thread: \n%s"
          (slack-message-unescape-string (oref m text) team)))

(cl-defmethod slack-message-body-to-string ((m slack-file-comment-message) team)
  (with-slots (file comment deleted-at) m
    (let ((commented-user (slack-user-name (plist-get comment :user)
                                           team))
          (comment-body (plist-get comment :comment))
          (file-id (plist-get file :id))
          (file-user (slack-user-name (plist-get file :user)
                                      team))
          (file-title (plist-get file :title))
          (text-propertize (or
                            (and deleted-at
                                 #'slack-message-put-deleted-property)
                            #'slack-message-put-text-property)))
      (format "%s %s: %s"
              (funcall text-propertize
                       (format "@%s commented on @%s's file"
                               commented-user
                               file-user))
              (slack-file-link-info file-id file-title)
              (funcall text-propertize
                       comment-body)))))

(cl-defmethod slack-team-display-image-inlinep ((_m slack-message) team)
  (slack-team-display-attachment-image-inlinep team))

(cl-defmethod slack-message-attachment-body ((m slack-message) team)
  (with-slots (attachments) m
    (let ((body (mapconcat #'(lambda (attachment)
                               (slack-message-to-string attachment team))
                           attachments "\n\t-\n")))
      (if (< 0 (length body))
          (slack-message-unescape-string (format "\n%s" body) team)))))

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

(defun slack-unescape-!date (text)
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

(defun slack-unescape-!subteam (text)
  (let ((regexp "<!subteam^\\(.*?\\)|\\(.*?\\)>"))
    (cl-labels
        ((replace (text)
                  (match-string 2 text)))
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
            (if (string= "here|here" match)
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
            )))
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
  (let ((user-regexp "<@\\(U.*?\\)\\(|.*?\\)?>"))
    (cl-labels ((replace
                 (text)
                 (let ((user-id (match-string 1 text))
                       (label (match-string 2 text)))
                   (concat "@" (or (and label (substring label 1))
                                   (slack-if-let* ((user (slack-user--find user-id team)))
                                       (plist-get user :name)
                                     (slack-log (format "User not found. ID: %S" user-id) team)
                                     "<Unknown USER>"))))))
      (replace-regexp-in-string user-regexp
                                #'replace
                                text t t))))

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
  (format "RAW: %s\nROOM: %s\nMESSAGE: %s\nATTACHMENTS: %s - %s\nFILES: %s - %s"
          (oref this text)
          (oref room id)
          (eieio-object-class this)
          (length (oref this attachments))
          (mapcar (lambda (e) (format "\n(TITLE: %s\nPRETEXT: %s\nTEXT: %s)"
                                      (slack-unescape-channel
                                       (oref e title)
                                       team)
                                      (oref e pretext)
                                      (oref e text)))
                  (oref this attachments))
          (length (oref this files))
          (mapcar (lambda (e) (format "(TITLE: %s)"
                                      (oref e title)))
                  (oref this files))))

(cl-defmethod slack-file-gdoc-to-string ((this slack-file))
  (with-slots (pretty-type name title url-private permalink) this
    (let ((title (propertize (format "<%s|%s>" permalink (or title name))
                             'face '(:weight bold)))
          (description (format "<%s|%s>" url-private pretty-type)))
      (slack-format-message title description))))

(cl-defmethod slack-message-body-to-string ((file slack-file) _team)
  (cond
   ((slack-file-gdoc-p file) (slack-file-gdoc-to-string file))
   (t (with-slots (name title size filetype permalink) file
        (slack-message-put-text-property
         (format "name: %s\nsize: %s\ntype: %s\n%s\n"
                 (or title name) size filetype permalink))))))

(cl-defmethod slack-message-to-string ((this slack-file-email) ts team)
  (let ((body (slack-file-summary this ts team))
        (thumb (slack-image-string (slack-file-thumb-image-spec this))))
    (slack-format-message body thumb)))

(cl-defmethod slack-message-to-string ((this slack-file) ts team)
  (let ((body (slack-file-summary this ts team))
        (thumb (slack-image-string (slack-file-thumb-image-spec this))))
    (slack-format-message body thumb)))

(cl-defmethod slack-message-to-string ((this slack-file-comment) team)
  (with-slots (user comment) this
    (let ((name (slack-user-name user team))
          (status (slack-user-status user team)))
      (format "%s\n%s\n"
              (propertize (format "%s %s" name status)
                          'face 'slack-message-output-header)
              (slack-message-unescape-string comment team)))))

(cl-defmethod slack-file-summary ((file slack-file) _ts team)
  (with-slots (mode pretty-type mimetype permalink name title) file
    (if (string= mode "tombstone")
        "This file was deleted."
      (format "uploaded this %s: %s <%s|open in browser>"
              (or pretty-type mimetype)
              (slack-file-link-info (oref file id)
                                    (slack-message-unescape-string (or title name)
                                                                   team))
              permalink))))

(cl-defmethod slack-file-summary ((this slack-file-email) ts team)
  (with-slots (preview-plain-text plain-text is-expanded) this
    (let* ((has-more (< (length preview-plain-text)
                        (length plain-text)))
           (body (slack-message-unescape-string
                  (or (and is-expanded plain-text)
                      (or (and has-more (format "%s…" preview-plain-text))
                          preview-plain-text))
                  team)))
      (format "%s\n\n%s\n\n%s"
              (cl-call-next-method)
              (propertize body
                          'slack-defer-face #'slack-put-preview-overlay)
              (propertize (or (and is-expanded "Collapse ↑")
                              "+ Click to expand inline")
                          'ts ts
                          'id (oref this id)
                          'face '(:underline t)
                          'keymap slack-expand-email-keymap)))))

(cl-defmethod slack-message-body-to-string ((this slack-file-email) _team)
  (let ((from (format "From: %s" (mapconcat #'(lambda (e) (oref e original))
                                            (oref this from)
                                            ", ")))
        (to (format "To: %s" (mapconcat #'(lambda (e) (oref e original))
                                        (oref this to)
                                        ", ")))
        (cc (format "CC: %s" (mapconcat #'(lambda (e) (oref e original))
                                        (oref this cc)
                                        ", ")))
        (subject (format "Subject: %s" (oref this subject)))
        (body (propertize (format "\n%s" (oref this plain-text))
                          'slack-defer-face #'slack-put-email-body-overlay))
        (date (format "Date: %s" (slack-message-time-to-string (oref this created)))))
    (mapconcat #'identity
               (list from to cc subject date body)
               "\n")))

(cl-defmethod slack-message-to-string ((attachment slack-attachment) team)
  (with-slots
      (fallback text ts color from-url footer fields pretext actions) attachment
    (let* ((pad-raw (propertize "|" 'face 'slack-attachment-pad))
           (pad (or (and color (propertize pad-raw 'face (list :foreground (concat "#" color))))
                    pad-raw))
           (header-raw (slack-attachment-header attachment))
           (header (and (not (slack-string-blankp header-raw))
                        (format "%s\t%s" pad
                                (propertize header-raw
                                            'face 'slack-attachment-header))))
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
                                      (format "\t%s\t" pad))))
      (slack-message-unescape-string
       (slack-format-message
        (or (and header (format "\t%s\n" header)) "")
        (or (and pretext (format "\t%s\n" pretext)) "")
        (or (and body (format "\t%s" body)) "")
        (or (and fields fields) "")
        (or (and actions (format "\t%s" actions)) "")
        (or (and footer (format "\n\t%s" footer)) "")
        (or (and image (< 0 (length image))
                 (format "\n\t%s\t%s" pad image)) ""))
       team))))

(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
