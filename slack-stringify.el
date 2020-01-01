;;; slack-stringify.el ---                           -*- lexical-binding: t; -*-

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
(require 'eieio)
(require 'slack-reaction)
(require 'slack-room-buffer)
(require 'slack-file)

(defvar slack-reaction-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-reaction-toggle)
    (define-key keymap [mouse-1] #'slack-reaction-toggle)
    keymap))

(cl-defmethod slack-reaction-to-string ((r slack-reaction))
  (propertize (format " :%s: %d " (oref r name) (oref r count))
              'face 'slack-message-output-reaction
              'mouse-face 'highlight
              'keymap slack-reaction-keymap
              'reaction r
              'help-echo #'slack-reaction-help-echo))

(cl-defmethod slack-file-body-to-string ((file slack-file))
  (let* ((url (oref file url-private))
         (type (slack-file-type file))
         (size (slack-file-size file))
         (title (slack-file-title file)))
    (slack-format-message (propertize (format "<%s|%s>" url title)
                                      'face '(:weight bold))
                          (format "%s%s"
                                  (or (and size (format "%s " size)) "")
                                  type))))

(cl-defmethod slack-file-body-to-string ((this slack-file-email))
  (let* ((label-face '(:foreground "#586e75" :weight bold))
         (from (format "%s %s"
                       (propertize "From:" 'face label-face)
                       (mapconcat #'(lambda (e) (oref e original))
                                  (oref this from)
                                  ", ")))
         (to (format "%s %s"
                     (propertize "To:" 'face label-face)
                     (mapconcat #'(lambda (e) (oref e original))
                                (oref this to)
                                ", ")))
         (cc (format "%s %s"
                     (propertize "CC:" 'face label-face)
                     (mapconcat #'(lambda (e) (oref e original))
                                (oref this cc)
                                ", ")))
         (subject (format "%s %s"
                          (propertize "Subject:" 'face label-face)
                          (propertize (oref this subject)
                                      'face '(:weight bold :height 1.1))))
         (body (propertize (format "\n%s" (oref this plain-text))
                           'slack-defer-face #'slack-put-email-body-overlay))
         (date (format "%s %s"
                       (propertize "Date:" 'face label-face)
                       (slack-format-ts (oref this created)))))
    (mapconcat #'identity
               (list from to cc subject date "" body)
               "\n")))

(cl-defmethod slack-message-to-string ((this slack-file) ts team)
  (if (slack-file-hidden-by-limit-p this)
      (slack-file-hidden-by-limit-message this)
    (let ((body (slack-file-summary this ts team))
          (thumb (slack-image-string (slack-file-thumb-image-spec this))))
      (slack-format-message body thumb))))

(cl-defmethod slack-message-to-string ((this slack-file-comment) team)
  (with-slots (user comment) this
    (let ((name (slack-user-name user team))
          (status (slack-user-status user team)))
      (format "%s\n%s\n"
              (propertize (format "%s %s" name status)
                          'face 'slack-message-output-header)
              (slack-unescape comment team)))))

(cl-defmethod slack-file-summary ((file slack-file) _ts team)
  (with-slots (mode permalink) file
    (if (string= mode "tombstone")
        "This file was deleted."
      (let ((type (slack-file-type file))
            (title (slack-file-title file)))
        (format "uploaded this %s: %s <%s|open in browser>"
                type
                (slack-file-link-info (oref file id)
                                      (slack-unescape title team))
                permalink)))))

(cl-defmethod slack-file-summary ((this slack-file-email) ts team)
  (with-slots (preview-plain-text plain-text is-expanded) this
    (let* ((has-more (< (length preview-plain-text)
                        (length plain-text)))
           (body (slack-unescape
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

(cl-defmethod slack-block-to-string ((this slack-button-block-element) &optional _option)
  (with-slots (text style) this
    (let ((face (cond ((string= "danger" style) 'slack-button-danger-block-element-face)
                      ((string= "primary" style) 'slack-button-primary-block-element-face)
                      (t 'slack-button-block-element-face))))
      (propertize (slack-block-to-string text)
                  'face face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET")
                              #'slack-execute-button-block-action)
                            map)))))

(cl-defmethod slack-block-to-string ((this slack-static-select-block-element) &optional _option)
  (with-slots (initial-option placeholder) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-static-select-block-action)
                          map))))

(cl-defmethod slack-block-to-string ((this slack-external-select-block-element))
  (with-slots (placeholder initial-option) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-external-select-block-action)
                          map))))

(cl-defmethod slack-block-to-string ((this slack-user-select-block-element) &optional _option)
  (with-slots (initial-user placeholder) this
    (let ((props (list
                  'face 'slack-select-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-user-select-block-action)
                            map))))
      (if initial-user
          (apply #'propertize (format "USER: %s" initial-user)
                 (append (list 'slack-user-id initial-user
                               'slack-lazy-user-name t)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-to-string ((this slack-conversation-select-block-element) &optional _option)
  (with-slots (initial-conversation placeholder) this
    (let ((props (list 'face 'slack-select-block-element-face
                       'slack-action-payload (slack-block-action-payload this)
                       'keymap (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "RET") #'slack-execute-conversation-select-block-action)
                                 map))))
      (if initial-conversation
          (apply #'propertize (format "CONVERSATION: %s" initial-conversation)
                 (append (list 'slack-conversation-id initial-conversation
                               'slack-lazy-conversation-name t)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-to-string ((this slack-channel-select-block-element) &optional _option)
  (with-slots (placeholder initial-channel) this
    (let ((props (list
                  'face 'slack-select-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-channel-select-block-action)
                            map))))
      (if initial-channel
          (apply #'propertize (format "CHANNEL: %s" initial-channel)
                 (append (list 'slack-lazy-conversation-name t
                               'slack-conversation-id initial-channel)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-to-string ((this slack-overflow-menu-block-element) &optional _option)
  (propertize " … "
              'face 'slack-overflow-block-element-face
              'slack-action-payload (slack-block-action-payload this)
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") #'slack-execute-overflow-menu-block-action)
                        map)))

(cl-defmethod slack-block-to-string ((this slack-date-picker-block-element) &optional _option)
  (with-slots (placeholder initial-date) this
    (let ((text (or initial-date
                    (slack-block-to-string placeholder)
                    "Pick a date")))
      (propertize text
                  'face 'slack-date-picker-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-datepicker-block-action)
                            map)))))

(provide 'slack-stringify)
;;; slack-stringify.el ends here
