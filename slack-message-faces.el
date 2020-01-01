;;; slack-message-faces.el ---                       -*- lexical-binding: t; -*-

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
(require 'color)

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

(defface slack-preview-face
  (let* ((default-bg (or (face-background 'default) "unspecified-bg"))
         (light-bg (if (equal default-bg "unspecified-bg")
                       "unspecified-bg"
                     (color-darken-name default-bg 3)))
         (dark-bg (if (equal default-bg "unspecified-bg")
                      "unspecified-bg"
                    (color-lighten-name default-bg 3))))
    `((default :inherit (fixed-pitch shadow) :slant normal :weight normal)
      (((type graphic) (class color) (background dark)) (:background ,dark-bg))
      (((type graphic) (class color) (background light)) (:background ,light-bg))))
  "Used preview text and code blocks"
  :group 'slack)

(defun slack-message-put-header-property (header)
  (if header
      (propertize header 'face 'slack-message-output-header)))

(defun slack-message-put-text-property (text)
  (if text
      (propertize text 'face 'slack-message-output-text)))

(defun slack-message-put-deleted-property (text)
  (if text
      (propertize text 'face 'slack-message-deleted-face)))

(defun slack-put-preview-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'slack-preview-face)))

(defalias 'slack-put-email-body-overlay 'slack-put-preview-overlay)
(defalias 'slack-put-code-block-overlay 'slack-put-preview-overlay)

(provide 'slack-message-faces)
;;; slack-message-faces.el ends here
