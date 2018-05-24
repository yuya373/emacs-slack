;;; slack-attachment.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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

(defclass slack-attachment ()
  ((fallback :initarg :fallback :initform nil)
   (title :initarg :title :initform nil)
   (title-link :initarg :title_link :initform nil)
   (pretext :initarg :pretext :initform nil)
   (text :initarg :text :initform nil)
   (author-name :initarg :author_name :initform nil)
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :initform '())
   (image-url :initarg :image_url :initform nil)
   (image-width :initarg :image_width :initform nil)
   (image-height :initarg :image_height :initform nil)
   (thumb-url :initarg :thumb_url)
   (is-share :initarg :is_share :initform nil)
   (footer :initarg :footer :initform nil)
   (color :initarg :color :initform nil)
   (ts :initarg :ts :initform nil)
   (author-subname :initarg :author_subname :initform nil)))

(defclass slack-shared-message (slack-attachment)
  ((channel-id :initarg :channel_id :initform nil)
   (channel-name :initarg :channel_name :initform nil)
   (from-url :initarg :from_url :initform nil)))

(defmethod slack-image-spec ((this slack-attachment))
  (with-slots (image-url image-height image-width) this
    (when image-url
      (list image-url image-width image-height slack-image-max-height))))

(defmethod slack-message-to-string ((attachment slack-attachment) team)
  (with-slots
      (fallback text ts color from-url footer fields pretext) attachment
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
           (footer (if footer
                       (format "%s\t%s"
                               pad
                               (propertize
                                (format "%s%s" footer
                                        (or (and ts (format "|%s" (slack-message-time-to-string ts)))
                                            ""))
                                'face 'slack-attachment-footer))))
           (image (slack-image-string (slack-image-spec attachment))))
      (slack-message-unescape-string
       (if (and (slack-string-blankp header)
                (slack-string-blankp pretext)
                (slack-string-blankp body)
                (slack-string-blankp fields)
                (slack-string-blankp footer))
           fallback
         (format "%s%s%s%s%s%s"
                 (or (and header (format "\t%s\n" header)) "")
                 (or (and pretext (format "\t%s\n" pretext)) "")
                 (or (and body (format "\t%s" body)) "")
                 (or (and fields fields) "")
                 (or (and footer (format "\n\t%s" footer)) "")
                 (or (and image (format "\n%s" image)) "")))
       team))))

(defmethod slack-attachment-header ((attachment slack-attachment))
  (with-slots (title title-link author-name author-subname) attachment
    (concat (or (and title title-link (slack-linkfy title title-link))
                title
                "")
            (or author-name author-subname ""))))

(defmethod slack-attachment-field-to-string ((field slack-attachment-field) &optional pad)
  (unless pad (setq pad ""))
  (let ((title (propertize (or (oref field title) "") 'face 'slack-attachment-field-title))
        (value (mapconcat #'(lambda (e) (format "\t%s" e))
                          (split-string (or (oref field value) "") "\n")
                          (format "\n%s\t" pad))))
    (format "%s\t%s\n%s\t%s" pad title pad value)))

(defmethod slack-attachment-to-alert ((a slack-attachment))
  (with-slots (title fallback pretext) a
    (if (and title (< 0 (length title)))
        title
      (if (and pretext (< 0 (length pretext)))
          (format "%s\n%s" pretext fallback)
        fallback))))

(provide 'slack-attachment)
;;; slack-attachment.el ends here
