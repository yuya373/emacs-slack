;;; slack-file-info-buffer.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-message)

(define-derived-mode slack-file-info-buffer-mode slack-buffer-mode  "Slack File Info"
  (add-hook 'lui-post-output-hook 'slack-display-image t t))

(defclass slack-file-info-buffer (slack-buffer)
  ((file :initarg :file :type slack-file)))

(defmethod slack-buffer-name :static ((class slack-file-info-buffer) file team)
  (format "*Slack - %s File: %s"
          (oref team name)
          (or (oref file title)
              (oref file name)
              (oref file id))))

(defun slack-create-file-info-buffer (team file)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             file
                                             team)))
      (progn
        (oset buffer file file)
        buffer)
    (slack-file-info-buffer :team team :file file)))

(defmethod slack-buffer-init-buffer :after ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let ((class (eieio-object-class-name this)))
      (slack-buffer-push-new-3 class file team))))

(defmethod slack-buffer-name ((this slack-file-info-buffer))
  (with-slots (file team) this
    (slack-buffer-name (eieio-object-class-name this)
                       file
                       team)))

(defmethod slack-buffer-buffer ((this slack-file-info-buffer))
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (slack-buffer-insert this)))
        buf)
    (slack-buffer-init-buffer this)))

(defmethod slack-buffer-init-buffer ((this slack-file-info-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-file-info-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert this))
    buf))

(defmethod slack-buffer-file-to-string ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let* ((header (format "%s %s\n"
                           (slack-message-put-header-property (oref file title))
                           (if (oref file is-starred) ":star:" "")))
           (body (slack-message-body-to-string file team))
           (thumb (or (and (slack-file-image-p file)
                           (slack-message-large-image-to-string file))
                      (slack-message-image-to-string file)))
           (comments (mapconcat #'(lambda (comment)
                                    (slack-message-to-string comment team))
                                (oref file comments)
                                "\n")))
      (propertize (slack-format-message header
                                        body
                                        thumb
                                        "\n"
                                        comments)
                  'file-id (oref file id)))))

(defmethod slack-buffer-insert ((this slack-file-info-buffer))
  (delete-region (point-min) lui-output-marker)
  (with-slots (file team) this
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties
       (slack-buffer-file-to-string this)
       ;; saved-text-properties not working??
       'file-id (oref file id)
       'ts (slack-ts file)))))

(defmethod slack-buffer-add-reaction-to-message
  ((this slack-file-info-buffer) reaction _ts)
  (with-slots (file team) this
    (slack-file-add-reaction (oref file id) reaction team)))

(defmethod slack-buffer-add-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-add-url))
    (with-slots (file team) this
      (slack-message-star-api-request url
                                      (list (slack-message-star-api-params file))
                                      team))))

(defmethod slack-buffer-remove-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-remove-url))
    (with-slots (file team) this
      (slack-message-star-api-request url
                                      (list (slack-message-star-api-params
                                             file))
                                      team))))

(defmethod slack-buffer--replace ((this slack-file-info-buffer) _ts)
  (slack-if-let* ((buffer (get-buffer (slack-buffer-name this))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (slack-buffer-insert this)))))

(defmethod slack-buffer-update ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let ((buffer (get-buffer (slack-buffer-name this))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (slack-buffer-insert this))))))

(provide 'slack-file-info-buffer)
;;; slack-file-info-buffer.el ends here
