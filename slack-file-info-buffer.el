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
  (lui-set-prompt (format "Add Comment %s" lui-prompt-string))
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (setq lui-input-function 'slack-file-comment--add))

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
          (slack-buffer-insert this))
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
    (let* ((header (slack-message-header-to-string file team))
           (body (slack-message-body-to-string file team))
           (thumb (or (and (slack-file-image-p file)
                           (slack-message-large-image-to-string file))
                      (slack-message-image-to-string file)))
           (reactions (slack-message-reaction-to-string file team))

           (comments-count
            (slack-file-comments-count-to-string file)))
      (propertize (slack-format-message header
                                        body
                                        thumb
                                        reactions
                                        comments-count)
                  'file-id (oref file id)))))

(defmethod slack-buffer-insert ((this slack-file-info-buffer))
  (delete-region (point-min) lui-output-marker)
  (with-slots (file team) this
    (let ((lui-time-stamp-position nil))
      (lui-insert "" t))

    (lui-insert-with-text-properties
     (slack-buffer-file-to-string this)
     ;; saved-text-properties not working??
     'file-id (oref file id)
     'ts (slack-ts file))

    (lui-insert "" t)

    (let ((comments (slack-file-comments-to-string file team)))
      (mapc #'(lambda (comment)
                (lui-insert-with-text-properties
                 (slack-message-to-string comment team)
                 'file-comment-id (oref comment id)
                 'ts (slack-ts comment))
                (lui-insert "" t))
            (oref file comments)))))

(defmethod slack-buffer-send-message ((this slack-file-info-buffer) message)
  (with-slots (file team) this
    (slack-file-comment-add-request (oref file id) message team)))

(defmethod slack-buffer-display-message-compose-buffer ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let ((buffer (slack-create-file-comment-compose-buffer file team)))
      (slack-buffer-display buffer))))

(defmethod slack-buffer-add-reaction-to-message
  ((this slack-file-info-buffer) reaction _ts)
  (with-slots (file team) this
    (slack-file-add-reaction (oref file id) reaction team)))

(defmethod slack-buffer-add-reaction-to-file-comment
  ((this slack-file-info-buffer) reaction id)
  (with-slots (team) this
    (slack-file-comment-add-reaction id reaction team)))

(defmethod slack-buffer-remove-reaction-from-message
  ((this slack-file-info-buffer) _ts &optional file-comment-id)
  (with-slots (file team) this
    (if file-comment-id
        (slack-file-comment-remove-reaction file-comment-id
                                            (oref file id)
                                            team)
      (slack-file-remove-reaction (oref file id) team))))

(defmethod slack-buffer-add-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-add-url)
        (file-comment-id (slack-get-file-comment-id)))
    (with-slots (file team) this
      (if file-comment-id
          (slack-with-file-comment file-comment-id file
            (slack-message-star-api-request
             url
             (list (slack-message-star-api-params file-comment))
             team))
        (slack-message-star-api-request url
                                        (list (slack-message-star-api-params file))
                                        team)))))

(defmethod slack-buffer-remove-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-remove-url)
        (file-comment-id (slack-get-file-comment-id)))
    (with-slots (file team) this
      (if file-comment-id
          (slack-with-file-comment file-comment-id file
            (slack-message-star-api-request url
                                            (list (slack-message-star-api-params
                                                   file-comment))
                                            team))
        (slack-message-star-api-request url
                                        (list (slack-message-star-api-params
                                               file))
                                        team)))))

(defmethod slack-buffer-display-edit-message-buffer ((this slack-file-info-buffer) _ts)
  (with-slots (file team) this
    (slack-if-let* ((file-comment-id (slack-get-file-comment-id)))
        (let ((buf (slack-create-edit-file-comment-buffer
                    file
                    (slack-get-file-comment-id)
                    team)))
          (slack-buffer-display buf)))))

(defmethod slack-buffer-delete-message ((this slack-file-info-buffer) _ts)
  (with-slots (file team) this
    (slack-if-let* ((file-comment-id (slack-get-file-comment-id)))
        (if (yes-or-no-p "Are you sure want to delete this comment?")
            (slack-with-file-comment file-comment-id file
              (slack-file-comment-delete-request (oref file id)
                                                 file-comment-id
                                                 team))
          (message "Canceled")))))

(defmethod slack-buffer--replace ((this slack-file-info-buffer) _ts)
  (slack-if-let* ((buffer (get-buffer (slack-buffer-name this))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (slack-buffer-insert this)))))

(defmethod slack-buffer-update ((this slack-file-info-buffer)
                                &optional file-comment-id)
  (with-slots (file team) this
    (let ((buffer (get-buffer (slack-buffer-name this))))
      (with-current-buffer buffer
        (if file-comment-id
            (let* ((comment (slack-find-file-comment file file-comment-id)))
              (cl-labels
                  ((pred () (let ((id (get-text-property (point) 'file-comment-id)))
                              (equal id file-comment-id))))
                (if comment
                    (lui-replace (slack-message-to-string comment team) #'pred)
                  (lui-delete #'pred))))
          (lui-replace
           (slack-buffer-file-to-string this)
           #'(lambda () (let ((id (get-text-property (point) 'file-id)))
                          (equal id (oref file id))))))))))

(defmethod slack-buffer-insert-file-comment ((this slack-file-info-buffer) comment-id)
  (with-slots (file team) this
    (let ((buffer (get-buffer (slack-buffer-name this)))
          (comment (slack-find-file-comment file comment-id)))
      (with-current-buffer buffer
        (lui-insert-with-text-properties
         (slack-message-to-string comment team)
         'file-comment-id (oref comment id)
         'ts (slack-ts comment))))))

(provide 'slack-file-info-buffer)
;;; slack-file-info-buffer.el ends here
