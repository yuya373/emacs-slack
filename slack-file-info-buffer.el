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
(require 'slack-file)
(require 'slack-message-formatter)
(require 'slack-message-reaction)

(defvar slack-file-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-file-display)
    (define-key map [mouse-1] #'slack-file-display)
    map))

(define-derived-mode slack-file-info-buffer-mode slack-buffer-mode  "Slack File Info"
  (setq-local lui-max-buffer-size nil)
  (add-hook 'lui-post-output-hook 'slack-display-image t t))

(defclass slack-file-info-buffer (slack-buffer)
  ((file :initarg :file :type slack-file)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-file-info-buffer)) file team)
  (format "*Slack - %s File: %s"
          (oref team name)
          (or (slack-file-title file)
              (oref file id))))

(cl-defmethod slack-buffer-display-file ((this slack-buffer) file-id)
  (with-slots (team) this
    (cl-labels
        ((open (file &rest _args)
               (slack-buffer-display
                (slack-create-file-info-buffer team file))))
      (slack-file-request-info file-id 1 team #'open))))

(defun slack-create-file-info-buffer (team file)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             file
                                             team)))
      (progn
        (oset buffer file file)
        buffer)
    (slack-file-info-buffer :team team :file file)))

(cl-defmethod slack-buffer-init-buffer :after ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let ((class (eieio-object-class-name this)))
      (slack-buffer-push-new-3 class file team))))

(cl-defmethod slack-buffer-name ((this slack-file-info-buffer))
  (with-slots (file team) this
    (slack-buffer-name (eieio-object-class-name this)
                       file
                       team)))

(cl-defmethod slack-buffer-buffer ((this slack-file-info-buffer))
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (slack-buffer-insert this)))
        buf)
    (slack-buffer-init-buffer this)))

(cl-defmethod slack-buffer-init-buffer ((this slack-file-info-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-file-info-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert this))
    buf))

(cl-defmethod slack-buffer-download-file ((this slack-file-info-buffer) file-id)
  (slack-if-let* ((team (oref this team))
                  (file (slack-file-find file-id team)))
      (slack-file-download file team)))

(cl-defmethod slack-buffer-run-file-action ((this slack-file-info-buffer) file-id)
  (slack-if-let* ((team (oref this team))
                  (file (slack-file-find file-id team)))
      (slack-file-run-action file this)))

(cl-defmethod slack-buffer-file-content-to-string ((this slack-file-info-buffer))
  (with-slots (file) this
    (slack-if-let* ((content (oref file content))
                    (html (oref content content-highlight-html))
                    (css (oref content content-highlight-css)))
        (propertize (concat "<style>\n" css "</style>" "\n" html)
                    'slack-file-html-content t)
      "")))

(cl-defmethod slack-buffer-file-to-string ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let* ((user-name (slack-user-name (oref file user) team))
           (header (format "%s %s %s%s"
                           (propertize user-name
                                       'face '(:weight bold))
                           (if (oref file is-starred) ":star:" "")
                           (if (slack-file-downloadable-p file)
                               (format "%s "
                                       (slack-file-download-button file))
                             "")
                           (slack-file-action-button file)))
           (timestamp (and (oref file timestamp)
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                               (seconds-to-time
                                                (oref file timestamp)))))
           (body (slack-file-body-to-string file))
           (content (slack-buffer-file-content-to-string this))
           (thumb (or (and (slack-file-image-p file)
                           (slack-message-large-image-to-string file))
                      (slack-message-image-to-string file)))
           (comments (mapconcat #'(lambda (comment)
                                    (slack-message-to-string comment team))
                                (oref file comments)
                                "\n")))
      (propertize (slack-format-message header
                                        timestamp
                                        " "
                                        body
                                        " "
                                        content
                                        " "
                                        thumb
                                        " "
                                        comments)
                  'file-id (oref file id)))))

(cl-defmethod slack-buffer-insert ((this slack-file-info-buffer))
  (delete-region (point-min) lui-output-marker)
  (with-slots (file team) this
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties
       (slack-buffer-file-to-string this)
       ;; saved-text-properties not working??
       'file-id (oref file id)
       'ts (slack-ts file))))
  (slack-if-let* ((html-beg (cl-loop for i from (point-min) to lui-output-marker
                                     if (get-text-property i 'slack-file-html-content)
                                     return i))
                  (html-end (next-single-property-change html-beg
                                                         'slack-file-html-content))
                  (inhibit-read-only t))
      (shr-render-region html-beg html-end))
  (goto-char (point-min)))

(cl-defmethod slack-buffer-add-reaction-to-message
  ((this slack-file-info-buffer) reaction _ts)
  (with-slots (file team) this
    (slack-file-add-reaction (oref file id) reaction team)))

(cl-defmethod slack-buffer-add-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-add-url))
    (with-slots (file team) this
      (slack-message-star-api-request url
                                      (list (slack-message-star-api-params file))
                                      team))))

(cl-defmethod slack-buffer-remove-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-remove-url))
    (with-slots (file team) this
      (slack-message-star-api-request url
                                      (list (slack-message-star-api-params
                                             file))
                                      team))))

(cl-defmethod slack-buffer--replace ((this slack-file-info-buffer) _ts)
  (slack-if-let* ((buffer (get-buffer (slack-buffer-name this))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (slack-buffer-insert this)))))

(cl-defmethod slack-buffer-update ((this slack-file-info-buffer))
  (with-slots (file team) this
    (let ((buffer (get-buffer (slack-buffer-name this))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (slack-buffer-insert this))))))

(defun slack-file-update ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (file (oref buf file))
                  (team (oref buf file))
                  (page (oref file page)))
      (slack-file-request-info
       file page team
       #'(lambda (file team &rest _args)
           (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer team)))
               (slack-buffer-replace buffer file))))))

(defun slack-file-display ()
  (interactive)
  (slack-if-let* ((id (get-text-property (point) 'file))
                  (buf slack-current-buffer))
      (slack-buffer-display-file buf id)))

(provide 'slack-file-info-buffer)
;;; slack-file-info-buffer.el ends here
