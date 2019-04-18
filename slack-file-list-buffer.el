;;; slack-file-list-buffer.el ---                    -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-message-buffer)

(defvar slack-file-download-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'slack-download-file-at-point)
    (define-key map [mouse-1] 'slack-download-file-at-point)
    map))

(define-derived-mode slack-file-list-buffer-mode slack-buffer-mode "Slack Files")

;; TODO impl without slack-message-buffer
(defclass slack-file-list-buffer (slack-buffer)
  ((page :initarg :page :type integer)
   (pages :initarg :pages :type integer)
   (oldest :type integer)
   (oldest-id :type string)))

(cl-defmethod slack-buffer-find ((_class (subclass slack-file-list-buffer)) team)
  (slack-if-let* ((buf (car (oref team slack-file-list-buffer))))
      (with-current-buffer buf slack-current-buffer)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-file-list-buffer)) team)
  (format "*Slack - %s : Files"
          (slack-team-name team)))

(cl-defmethod slack-buffer-name ((this slack-file-list-buffer))
  (slack-buffer-name 'slack-file-list-buffer (oref this team)))

(cl-defmethod slack-buffer-major-mode ((_this slack-file-list-buffer))
  'slack-file-list-buffer-mode)

(defun slack-create-file-list-buffer (page pages team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer
                                             team)))
      buffer
    (slack-file-list-buffer :team team :page page :pages pages)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-file-list-buffer))
  (with-slots (page pages) this
    (< page pages)))

(cl-defmethod slack-buffer-set-oldest ((this slack-file-list-buffer) file)
  (when file
    (oset this oldest (oref file created))
    (oset this oldest-id (oref file id))))

(cl-defmethod slack-buffer-insert-history ((this slack-file-list-buffer))
  (with-slots (team) this
    (let ((files (oref team files))
          (before-oldest (oref this oldest))
          (before-oldest-id (oref this oldest-id)))
      (slack-buffer-set-oldest this (car files))
      (cl-loop for file in files
               do (when (or (< (oref file created) before-oldest)
                            (and (= (oref file created) before-oldest)
                                 (not (string= (oref file id) before-oldest-id))))
                    (slack-buffer-insert this file t)))

      (slack-if-let* ((point (slack-buffer-ts-eq (point-min)
                                                 (point-max)
                                                 before-oldest-id)))
          (goto-char point)))))

(cl-defmethod slack-buffer-request-history ((this slack-file-list-buffer) after-success)
  (cl-labels
      ((success (page pages)
                (oset this page page)
                (oset this pages pages)
                (funcall after-success)))
    (with-slots (team page pages) this
      (slack-file-list-request team
                               :page (number-to-string (1+ page))
                               :after-success #'success))))

(cl-defmethod slack-buffer-init-buffer ((this slack-file-list-buffer))
  (let ((buf (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buf
      (slack-file-list-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert-load-more this)
      (let* ((inhibit-read-only t)
             (files (oref (oref this team) files)))
        (cl-loop for file in files
                 do (slack-buffer-insert this file))
        (slack-buffer-set-oldest this (car files)))
      (goto-char (point-max)))
    (unless (oref (oref this team) slack-file-list-buffer)
      (push buf (oref (oref this team) slack-file-list-buffer)))
    buf))

(cl-defmethod slack-buffer-update ((this slack-file-list-buffer) message &key replace)
  (with-slots (room team) this
    (let ((buffer (get-buffer (slack-buffer-name this))))
      (if replace (slack-buffer-replace this message)
        (with-current-buffer buffer
          (slack-buffer-insert this message))))))

(cl-defmethod slack-buffer-download-file ((this slack-file-list-buffer) file-id)
  (slack-if-let* ((team (oref this team))
                  (file (slack-file-find file-id team)))
      (slack-file-download file team)))

(defun slack-download-file-at-point ()
  (interactive)
  (slack-if-let* ((file-id (get-text-property (point) 'file-id))
                  (buf slack-current-buffer))
      (slack-buffer-download-file buf file-id)))

(defun slack-buffer--run-file-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (file-id (get-text-property (point) 'file-id)))
      (slack-buffer-run-file-action buf file-id)))

(cl-defmethod slack-buffer-run-file-action ((this slack-file-list-buffer) file-id)
  (let* ((team (oref this team))
         (file (slack-file-find file-id team)))
    (slack-file-run-action file this)))

(cl-defmethod slack-buffer-file-to-string ((this slack-file-list-buffer) file)
  (let* ((team (oref this team))
         (lui-time-stamp-time (slack-message-time-stamp file))
         (thumb (slack-image-string (slack-file-thumb-image-spec file 80)))
         (header (format "%s%s"
                         (if (slack-string-blankp thumb) ""
                           (format "%s " thumb))
                         (slack-file-link-info (slack-file-id file)
                                               (oref file title))))
         (user-name (propertize (or (slack-user-name (oref file user) team) "")
                                'face '(:weight bold :height 0.8)))
         (timestamp (and (oref file timestamp)
                         (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (seconds-to-time
                                              (oref file timestamp)))))
         (description (format "%s %s %s%s"
                              user-name
                              timestamp
                              (if (slack-file-downloadable-p file)
                                  (format "%s "
                                          (slack-file-download-button file))
                                "")
                              (slack-file-action-button file))))
    (slack-format-message header description)))

(cl-defmethod slack-buffer-insert ((this slack-file-list-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-time (slack-message-time-stamp message))
        (ts (slack-file-id message)))
    (lui-insert-with-text-properties
     (slack-buffer-file-to-string this message)
     'not-tracked-p not-tracked-p
     'ts ts
     'slack-last-ts lui-time-stamp-last)
    (lui-insert "" t)))

(cl-defmethod slack-buffer--replace ((this slack-file-list-buffer) ts)
  (with-slots (team) this
    (slack-if-let* ((file (slack-file-find ts team)))
        (slack-buffer-replace this file))))

(cl-defmethod slack-buffer-replace ((this slack-file-list-buffer) message)
  (with-slots (team) this
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (slack-buffer-file-to-string this message)
                   (lambda ()
                     (equal (get-text-property (point) 'ts)
                            (slack-file-id message)))))))

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-file-list-buffer) file-id)
  (with-slots (team) this
    (slack-if-let* ((file (cl-find-if
                           #'(lambda (e) (string= (oref e id)
                                                  file-id))
                           (oref team files))))
        (progn
          (oset file is-expanded (not (oref file is-expanded)))
          (slack-buffer-update this file :replace t)))))

(defun slack-file-list ()
  (interactive)
  (cl-labels
      ((open-buffer (buf)
                    (slack-buffer-display buf)))
    (slack-if-let* ((team (slack-team-select))
                    (buffer (slack-buffer-find 'slack-file-list-buffer
                                               team)))
        (open-buffer buffer)
      (slack-file-list-request
       team
       :after-success #'(lambda (page pages)
                          (open-buffer
                           (slack-create-file-list-buffer page
                                                          pages
                                                          team)))))))

(cl-defmethod slack-buffer-add-star ((this slack-file-list-buffer) ts)
  (with-slots (team) this
    (slack-if-let* ((file (slack-file-find ts team)))
        (slack-message-star-api-request slack-message-stars-add-url
                                        (list (slack-message-star-api-params file))
                                        team))))

(cl-defmethod slack-buffer-remove-star ((this slack-file-list-buffer) ts)
  (with-slots (team) this
    (slack-if-let* ((file (slack-file-find ts team)))
        (slack-message-star-api-request slack-message-stars-remove-url
                                        (list (slack-message-star-api-params file))
                                        team))))

(provide 'slack-file-list-buffer)
;;; slack-file-list-buffer.el ends here
