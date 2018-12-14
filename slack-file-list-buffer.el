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
                                                 (number-to-string
                                                  before-oldest))))
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

(cl-defmethod slack-buffer-insert ((this slack-file-list-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-time (slack-message-time-stamp message))
        (ts (slack-ts message))
        (team (oref this team)))
    (lui-insert-with-text-properties
     (slack-message-to-string message ts team)
     'not-tracked-p not-tracked-p
     'ts ts
     'slack-last-ts lui-time-stamp-last)
    (lui-insert "" t)
    ))

(cl-defmethod slack-buffer--replace ((this slack-file-list-buffer) ts)
  (with-slots (team) this
    (slack-if-let*
        ((file (cl-find-if #'(lambda (e)
                               (string= ts
                                        (number-to-string (oref e created))))
                           (oref team files))))
        (slack-buffer-replace this file))))

(cl-defmethod slack-buffer-replace ((this slack-file-list-buffer)
                                    message)
  (with-slots (team) this
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (slack-message-to-string message
                                            (slack-ts message)
                                            team)
                   (lambda ()
                     (equal (get-text-property (point) 'ts)
                            (slack-ts message)))))))

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

(provide 'slack-file-list-buffer)
;;; slack-file-list-buffer.el ends here
