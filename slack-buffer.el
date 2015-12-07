;;; slack-buffer.el --- slack buffer                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'lui)

(define-derived-mode slack-mode lui-mode "Slack"
  ""
  (lui-set-prompt "> ")
  (setq lui-time-stamp-position nil)
  (setq lui-fill-type nil)
  (setq lui-input-function 'slack-message--send))

(defvar slack-current-room)
(make-local-variable 'slack-current-room)

(defcustom slack-buffer-emojify nil
  "Show emoji with `emojify' if true."
  :group 'slack)

(defun slack-get-buffer-create (buf-name)
  (let ((buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (slack-mode)))
    buffer))

(defun slack-buffer-set-current-room (room)
  (set (make-local-variable 'slack-current-room) room))

(defun slack-buffer-enable-emojify ()
  (if slack-buffer-emojify
      (let ((emojify (require 'emojify nil t)))
        (unless emojify
          (error "Emojify is not installed"))
        (emojify-mode t))))

(defun slack-buffer-create (buf-name room header messages)
  (let ((buffer (slack-get-buffer-create buf-name)))
    (with-current-buffer buffer
      (mapc (lambda (m) (lui-insert m t)) (reverse messages))
      (slack-buffer-set-current-room room)
      (goto-char (point-max))
      (slack-buffer-enable-emojify))
    buffer))

(cl-defun slack-buffer-update (buf-name text &key replace before)
  (let ((buffer (get-buffer buf-name)))
    (if buffer
        (if replace
            (slack-buffer-replace buffer text before)
          (with-current-buffer buffer (lui-insert text))))))

(defun slack-buffer-replace (buffer text before)
  (with-current-buffer buffer
    (goto-char (point-max))
    (let ((match (search-backward before))
          (inhibit-read-only t))
      (if match
          (progn
            (delete-region match
                           (+ match (length before)))
            (insert text t)
            (goto-char (point-max)))))))

(defun slack-buffer-update-notification (buf-name string)
  (let ((buffer (slack-get-buffer-create buf-name)))
    (with-current-buffer buffer
      (lui-insert string))))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
