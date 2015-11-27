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

(define-derived-mode slack-mode fundamental-mode "Slack")

(defvar slack-current-room)
(make-local-variable 'slack-current-room)

(defun slack-buffer-set-current-room (room)
  (set (make-local-variable 'slack-current-room) room))

(defun slack-buffer-harden-newlines ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(defun slack-buffer-format ()
  (setq line-spacing 0.5
        use-hard-newlines t
        truncate-lines nil
        fill-column 80
        word-wrap t)
  (slack-buffer-harden-newlines)
  (fill-region (point-min) (point-max) t t t))

(defun slack-buffer-create (buf-name room header messages)
  (let ((buffer (get-buffer-create buf-name)))
    (if buffer
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (slack-mode)
          (insert header)
          (insert "Messages:\n")
          (mapc #'(lambda (m) (insert m) (insert "\n")) (reverse messages))
          (slack-buffer-format)
          (slack-buffer-set-current-room room)
          (setq buffer-read-only t)))
    buffer))

(defun slack-buffer-update (buf-name text)
  (let ((buffer (get-buffer buf-name)))
    (if buffer
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert text)
          (insert "\n")
          (goto-char (point-max))
          (slack-buffer-format)
          (setq buffer-read-only t)))))

(defun slack-buffer-update-notification (buf-name string)
  (let ((buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (slack-mode)
      (goto-char (point-max))
      (insert string)
      (insert "\n")
      (goto-char (point-max))
      (slack-buffer-format)
      (setq buffer-read-only t))))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
