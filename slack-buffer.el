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
(defvar-local slack-current-room nil)

(defun slack-buffer-set-current-room (room)
  (set (make-local-variable 'slack-current-room) room))

(defun slack-buffer-create (buf-name room header messages)
  (let ((buffer (get-buffer-create buf-name)))
    (if buffer
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (slack-mode)
          (slack-buffer-set-current-room room)
          (insert header)
          (insert "Messages:\n")
          (mapc #'(lambda (m) (insert (slack-message-to-string m)))
                (reverse messages))
          (setq line-spacing 0.5)
          (setq buffer-read-only t)))
    buffer))

(defun slack-buffer-update-message ()
  (interactive)
  (unless slack-current-room
    (error "Call From Slack Room Buffer"))
  (slack-room-history slack-current-room)
  (slack-buffer-create (slack-room-buffer-name slack-current-room)
                       slack-current-room
                       (slack-room-buffer-header slack-current-room)
                       (oref slack-current-room messages)))

(defun slack-buffer-update (buf-name m)
  (let ((buffer (get-buffer buf-name)))
    (if buffer
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert (slack-message-to-string m))
          (goto-char (point-max))
          (setq buffer-read-only t)))))

(defun slack-buffer-update-notification (buf-name string)
  (let ((buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (setq line-spacing 0.5)
      (slack-mode)
      (goto-char (point-max))
      (insert string)
      (insert "\n")
      (goto-char (point-max))
      (setq buffer-read-only t))))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
