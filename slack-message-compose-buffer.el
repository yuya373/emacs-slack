;;; slack-message-compose-buffer.el ---              -*- lexical-binding: t; -*-

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
(require 'slack-message-editor)
(require 'slack-message-attachment-preview-buffer)

(defvar slack-message-compose-buffer-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-f") #'slack-message-select-file)
    keymap))

(define-derived-mode slack-message-compose-buffer-mode
  slack-edit-message-mode
  "Slack Compose Message")

(defclass slack-message-compose-buffer (slack-buffer)
  ((room-id :initarg :room-id :type string)
   (attachment-buffer
    :type (or null slack-message-attachment-preview-buffer)
    :initform nil))
  :abstract t)

(defun slack-message-select-file ()
  (interactive)
  (slack-buffer-select-file slack-current-buffer))

(cl-defmethod slack-buffer-select-file ((this slack-message-compose-buffer))
  (when (null (oref this attachment-buffer))
    (oset this
          attachment-buffer
          (make-instance 'slack-message-attachment-preview-buffer
                         :team-id (oref this team-id)
                         :room-id (oref this room-id))))
  (let* ((path (expand-file-name
                (car (find-file-read-args "Select File: " t))))
         (filename (read-from-minibuffer "Filename: "
                                         (file-name-nondirectory path))))

    (slack-buffer-append-file (oref this attachment-buffer)
                              path
                              filename)))

(cl-defmethod slack-buffer-remove-file ((this slack-message-compose-buffer))
  (slack-buffer-remove-file (oref this attachment-buffer)))

(defun slack-message-remove-file ()
  (interactive)
  (slack-buffer-remove-file slack-current-buffer))

(cl-defmethod slack-buffer-attachments ((this slack-message-compose-buffer))
  (slack-if-let* ((attachment-buffer (oref this attachment-buffer)))
      (oref attachment-buffer files)))

(cl-defmethod slack-buffer-room ((this slack-message-compose-buffer))
  (with-slots (room-id) this
    (slack-room-find room-id (slack-buffer-team this))))

(cl-defmethod slack-buffer-send-message ((this slack-message-compose-buffer) _message)
  (when (oref this attachment-buffer)
    (slack-buffer-kill-buffer-window (oref this attachment-buffer)))

  (slack-buffer-kill-buffer-window this))

(cl-defmethod slack-buffer-init-buffer ((this slack-message-compose-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-compose-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (message "C-c C-c to send message")
    buf))

(provide 'slack-message-compose-buffer)
;;; slack-message-compose-buffer.el ends here
