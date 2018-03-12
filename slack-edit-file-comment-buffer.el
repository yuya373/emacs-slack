;;; slack-edit-file-comment-buffer.el ---            -*- lexical-binding: t; -*-

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
(require 'slack-message)
(require 'slack-message-compose-buffer)

(define-derived-mode slack-edit-file-comment-buffer-mode
  slack-message-edit-buffer-mode
  "Slack Edit File Comment")

(defclass slack-edit-file-comment-buffer
  (slack-message-compose-buffer)
  ((file :initarg :file :type slack-file)
   (file-comment-id :initarg :file-comment-id :type string)))

(defun slack-create-edit-file-comment-buffer (file file-comment-id team)
  (slack-if-let* ((buf (slack-buffer-find 'slack-edit-file-comment-buffer
                                    file
                                    file-comment-id
                                    team)))
      buf
    (slack-edit-file-comment-buffer :file file
                                    :file-comment-id file-comment-id
                                    :team team)))

(defmethod slack-buffer-name :static ((_class slack-edit-file-comment-buffer) file file-comment-id team)
  (format "*Slack - %s : %s Edit File Comment: %s"
          (oref team name)
          (with-slots (title name id) file
            (or title name id))
          file-comment-id))

(defmethod slack-buffer-name ((this slack-edit-file-comment-buffer))
  (with-slots (file file-comment-id team) this
    (slack-buffer-name (eieio-object-class-name this)
                       file file-comment-id team)))

(defmethod slack-buffer-find :static ((class slack-edit-file-comment-buffer) file file-comment-id team)
  (slack-buffer-find-4 class file file-comment-id team))

(defmethod slack-buffer-init-buffer ((this slack-edit-file-comment-buffer))
  (let ((buf (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buf
      (slack-edit-file-comment-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (setq buffer-read-only nil)
      (erase-buffer)
      (with-slots (file file-comment-id) this
        (slack-with-file-comment file-comment-id file
          (insert (slack-message-unescape-string
                   (oref file-comment comment)
                   (oref this team))))))
    buf))

(defmethod slack-buffer-send-message ((this slack-edit-file-comment-buffer) message)
  (with-slots (file file-comment-id team) this
    (slack-file-comment-edit-request (oref file id)
                                     file-comment-id
                                     message
                                     team))
  (call-next-method))

(provide 'slack-edit-file-comment-buffer)
;;; slack-edit-file-comment-buffer.el ends here
