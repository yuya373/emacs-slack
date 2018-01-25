;;; slack-file-comment-compose-buffer.el ---         -*- lexical-binding: t; -*-

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
(require 'slack-message-compose-buffer)

(defclass slack-file-comment-compose-buffer
  (slack-message-compose-buffer)
  ((file :initarg :file :type slack-file)))

(defmethod slack-buffer-name :static ((class slack-file-comment-compose-buffer) file team)
  (format "*Slack -  %s : Compose %s Comment*"
          (oref team name)
          (with-slots (title name id) file
            (or title name id))))

(defmethod slack-buffer-name ((this slack-file-comment-compose-buffer))
  (with-slots (file team) this
    (slack-buffer-name 'slack-file-comment-compose-buffer
                       file team)))

(defmethod slack-buffer-init-buffer ((this slack-file-comment-compose-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-message-compose-buffer)
      (slack-buffer-set-current-buffer this)
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-slots (file team) this
      (slack-buffer-push-new-3 'slack-file-comment-compose-buffer
                               file team))
    buf))

(defmethod slack-buffer-send-message
  ((this slack-file-comment-compose-buffer) message)
  (with-slots (file team) this
    (slack-file-comment-add-request (oref file id)
                                    message
                                    team))
  (call-next-method))

(defun slack-create-file-comment-compose-buffer (file team)
  (slack-if-let* ((buf (slack-buffer-find
                        'slack-file-comment-compose-buffer
                        file team)))
      buf
    (slack-file-comment-compose-buffer :file file
                                       :team team)))

(provide 'slack-file-comment-compose-buffer)
;;; slack-file-comment-compose-buffer.el ends here
