;;; slack-stars-buffer.el ---                        -*- lexical-binding: t; -*-

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
(require 'slack-buffer)

(defclass slack-stars-buffer (slack-buffer) ())

(defmethod slack-buffer-name :static ((_class slack-stars-buffer) team)
  (format "*Slack - %s : Stars*" (oref team name)))

(defmethod slack-buffer-name ((this slack-stars-buffer))
  (slack-buffer-name 'slack-stars-buffer (oref this team)))

(defmethod slack-buffer-find :static ((class slack-stars-buffer) team)
  (if-let* ((buf (cl-find-if #'(lambda (e) (string= (buffer-name e)
                                                    (slack-buffer-name class team)))
                             (slot-value team class))))
      (with-current-buffer buf slack-current-buffer)))

(defmethod slack-buffer-insert ((this slack-stars-buffer) item)
  (lui-insert (slack-to-string item (oref this team))))

(defmethod slack-buffer-insert-load-more ((this slack-stars-buffer))
  (let ((str (propertize "(load more)\n"
                         'face '(:underline t :wight bold)
                         'keymap (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "RET")
                                     #'(lambda ()
                                         (interactive)
                                         (slack-buffer-load-more this)))
                                   map))))
    (let ((lui-time-stamp-position nil))
      (lui-insert str))))

(defmethod slack-buffer-init-buffer ((this slack-stars-buffer))
  (let ((buf (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buf
      (slack-info-mode)
      (slack-buffer-insert-load-more this)
      (with-slots (star) (oref this team)
        (cl-loop for m in (oref star items)
                 do (slack-buffer-insert this m)))
      (goto-char (point-max)))
    (unless (oref (oref this team) slack-stars-buffer)
      (push buf (oref (oref this team) slack-stars-buffer)))
    buf))

(defun slack-create-stars-buffer (team)
  (if-let* ((buf (slack-buffer-find 'slack-stars-buffer team)))
      buf
    (make-instance 'slack-stars-buffer
                   :team team)))

(provide 'slack-stars-buffer)
;;; slack-stars-buffer.el ends here
