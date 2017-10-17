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
(require 'slack-buffer)

(defclass slack-file-info-buffer (slack-buffer)
  ((file :initarg :file :type slack-file)))

(defmethod slack-buffer-name ((this slack-file-info-buffer))
  (format "%s File: %s"
          (call-next-method)
          (with-slots (title name id) (oref this file)
            (or title name id))))

(defmethod slack-buffer-init-buffer ((this slack-file-info-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-file-info-mode)
      (setq slack-current-buffer this)
      (let ((inhibit-read-only t))
        (delete-region (point-min) lui-output-marker))
      (with-slots (file team) this
        (lui-insert (slack-to-string file team))))
    buf))

(defun slack-create-file-info-buffer (team file)
  (slack-file-info-buffer :team team :file file))

(defmethod slack-buffer-send-message ((this slack-file-info-buffer) message)
  (with-slots (file team) this
    (slack-file-comment-add-request (oref file id) message team)))

(defmethod slack-buffer-redisplay ((this slack-file-info-buffer))
  (with-current-buffer (slack-buffer-buffer this)
    (let ((cur-point (point))
          (max (marker-position lui-output-marker)))
      (slack-buffer-init-buffer this)
      (if (and (<= (point-min) cur-point)
               (< cur-point max))
          (goto-char cur-point)))))



(provide 'slack-file-info-buffer)
;;; slack-file-info-buffer.el ends here
