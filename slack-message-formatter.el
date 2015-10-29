;;; slack-message-formatter.el ---format message text  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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

(defface slack-message-output-text
  '((t (:weight normal :height 0.9)))
  "Face used to text message."
  :group 'slack-buffer)

(defface slack-message-output-header
  '((t (:foreground "#FFA000"
        :weight bold
        :height 1.0
        :underline t)))
  "Face used to text message."
  :group 'slack-buffer)

(defun slack-message-put-header-property (header)
  (put-text-property 0 (length header)
                       'face 'slack-message-output-header header))

(defun slack-message-put-text-property (text)
  (put-text-property 0 (length text)
                       'face 'slack-message-output-text text))

(defmethod slack-message-time-to-string ((m slack-message))
  (with-slots (ts) m
    (format-time-string "%Y-%m-%d %H:%M"
                      (seconds-to-time
                       (string-to-number ts)))))

(defmethod slack-message-to-string ((m slack-message))
  (with-slots (text) m
    (let ((ts (slack-message-time-to-string m)))
      (slack-message-put-header-property ts)
      (slack-message-put-text-property text)
      (concat ts "\n" text "\n"))))


(provide 'slack-message-formatter)
;;; slack-message-formatter.el ends here
