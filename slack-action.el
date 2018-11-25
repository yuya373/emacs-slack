;;; slack-action.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

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
(require 'slack-util)

(defvar slack-current-buffer)
(defvar slack-action-keymap)

(defface slack-message-action-face
  '((t (:box (:line-width 1 :style released-button))))
  "Face used to action."
  :group 'slack)

(defun slack-display-inline-action ()
  (goto-char (point-min))
  (let ((regexp "<slack-action://\\(.*?\\)/\\(.*?\\)|\\(.*?\\)>"))
    (while (re-search-forward regexp (point-max) t)
      (let ((bot (match-string 1))
            (payload (match-string 2))
            (label (match-string 3)))
        (replace-match (propertize label
                                   'face 'slack-message-action-face
                                   'bot bot
                                   'payload payload
                                   'org-text (match-string 0)
                                   'keymap slack-action-keymap))))))

(provide 'slack-action)
;;; slack-action.el ends here
