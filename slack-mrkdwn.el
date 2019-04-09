;;; slack-mrkdwn.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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

(defconst slack-mrkdwn-regex-bold
  "\\(?:^\\|\\W\\)\\(\\(*\\)\\([^ \n\t\\]\\|[^ \n\t*]\\(?:.\\)*?[^\\ ]\\)\\(\\2\\)\\(\\W\\|$\\)\\)")

(defface slack-mrkdwn-bold-face
  '((t (:weight bold)))
  "Face used to between `*'"
  :group 'slack)

(defconst slack-mrkdwn-regex-italic
  "\\(?:^\\|\\W\\)\\(\\(_\\)\\([^\n\t]\\|[^\n\t*]\\(?:.\\)*?\\)\\(\\2\\)\\(\\W\\|$\\)\\)")

(defface slack-mrkdwn-italic-face
  '((t (:slant italic)))
  "Face used to between `_'"
  :group 'slack)

(defconst slack-mrkdwn-regex-strike
  "\\(?:^\\|\\W\\)\\(\\(~\\)\\([^ \n\t\\]\\|[^ \n\t*]\\(?:.\\)*?[^\\ ]\\)\\(\\2\\)\\(\\W\\|$\\)\\)")

(defface slack-mrkdwn-strike-face
  '((t (:strike-through t)))
  "Face used to between `~'"
  :group 'slack)

(defconst slack-mrkdwn-regex-code
  "\\(?:\\`\\|\\W\\)\\(\\(`\\)\\(\\(?:.\\)*?[^`]\\)\\(\\2\\)\\)\\(?:[^`]\\|\\'\\)")

(defface slack-mrkdwn-code-face
  '((t (:inherit slack-preview-face)))
  "Face used to between ``'"
  :group 'slack)

(defconst slack-mrkdwn-regex-code-block "\\(?:^\\|\\W\\)\\(```\\)\\(?:\n\\)?\\(\\(.\\|\n\\)*?\\)\\(```\\)\\W")

(defface slack-mrkdwn-code-block-face
  '((t (:inherit slack-preview-face)))
  "Face used to between ````'"
  :group 'slack)

(defconst slack-mrkdwn-regex-blockquote
  "^[ \t]*\\([A-Z]?>\\)\\([ \t]*\\)\\(.+\\)$")

(defface slack-mrkdwn-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face used to `>'"
  :group 'slack)

(defun slack-mrkdwn-plain-text-p (point)
  (let ((text-type (get-text-property point 'slack-text-type)))
    (or (null text-type)
        (eq 'plain text-type))))

(defun slack-mrkdwn-add-face ()
  (slack-mrkdwn-mark-code-block)
  (slack-mrkdwn-add-bold-face)
  (slack-mrkdwn-add-italic-face)
  (slack-mrkdwn-add-strike-face)
  (slack-mrkdwn-add-code-face)
  (slack-mrkdwn-add-code-block-face)
  (slack-mrkdwn-add-blockquote-face))

(defun slack-mrkdwn-inside-code-p (point)
  (or (slack-mrkdwn-inside-code-block-p point)
      (slack-mrkdwn-inside-inline-code-p point)))

(defun slack-mrkdwn-inside-code-block-p (point)
  (let ((block-type (get-text-property point 'slack-code-block-type)))
    (eq 'block block-type)))

(defun slack-mrkdwn-inside-inline-code-p (point)
  (let ((block-type (get-text-property point 'slack-code-block-type)))
    (eq 'inline block-type)))

(defun slack-mrkdwn-mark-code-block ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code-block (point-max) t)
    (slack-if-let* ((beg (match-beginning 2))
                    (end (match-end 2)))
        (unless (slack-mrkdwn-plain-text-p beg)
          (put-text-property beg end
                             'slack-code-block-type 'block))))

  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end
                             'slack-code-block-type 'inline)))))

(defun slack-mrkdwn-add-bold-face ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-bold (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-bold-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-italic-face ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-italic (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (slack-mrkdwn-plain-text-p beg)
          (put-text-property beg end 'face 'slack-mrkdwn-italic-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-strike-face ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-strike (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-strike-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-code-face ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-block-p beg))
          (put-text-property beg end 'face 'slack-mrkdwn-code-face)
          (slack-if-let* ((markup-start-beg (match-beginning 2))
                          (markup-start-end (match-end 2)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))


(defun slack-mrkdwn-add-code-block-face ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code-block (point-max) t)
    (slack-if-let* ((beg (match-beginning 2))
                    (end (match-end 2)))
        (unless (slack-mrkdwn-plain-text-p beg)
          (overlay-put (make-overlay beg end)
                       'face 'slack-mrkdwn-code-block-face)
          (put-text-property beg end 'face 'slack-mrkdwn-code-face)
          (slack-if-let* ((markup-start-beg (match-beginning 1))
                          (markup-start-end (match-end 1)))
              (put-text-property markup-start-beg
                                 markup-start-end
                                 'invisible t))
          (slack-if-let* ((markup-end-beg (match-beginning 4))
                          (markup-end-end (match-end 4)))
              (put-text-property markup-end-beg
                                 markup-end-end
                                 'invisible t))))))

(defun slack-mrkdwn-add-blockquote-face ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-blockquote (point-max) t)
    (slack-if-let* ((beg (match-beginning 3))
                    (end (match-end 3)))
        (unless (or (slack-mrkdwn-plain-text-p beg)
                    (slack-mrkdwn-inside-code-p beg))
          (put-text-property beg end
                             'face 'slack-mrkdwn-blockquote-face)
          (slack-if-let* ((markup-start-beg (match-beginning 1))
                          (markup-start-end (match-end 1)))
              (progn
                (put-text-property markup-start-beg
                                   markup-start-end
                                   'face
                                   'slack-mrkdwn-blockquote-face)
                (put-text-property markup-start-beg
                                   markup-start-end
                                   'display "┃")))))))

(provide 'slack-mrkdwn)
;;; slack-mrkdwn.el ends here
