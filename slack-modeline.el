;;; slack-modeline.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
(require 'slack-team)
(require 'slack-counts)

(defvar slack-modeline nil)

(defcustom slack-modeline-formatter #'slack-default-modeline-formatter
  "Format modeline with Arg '((team-name . (has-unreads . mention-count)))."
  :type 'function
  :group 'slack)

(defface slack-modeline-has-unreads-face
  '((t (:weight bold)))
  "Face used to team has unreads message in modeline"
  :group 'slack)

(defun slack-default-modeline-formatter (alist)
  "Arg is alist of '((team-name . (has-unreads . mention-count)))"
  (mapconcat #'(lambda (e)
                 (let ((team-name (car e))
                       (has-unreads (cadr e))
                       (mention-count (cddr e)))
                   (format "[ %s: %s ]"
                           (if has-unreads
                               (propertize team-name
                                           'face 'slack-modeline-has-unreads-face)
                             team-name)
                           mention-count)))
             alist " "))

(defun slack-enable-modeline ()
  (add-to-list 'global-mode-string '(:eval slack-modeline) t))

(defun slack-update-modeline ()
  (interactive)
  (let ((teams (cl-remove-if-not #'slack-team-modeline-enabledp
                                 slack-teams)))
    (when (< 0 (length teams))
      (setq slack-modeline
            (funcall slack-modeline-formatter
                     (mapcar #'(lambda (e)
                                 (cons (or (oref e modeline-name)
                                           (slack-team-name e))
                                       (slack-team-counts-summary e)))
                             teams)))
      (force-mode-line-update))))

(defun slack-team-counts-summary (team)
  (with-slots (counts) team
    (if counts
        (let ((summary (slack-counts-summary counts))
              (unreads nil)
              (count 0))
          (cl-loop for e in summary
                   do (let ((has-unreads (cadr e))
                            (mention-count (cddr e)))
                        (cl-incf count mention-count)
                        (if (and has-unreads (null unreads))
                            (setq unreads t))))
          (cons unreads count))
      (cons nil 0))))

(provide 'slack-modeline)
;;; slack-modeline.el ends here
