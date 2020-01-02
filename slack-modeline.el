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

(defcustom slack-enable-global-mode-string nil
  "If true, add `slack-modeline' to `global-mode-string'"
  :type 'boolean
  :group 'slack)

(defcustom slack-modeline-formatter #'slack-default-modeline-formatter
  "Format modeline with Arg '((team-name . (has-unreads . mention-count)))."
  :type 'function
  :group 'slack)

(defface slack-modeline-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to team has unreads message in modeline"
  :group 'slack)

(defface slack-modeline-thread-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to thread has unreads message in modeline"
  :group 'slack)

(defface slack-modeline-channel-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to channel has unreads message in modeline"
  :group 'slack)

(defun slack-default-modeline-formatter (alist)
  "Element in ALIST is  '((team-name . ((thread . (has-unreads . mention-count)) (channel . (has-unreads . mention-count)))))"
  (mapconcat #'(lambda (e)
                 (let* ((team-name (car e))
                        (summary (cdr e))
                        (thread (cdr (cl-assoc 'thread summary)))
                        (channel (cdr (cl-assoc 'channel summary)))
                        (thread-has-unreads (car thread))
                        (channel-has-unreads (car channel))
                        (has-unreads (or thread-has-unreads
                                         channel-has-unreads))
                        (thread-mention-count (cdr thread))
                        (channel-mention-count (cdr channel)))
                   (format "[ %s: %s, %s ]"
                           (if has-unreads
                               (propertize team-name
                                           'face 'slack-modeline-has-unreads-face)
                             team-name)
                           (if (or channel-has-unreads (< 0 channel-mention-count))
                               (propertize (number-to-string channel-mention-count)
                                           'face 'slack-modeline-channel-has-unreads-face)
                             channel-mention-count)
                           (if (or thread-has-unreads (< 0 thread-mention-count))
                               (propertize (number-to-string thread-mention-count)
                                           'face 'slack-modeline-thread-has-unreads-face)
                             thread-mention-count))))
             alist " "))

(defun slack-enable-modeline ()
  (when slack-enable-global-mode-string
    (add-to-list 'global-mode-string '(:eval slack-modeline) t)))

(defun slack-update-modeline ()
  (interactive)
  (let ((teams (cl-remove-if-not #'slack-team-modeline-enabledp
                                 (hash-table-values slack-teams-by-token))))
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
        (let* ((summary (slack-counts-summary counts))
               (unreads nil)
               (count 0)
               (thread (cdr (cl-assoc 'thread summary))))
          (cl-loop for e in summary
                   do (let ((type (car e))
                            (has-unreads (cadr e))
                            (mention-count (cddr e)))
                        (unless (eq type 'thread)
                          (cl-incf count mention-count)
                          (if (and has-unreads (null unreads))
                              (setq unreads t)))))
          (list (cons 'thread thread)
                (cons 'channel (cons unreads count))))
      (list (cons 'thread (cons nil 0))
            (cons 'channel (cons nil 0))))))

(cl-defmethod slack-counts-update ((team slack-team))
  (slack-client-counts team
                       #'(lambda (counts)
                           (oset team counts counts)
                           (slack-update-modeline))))

(provide 'slack-modeline)
;;; slack-modeline.el ends here
