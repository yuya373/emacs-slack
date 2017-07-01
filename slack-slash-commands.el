;;; slack-slash-commands.el ---                      -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defvar slack-slash-commands-available
  '("active" "away" "dnd"))

(defun slack-slash-commands-parse (text)
  (if (string-prefix-p "/" text)
      (let* ((maybe-command (car (split-string (substring text 1) " ")))
             (command (cl-find maybe-command slack-slash-commands-available
                               :test #'string=)))
        (when command
          (cons command (cdr (split-string text " ")))))))

(defun slack-slack-commands-execute (command team)
  (let ((command (car command))
        (args (cdr command)))
    (cond
     ((string= command "active")
      (slack-request-set-active team))
     ((string= command "away")
      (slack-request-set-presence team))
     ((string= command "dnd")
      (slack-request-dnd-set-snooze team (car args))))))

(provide 'slack-slash-commands)
;;; slack-slash-commands.el ends here
