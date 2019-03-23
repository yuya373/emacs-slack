;;; slack-company.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  南優也

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-usergroup)
(require 'slack-slash-commands)

(declare-function company-begin-backend "company")
(declare-function company-grab-line "company")
(declare-function company-doc-buffer "company")

(defun company-slack-backend (command &optional arg &rest ignored)
  "Completion backend for slack chats.  It currently understands
@USER; adding #CHANNEL should be a simple matter of programming."
  (interactive (list 'interactive))
  (cl-labels
      ((start-from-line-beginning (str)
                                  (let ((prompt-length (length lui-prompt-string)))
                                    (>= 0 (- (current-column) prompt-length (length str)))))
       (prefix-type (str) (cond
                           ((string-prefix-p "@" str) 'user-or-usergroup)
                           ((string-prefix-p "#" str) 'channel)
                           ((and (string-prefix-p "/" str)
                                 (start-from-line-beginning str))
                            'slash)))
       (content (str) (substring str 1 nil)))
    (cl-case command
      (interactive (company-begin-backend 'company-slack-backend))
      (prefix (when (string= "slack" (car (split-string (format "%s" major-mode) "-")))
                (company-grab-line "\\(\\W\\|^\\)\\(\\(@\\|#\\|/\\)\\([A-Za-z0-9._\\-]+\\)\\)"
                                   2)))
      (candidates (slack-if-let*
                      ((content (content arg))
                       (team (and slack-current-buffer
                                  (oref slack-current-buffer team))))
                      (cl-case (prefix-type arg)
                        (user-or-usergroup
                         (nconc
                          (cl-loop for special in '("here" "channel" "everyone")
                                   if (string-prefix-p content special)
                                   collect (concat "@" special))
                          (cl-loop for usergroup in (oref team usergroups)
                                   if (and (not (slack-usergroup-deleted-p usergroup))
                                           (string-prefix-p content
                                                            (oref usergroup handle)))
                                   collect (concat "@" (oref usergroup handle)))
                          (cl-loop for user in (oref team users)
                                   if (and (not (eq (plist-get user :deleted) t))
                                           (string-prefix-p content
                                                            (slack-user--name user team)))
                                   collect (concat "@" (slack-user--name user team)))))
                        (channel
                         (cl-loop for team in (oref team channels)
                                  if (string-prefix-p content
                                                      (oref team name))
                                  collect (concat "#" (oref team name))))
                        (slash
                         (cl-loop for command in (oref team commands)
                                  if (string-prefix-p (concat "/" content)
                                                      (oref command name))
                                  collect (oref command name))))))
      (post-completion
       (let* ((inserted arg)
              (end (point))
              (team (oref slack-current-buffer team))
              (user (slack-user--find-by-name (substring-no-properties
                                               inserted 1)
                                              team)))
         (when (re-search-backward (substring-no-properties inserted)
                                   (point-min)
                                   t)
           (let ((beg (point)))
             (delete-region beg end)
             (insert (concat "@" (plist-get user :name)))
             (put-text-property beg (point) 'face 'slack-message-mention-face)
             (put-text-property beg (point) 'display inserted)
             (insert " ")
             (goto-char (+ 1 (length inserted) end))))))
      (doc-buffer
       (cl-case (prefix-type arg)
         (user-or-usergroup
          (slack-if-let* ((team (and slack-current-buffer
                             (oref slack-current-buffer team)))
                          (user-name (substring arg 1))
                          (user (slack-user-find-by-name user-name team))
                          (name (slack-user--name user team)))
              (unless (string= user-name name)
                (company-doc-buffer name))))
         (slash
          (company-doc-buffer
           (let* ((team (and slack-current-buffer
                             (oref slack-current-buffer team)))
                  (command (slack-command-find arg team)))
             (when command
               (slack-command-company-doc-string command team))))))))))


(provide 'slack-company)
;;; slack-company.el ends here
