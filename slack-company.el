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
       (content (str) (substring str 1 nil))
       (content-match-p (content text)
                        (or (eq 0 (length content))
                            (string-prefix-p content text))))
    (cl-case command
      (interactive (company-begin-backend 'company-slack-backend))
      (prefix (when (string= "slack" (car (split-string (format "%s" major-mode) "-")))
                (company-grab-line
                 "\\(\\W\\|^\\)\\(\\(@\\|#\\|/\\)\\(\\w\\|.\\|-\\|_\\)*\\)"
                 2)))
      (candidates (slack-if-let*
                      ((content (content arg))
                       (team (and slack-current-buffer
                                  (oref slack-current-buffer team))))
                      (cl-case (prefix-type arg)
                        (user-or-usergroup
                         (nconc
                          (cl-loop for special in '("here" "channel" "everyone")
                                   if (content-match-p content special)
                                   collect (propertize (concat "@" special)
                                                       'slack-company-prefix 'keyword))
                          (cl-loop for usergroup in (oref team usergroups)
                                   if (and (not (slack-usergroup-deleted-p usergroup))
                                           (content-match-p content
                                                            (oref usergroup handle)))
                                   collect (propertize (concat "@" (oref usergroup handle))
                                                       'slack-company-prefix 'usergroup
                                                       'slack-usergroup-id (oref usergroup id)))
                          (cl-loop for user in (slack-team-users team)
                                   if (and (not (eq (plist-get user :deleted) t))
                                           (or (content-match-p content
                                                                (slack-user-real-name user))
                                               (content-match-p content
                                                                (slack-user-display-name user))))
                                   collect (propertize (concat "@" (slack-user--name user team))
                                                       'slack-company-prefix 'user
                                                       'slack-user-id (plist-get user :id)))))
                        (channel
                         (cl-loop for channel in (slack-team-channels team)
                                  if (content-match-p content
                                                      (oref channel name))
                                  collect (concat "#" (oref channel name))))
                        (slash
                         (cl-loop for command in (oref team commands)
                                  if (content-match-p (concat "/" content)
                                                      (oref command name))
                                  collect (oref command name))))))
      (post-completion
       (cl-case (get-text-property 0 'slack-company-prefix arg)
         (user (let* ((inserted arg)
                      (end (point))
                      (user-id (get-text-property 0 'slack-user-id arg)))
                 (when (re-search-backward (substring-no-properties inserted)
                                           (point-min)
                                           t)
                   (let ((beg (point)))
                     (delete-region beg end)
                     (insert (concat "@" user-id))
                     (put-text-property beg (point) 'face 'slack-message-mention-face)
                     (put-text-property beg (point) 'display inserted)
                     (put-text-property beg (point) 'rear-nonsticky t)
                     (insert " ")))))
         (keyword (let ((inserted arg)
                        (end (point)))
                    (when (re-search-backward (substring-no-properties inserted)
                                              (point-at-bol)
                                              t)
                      (put-text-property (point) end
                                         'face 'slack-message-mention-keyword-face)
                      (put-text-property (point) end 'rear-nonsticky t)
                      (goto-char end)
                      (insert " "))))
         (usergroup (let* ((inserted arg)
                           (end (point))
                           (id (get-text-property 0 'slack-usergroup-id arg)))
                      (when (re-search-backward (substring-no-properties inserted)
                                                (point-at-bol)
                                                t)
                        (let ((beg (point)))
                          (delete-region beg end)
                          (insert (concat "@" id))
                          (put-text-property beg (point) 'face 'slack-message-mention-keyword-face)
                          (put-text-property beg (point) 'display inserted)
                          (put-text-property beg (point) 'rear-nonsticky t)
                          (insert " ")))))))
      (doc-buffer
       (cl-case (prefix-type arg)
         (user-or-usergroup
          (slack-if-let* ((team (and slack-current-buffer
                                     (oref slack-current-buffer team)))
                          (user-id (get-text-property 0 'slack-user-id arg))
                          (user (slack-user--find user-id team)))
              (company-doc-buffer (slack-user--profile-to-string user team))))
         (slash
          (company-doc-buffer
           (let* ((team (and slack-current-buffer
                             (oref slack-current-buffer team)))
                  (command (slack-command-find arg team)))
             (when command
               (slack-command-company-doc-string command team))))))))))


(provide 'slack-company)
;;; slack-company.el ends here
