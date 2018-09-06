;;; helm-slack.el ---                                -*- lexical-binding: t; -*-

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

(require 'helm)
(require 'slack-team)
(require 'slack-conversations)

(defvar helm-slack-source nil)
(defvar helm-slack-source-history nil)
(defvar helm-slack-actions nil)

(setq helm-slack-actions
      (helm-make-actions
       "Open channel" #'helm-slack-display-room
       "Archive channel" #'helm-slack-archive-room
       "Invite user to channel" #'helm-slack-invite-to-room
       ))

(defun helm-slack-build-candidates ()
  (cl-loop for team in slack-teams
           as c = (oref team channels)
           as g = (oref team groups)
           as i = (oref team ims)
           nconc (cl-labels
                     ((filter (rooms) (cl-remove-if #'slack-room-hidden-p
                                                    rooms))
                      (collector (label room) (list label room team)))
                   (let ((slack-display-team-name
                          (< 1 (length slack-teams))))
                     (slack-room-names (append c g i) team
                                       #'filter #'collector)))))

(defmacro helm-slack-bind-room-and-team (candidate &rest body)
  (declare (indent 2) (debug t))
  `(let ((room (car ,candidate))
         (team (cadr ,candidate)))
     ,@body))

(defun helm-slack-display-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-room-display room team)))

(defun helm-slack-archive-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-archive room team)))

(defun helm-slack-invite-to-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-invite room team)))

(defun helm-slack ()
  "Helm Slack"
  (interactive)
  (setf helm-slack-source
        (helm-build-sync-source "Helm Slack"
          :history 'helm-slack-source-history
          :action helm-slack-actions
          :candidates #'helm-slack-build-candidates))
  (helm
   :prompt "Select Channel : "
   :sources 'helm-slack-source
   :history 'helm-slack-source-history))

(provide 'helm-slack)
;;; helm-slack.el ends here
