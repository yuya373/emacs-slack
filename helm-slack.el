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
(require 'slack-room-info-buffer)
(require 'slack-conversations)
(require 'slack-user-profile-buffer)

(defvar helm-slack-actions
      (helm-make-actions
       "Display channel" #'helm-slack-display-room
       "Archive channel" #'helm-slack-archive-room
       "Invite user to channel" #'helm-slack-invite-to-room
       "Kick user from channel" #'helm-slack-kick-from-room
       "List user in channel" #'helm-slack-list-member-in-room
       "Join channel" #'helm-slack-join-room
       "Leave channel" #'helm-slack-leave-room
       "Rename channel" #'helm-slack-rename-room
       "Set purpose for channel" #'helm-slack-set-purpose-for-room
       "Set topic of channel" #'helm-slack-set-topic-of-room
       ))

(defvar helm-slack-members-actions
  (helm-make-actions "Display User" #'helm-slack-display-user))


(defvar helm-slack-channels-source (helm-build-sync-source "Channels (Slack)"
                                     :persistent-action #'helm-slack-persistent-action
                                     :action helm-slack-actions
                                     :candidates #'helm-slack-build-channels-candidates))

(defvar helm-slack-groups-source (helm-build-sync-source "Private Channels (Slack)"
                                   :persistent-action #'helm-slack-persistent-action
                                   :action helm-slack-actions
                                   :candidates #'helm-slack-build-groups-candidates))

(defvar helm-slack-ims-source (helm-build-sync-source "Direct Messages (Slack)"
                                :persistent-action #'helm-slack-persistent-action
                                :action helm-slack-actions
                                :candidates #'helm-slack-build-ims-candidates))

(defvar helm-slack-source (helm-build-sync-source "Slack"
                            :persistent-action #'helm-slack-persistent-action
                            :action helm-slack-actions
                            :candidates #'helm-slack-build-candidates))

(defcustom helm-slack-sources
  '(helm-slack-source)
  "Default helm sources.
pre defined sources are `helm-slack-channels-source', `helm-slack-groups-source', `helm-slack-ims-source', `helm-slack-source'"
  :type 'list
  :group 'slack)

(defun helm-slack-build-channels-candidates ()
  (helm-slack-build--candidates #'(lambda (team) (oref team channels))))

(defun helm-slack-build-groups-candidates ()
  (helm-slack-build--candidates #'(lambda (team) (oref team groups))))

(defun helm-slack-build-ims-candidates ()
  (helm-slack-build--candidates #'(lambda (team) (oref team ims))))

(defun helm-slack-build-candidates ()
  (helm-slack-build--candidates #'(lambda (team) (with-slots (channels groups ims) team
                                                   (append channels groups ims)))))

(defun helm-slack-build--candidates (rooms-selector)
  (cl-loop for team in slack-teams
           as rooms = (funcall rooms-selector team)
           nconc (cl-labels
                     ((filter (rooms) (cl-remove-if #'slack-room-hidden-p
                                                    rooms))
                      (collector (label room) (list label room team)))
                   (let ((slack-display-team-name
                          (< 1 (length slack-teams))))
                     (slack-room-names (append rooms nil) team
                                       #'filter #'collector)))))

(defmacro helm-slack-bind-room-and-team (candidate &rest body)
  (declare (indent 2) (debug t))
  `(let ((room (car ,candidate))
         (team (cadr ,candidate)))
     ,@body))

(defmacro helm-slack-bind-user-and-team (candidate &rest body)
  (declare (indent 2) (debug t))
  `(let ((user (car ,candidate))
         (team (cadr ,candidate)))
     ,@body))

(defun helm-slack-persistent-action (candidate)
  (helm-slack-bind-room-and-team candidate
      (let* ((buffer (slack-create-room-info-buffer room team)))
        (switch-to-buffer (slack-buffer-buffer buffer)))))

(defun helm-slack-display-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-room-display room team)))

(defun helm-slack-archive-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-archive room team)))

(defun helm-slack-invite-to-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-invite room team)))

(defun helm-slack-join-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-join room team)))

(defun helm-slack-leave-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-leave room team)))

(defun helm-slack-rename-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-rename room team)))

(defun helm-slack-set-purpose-for-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-set-purpose room team)))

(defun helm-slack-set-topic-of-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-set-topic room team)))

(defun helm-slack-kick-from-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-conversations-kick room team)))

(defun helm-slack-list-member-in-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (helm-slack-members-in-room room team)))

(defun helm-slack-members-in-room (room team)
  (let ((candidates nil)
        (cursor nil))
    (cl-labels
        ((inject-team (candidates)
                      (mapcar #'(lambda (candidate)
                                  (list (car candidate)
                                        (cdr candidate)
                                        team))
                              candidates))
         (on-members-success (members next-cursor)
                             (setq candidates (append candidates
                                                      members))
                             (setq cursor next-cursor)))
      (while (or (null cursor)
                 (< 0 (length cursor)))
        (slack-conversations-members room
                                     team
                                     cursor
                                     #'on-members-success))
      (helm
       :prompt "Select Member : "
       :sources (helm-build-sync-source "Members"
                  :candidates (inject-team candidates)
                  :action helm-slack-members-actions)))))

(defun helm-slack-display-user (candidate)
  (helm-slack-bind-user-and-team candidate
      (let* ((user-id (plist-get user :id))
             (buffer (slack-create-user-profile-buffer team
                                                       user-id)))
        (slack-buffer-display buffer))))

(defun helm-slack ()
  "Helm Slack"
  (interactive)
  (helm
   :prompt "Select Channel : "
   :sources helm-slack-sources))

(provide 'helm-slack)
;;; helm-slack.el ends here
