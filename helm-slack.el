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
(require 'slack-message-buffer)
(require 'slack-unread)
(require 'slack-util)

(defvar slack-display-team-name)

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
  (helm-slack-build--candidates #'slack-team-channels))

(defun helm-slack-build-groups-candidates ()
  (helm-slack-build--candidates #'slack-team-groups))

(defun helm-slack-build-ims-candidates ()
  (helm-slack-build--candidates #'slack-team-ims))

(defun helm-slack-build-candidates ()
  (helm-slack-build--candidates #'(lambda (team) (append (slack-team-channels team)
                                                         (slack-team-groups team)
                                                         (slack-team-ims team)))))

(defun helm-slack-build--candidates (rooms-selector)
  (cl-labels ((sort (rooms)
                    (nreverse (cl-sort rooms #'string< :key #'latest-ts)))
              (latest-ts (label-room-team)
                         (cl-destructuring-bind (_label room team) label-room-team
                           (slack-room-latest room team))))

    (sort (cl-loop for team in slack-teams
                   as rooms = (funcall rooms-selector team)
                   nconc (cl-labels
                             ((filter (rooms) (cl-remove-if #'slack-room-hidden-p
                                                            rooms))
                              (collector (label room) (list label room team)))
                           (let ((slack-display-team-name
                                  (< 1 (length slack-teams))))
                             (cl-loop for room in (filter rooms)
                                      collect (collector (slack-room-label room team)
                                                         room))))))))

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

(defmacro helm-slack-handle-next-page (candidate if-next-page
                                                 &rest body)
  (declare (indent 2) (debug t))
  `(let ((token (car ,candidate)))
     (if (and (stringp token)
              (string= token slack-next-page-token))
         ,if-next-page
       ,@body)))

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

(defun helm-slack-members-in-room (room team &optional cursor)
  (cl-labels
      ((success (members next-cursor)
                (let ((candidates (cl-remove-if #'null
                                                (cl-loop for member in members
                                                         collect (slack-if-let*
                                                                     ((user (slack-user--find member team))
                                                                      (not-hidden (not (slack-user-hidden-p user))))
                                                                     (list (slack-user-label user team)
                                                                           user
                                                                           team))))))
                  (when (< 0 (length next-cursor))
                    (setq candidates (append candidates
                                             (list (list slack-next-page-token
                                                         slack-next-page-token
                                                         room team next-cursor)))))
                  (helm
                   :prompt "Select Member : "
                   :sources (helm-build-sync-source "Members"
                              :candidates candidates
                              :action helm-slack-members-actions)))))
    (slack-conversations-members room team cursor #'success)))

(defun helm-slack-display-user (candidate)
  (helm-slack-handle-next-page candidate
      (cl-destructuring-bind (_token room team next-cursor) candidate
        (helm-slack-members-in-room room team next-cursor))
    (helm-slack-bind-user-and-team candidate
        (let* ((user-id (plist-get user :id))
               (buffer (slack-create-user-profile-buffer team
                                                         user-id)))
          (slack-buffer-display buffer)))))

(defun helm-slack ()
  "Helm Slack"
  (interactive)
  (helm
   :prompt "Select Channel : "
   :sources helm-slack-sources))

(defvar helm-slack-unreads-actions
  (helm-make-actions
   "Display channel" #'helm-slack-display-room
   "Collapse channel" #'helm-slack-unreads-collapse-room
   "Expand channel" #'helm-slack-unreads-expand-room
   ))

(defun helm-slack-unreads-collapse-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-unread-collapse room team)))

(defun helm-slack-unreads-expand-room (candidate)
  (helm-slack-bind-room-and-team candidate
      (slack-unread-expand room team)))

(defun helm-slack-unreads ()
  (interactive)
  (let ((waiting-complete 0)
        (candidates nil)
        (timer nil))
    (cl-labels
        ((build-candidate
          (channel team)
          (slack-if-let*
              ((room (slack-room-find (plist-get channel :channel_id)
                                      team)))
              (list (format "%s%s %s (%s)"
                            (slack-room-label-prefix room team)
                            (slack-room-display-name room team)
                            (if (eq t (plist-get channel :collapsed))
                                " :arrow_forward:"
                              " :arrow_down_small:")
                            (plist-get channel :total_unreads))
                    room
                    team)))
         (success
          (channels-count channels team)
          (cl-decf waiting-complete)
          (if (< channels-count 1)
              (slack-log "No unread messages"
                         team
                         :level 'info)
            (cl-loop for channel in channels
                     do (push (build-candidate channel team)
                              candidates))))
         (on-error (err team)
                   (if (string= "not_allowed_token_type"
                                err)
                       (slack-log "Your workspace doesn't support unread.history. \
use `slack-select-unread-rooms' instead."
                                  team
                                  :level 'info)
                     (slack-log (format "Error in unread.history: %S" err)
                                team
                                :level 'error))
                   (cl-decf waiting-complete)))
      (mapc #'(lambda (team)
                (cl-incf waiting-complete)
                (slack-unread-history team
                                      #'(lambda (channels-count
                                                 _total_messages-count
                                                 channels)
                                          (condition-case _err
                                              (success channels-count
                                                       channels
                                                       team)
                                            (error (when (timerp timer)
                                                     (cancel-timer timer)))))
                                      :on-error #'on-error))
            slack-teams))

    (setq timer
          (run-at-time t 0.5 #'(lambda ()
                                 (when (< waiting-complete 1)
                                   (cancel-timer timer)
                                   (helm
                                    :prompt "Select Channel : "
                                    :sources (list (helm-build-sync-source "All Unreads"
                                                     :persistent-action #'helm-slack-persistent-action
                                                     :action helm-slack-unreads-actions
                                                     :candidates candidates)))))))))

(provide 'helm-slack)
;;; helm-slack.el ends here
