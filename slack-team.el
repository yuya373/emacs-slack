;;; slack-team.el ---  team class                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'eieio)
(require 'slack-util)
(require 'slack-team-ws)

(declare-function emojify-create-emojify-emojis "emojify")

(defvar slack-current-team nil)
(defvar slack-completing-read-function)
(defcustom slack-prefer-current-team nil
  "If set to t, using `slack-current-team' for interactive function.
use `slack-change-current-team' to change `slack-current-team'"
  :type 'boolean
  :group 'slack)

(defcustom slack-modeline-count-only-subscribed-channel t
  "Count unread only subscribed channel."
  :type 'boolean
  :group 'slack)

(defclass slack-team-threads ()
  ((initializedp :initform nil)
   (has-more :initform t)
   (total-unread-replies :initform 0 :type number)
   (new-threads-count :initform 0 :type number)))

(defclass slack-team ()
  ((id :initarg :id)
   (token :initarg :token :initform nil)
   (cookie :initarg :cookie :initform nil)
   (name :initarg :name :initform nil)
   (domain :initarg :domain)
   (self :initarg :self)
   (self-id :initarg :self-id)
   (self-name :initarg :self-name)
   (channels :initarg :channels :initform (make-hash-table :test 'equal))
   (groups :initarg :groups :initform (make-hash-table :test 'equal))
   (ims :initarg :ims :initform (make-hash-table :test 'equal))
   (file-room :initform nil)
   (search-results :initform nil)
   (users :initarg :users :initform (make-hash-table :test 'equal))
   (bots :initarg :bots :initform (make-hash-table :test 'equal))
   (sent-message :initform (make-hash-table :test 'equal))
   (message-id :initform 0)
   (subscribed-channels :initarg :subscribed-channels
                        :type list :initform nil)
   (typing :initform nil)
   (typing-timer :initform nil)
   (reminders :initform nil :type list)
   (threads :type slack-team-threads :initform (make-instance 'slack-team-threads))
   (modeline-enabled :initarg :modeline-enabled :initform nil)
   (modeline-name :initarg :modeline-name :initform nil)
   (websocket-event-log-enabled :initarg :websocket-event-log-enabled :initform nil)
   (waiting-requests :initform nil)
   (authorize-request :initform nil)
   (emoji-download-watch-timer :initform nil)
   (star :initform nil)
   (slack-message-buffer :initform nil :type (or null hash-table))
   (slack-file-info-buffer :initform nil :type (or null hash-table))
   (slack-file-list-buffer :initform nil :type (or null hash-table))
   (slack-message-edit-buffer :initform nil :type (or null hash-table))
   (slack-pinned-items-buffer :initform nil :type (or null hash-table))
   (slack-user-profile-buffer :initform nil :type (or null hash-table))
   (slack-thread-message-buffer :initform nil :type (or null hash-table))
   (slack-message-share-buffer :initform nil :type (or null hash-table))
   (slack-room-message-compose-buffer :initform nil :type (or null hash-table))
   (slack-thread-message-compose-buffer :initform nil :type (or null hash-table))
   (slack-stars-buffer :initform nil :type (or null hash-table))
   (slack-search-result-buffer :initform nil :type (or null hash-table))
   (slack-dialog-buffer :initform nil :type (or null hash-table))
   (slack-dialog-edit-element-buffer :initform nil :type (or null hash-table))
   (slack-room-info-buffer :initform nil :type (or null hash-table))
   (slack-all-threads-buffer :initform nil :type (or null hash-table))
   (slack-message-attachment-preview-buffer :initform nil :type (or null hash-table))
   (full-and-display-names :initarg :full-and-display-names :initform nil)
   (mark-as-read-immediately :initarg :mark-as-read-immediately :initform t)
   (commands :initform '() :type list)
   (usergroups :initarg :usergroups :initform '() :type list)
   (ws :type slack-team-ws)
   (files :initarg :files :initform (make-hash-table :test 'equal))
   (file-ids :initarg file-ids :initform '())
   (counts :initform nil)
   (emoji-master :initform (make-hash-table :test 'equal))
   (visible-threads :initarg :visible-threads :initform nil :type boolean)
   (animate-image :initarg :animate-image :initform nil :type boolean)
   (dnd-status :initform (make-hash-table :test 'equal))
   (presence :initform (make-hash-table :test 'equal))
   (disable-block-format :initform nil :initarg :disable-block-format :type boolean)
   ))

(defun slack-create-team (plist)
  (let ((ws (apply #'make-instance 'slack-team-ws
                   (slack-collect-slots 'slack-team-ws plist)))
        (team (apply #'make-instance 'slack-team
                     (slack-collect-slots 'slack-team plist))))
    (oset team ws ws)
    team))

(cl-defmethod slack-equalp ((this slack-team) other)
  (string= (oref this id) (oref other id)))

(cl-defmethod slack-team-set-ws-url ((this slack-team) url)
  (with-slots (ws) this
    (oset ws url url)))

(cl-defmethod slack-team-kill-buffers ((this slack-team) &key (except nil))
  (let* ((l (list 'slack-message-buffer
                  'slack-file-info-buffer
                  'slack-file-list-buffer
                  'slack-message-edit-buffer
                  'slack-pinned-items-buffer
                  'slack-user-profile-buffer
                  'slack-thread-message-buffer
                  'slack-message-share-buffer
                  'slack-room-message-compose-buffer
                  'slack-thread-message-compose-buffer
                  'slack-search-result-buffer
                  'slack-stars-buffer))
         (slots (cl-remove-if #'(lambda (e) (cl-find e except)) l)))
    (cl-loop for slot in slots
             do (let ((ht (slot-value this slot)))
                  (when (hash-table-p ht)
                    (cl-loop for buffer in (hash-table-values ht)
                             do (when buffer
                                  (kill-buffer (slack-buffer-buffer buffer)))))))))

(defvar slack-tokens-by-id (make-hash-table :test 'equal))
(defvar slack-teams-by-token (make-hash-table :test 'equal))
(defun slack-team-find-by-token (token)
  (gethash token slack-teams-by-token))

(defun slack-team-find (id)
  (let ((token (gethash id slack-tokens-by-id)))
    (when token
      (slack-team-find-by-token token))))

(cl-defmethod slack-team--delete ((this slack-team))
  (remhash (oref this id) slack-tokens-by-id)
  (remhash (oref this token) slack-teams-by-token))

(cl-defmethod slack-team-equalp ((team slack-team) other)
  (with-slots (token) team
    (string= token (oref other token))))

(cl-defmethod slack-team-name ((team slack-team))
  (oref team name))

(cl-defun slack-team-select (&optional no-default include-not-connected)
  (cl-labels ((select-team ()
                           (let* ((teams (if include-not-connected
                                             (hash-table-values slack-teams-by-token)
                                           (slack-team-connected-list)))
                                  (alist (mapcar #'(lambda (team) (cons (slack-team-name team)
                                                                        (oref team token)))
                                                 teams))
                                  (selected (funcall slack-completing-read-function "Select Team: " alist)))
                             (slack-team-find-by-token (cdr (cl-assoc selected alist :test #'string=))))))
    (let ((team (if (and slack-prefer-current-team
                         slack-current-team
                         (not no-default))
                    slack-current-team
                  (select-team))))
      ;; (if (and slack-prefer-current-team
      ;;          (not slack-current-team)
      ;;          (not no-default))
      ;;     (if (yes-or-no-p (format "Set %s to current-team?"
      ;;                              (oref team name)))
      ;;         (setq slack-current-team team)))
      team)))

(cl-defmethod slack-team-connectedp ((team slack-team))
  (oref (oref team ws) connected))

(defun slack-team-connected-list ()
  (cl-remove-if #'null
                (mapcar #'(lambda (team)
                            (if (slack-team-connectedp team) team))
                        (hash-table-values slack-teams-by-token))))

(defun slack-team-modeline-enabledp (team)
  (oref team modeline-enabled))

(cl-defmethod slack-team-event-log-enabledp ((team slack-team))
  (oref team websocket-event-log-enabled))

(cl-defmethod slack-team-mark-as-read-immediatelyp ((team slack-team))
  (oref team mark-as-read-immediately))

(defvar slack-team-random-numbers-for-client-token
  (let ((result nil))
    (dotimes (_ 10)
      (push (random 10) result))
    (mapconcat #'number-to-string result "")))

(cl-defmethod slack-team-client-token ((team slack-team))
  (format "EmacsSlack-%s-%s"
          (oref team id)
          slack-team-random-numbers-for-client-token))

(cl-defmethod slack-team-inc-message-id ((team slack-team))
  (with-slots (message-id) team
    (if (eq message-id (1- most-positive-fixnum))
        (setq message-id 1)
      (cl-incf message-id))))

(defun slack-team-watch-emoji-download-complete (team paths)
  (if (eq (length (cl-remove-if #'identity (mapcar #'file-exists-p paths)))
          0)
      (when (timerp (oref team emoji-download-watch-timer))
        (cancel-timer (oref team emoji-download-watch-timer))
        (oset team emoji-download-watch-timer nil)
        (emojify-create-emojify-emojis t))))

(cl-defmethod slack-team-token ((this slack-team))
  (oref this token))

(cl-defmethod slack-team-cookie ((this slack-team))
  (oref this cookie))

(cl-defmethod slack-team-missing-user-ids ((this slack-team) user-ids)
  (let ((exists-user-ids (hash-table-keys (oref this users))))
    (cl-remove-if #'(lambda (e) (cl-find e exists-user-ids :test #'string=))
                  (cl-remove-duplicates user-ids :test #'string=))))

(cl-defmethod slack-team-visible-threads-p ((this slack-team))
  (oref this visible-threads))

(cl-defmethod slack-team-animate-image-p ((this slack-team))
  (oref this animate-image))

(cl-defmethod slack-team-channels ((this slack-team))
  (hash-table-values (oref this channels)))

(cl-defmethod slack-team-groups ((this slack-team))
  (hash-table-values (oref this groups)))

(cl-defmethod slack-team-ims ((this slack-team))
  (hash-table-values (oref this ims)))

(cl-defmethod slack-team-users ((this slack-team))
  (hash-table-values (oref this users)))

(cl-defmethod slack-team-set-users ((this slack-team) users)
  (cl-loop for user in users
           do (puthash (plist-get user :id)
                       user
                       (oref this users))))

(cl-defmethod slack-team-set-bots ((this slack-team) bots)
  (cl-loop for bot in bots
           do (puthash (plist-get bot :id)
                       bot
                       (oref this bots))))

(cl-defmethod slack-team-bots ((this slack-team))
  (hash-table-values (oref this bots)))

(cl-defmethod slack-team-files ((this slack-team))
  (let ((ret))
    (cl-loop for id in (oref this file-ids)
             do (push (gethash id (oref this files))
                      ret))
    ret))

(cl-defmethod slack-team-id ((this slack-team))
  (oref this id))

(provide 'slack-team)
;;; slack-team.el ends here
