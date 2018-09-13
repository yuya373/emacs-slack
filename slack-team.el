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

(defvar slack-teams nil)
(defvar slack-current-team nil)
(defcustom slack-prefer-current-team nil
  "If set to t, using `slack-current-team' for interactive function.
use `slack-change-current-team' to change `slack-current-team'"
  :group 'slack)

(defcustom slack-modeline-count-only-subscribed-channel t
  "Count unread only subscribed channel."
  :group 'slack)

(defclass slack-team-threads ()
  ((initializedp :initform nil)
   (has-more :initform t)
   (total-unread-replies :initform 0 :type number)
   (new-threads-count :initform 0 :type number)))

(defclass slack-team ()
  ((id :initarg :id)
   (token :initarg :token :initform nil)
   (client-id :initarg :client-id)
   (client-secret :initarg :client-secret)
   (name :initarg :name :initform nil)
   (domain :initarg :domain)
   (self :initarg :self)
   (self-id :initarg :self-id)
   (self-name :initarg :self-name)
   (channels :initarg :channels :initform nil)
   (groups :initarg :groups :initform nil)
   (ims :initarg :ims :initform nil)
   (file-room :initform nil)
   (search-results :initform nil)
   (users :initarg :users :initform nil)
   (bots :initarg :bots :initform nil)
   (ws-url :initarg :ws-url)
   (ws-conn :initarg :ws-conn :initform nil)
   (ping-timer :initform nil)
   (check-ping-timeout-timer :initform nil)
   (check-ping-timeout-sec :initarg :check-ping-timeout-sec :initform 20)
   (reconnect-auto :initarg :reconnect-auto :initform t)
   (reconnect-timer :initform nil)
   (reconnect-after-sec :initform 10)
   (reconnect-count :initform 0)
   (reconnect-count-max :initform 360)
   (last-pong :initform nil)
   (waiting-send :initform nil)
   (sent-message :initform (make-hash-table))
   (message-id :initform 0)
   (connected :initform nil)
   (subscribed-channels :initarg :subscribed-channels
                        :type list :initform nil)
   (typing :initform nil)
   (typing-timer :initform nil)
   (reminders :initform nil :type list)
   (ping-check-timers :initform (make-hash-table :test 'equal))
   (threads :type slack-team-threads :initform (make-instance 'slack-team-threads))
   (modeline-enabled :initarg :modeline-enabled :initform nil)
   (modeline-name :initarg :modeline-name :initform nil)
   (websocket-event-log-enabled :initarg :websocket-event-log-enabled :initform nil)
   (display-profile-image :initarg :display-profile-image :initform nil)
   (display-attachment-image-inline :initarg :display-attachment-image-inline :initform nil)
   (display-file-image-inline :initarg :display-file-image-inline :initform nil)
   (waiting-requests :initform nil)
   (authorize-request :initform nil)
   (emoji-download-watch-timer :initform nil)
   (websocket-nowait :initarg :websocket-nowait :initform nil)
   (star :initform nil)
   (slack-message-buffer :initform nil :type (or null list))
   (slack-file-info-buffer :initform nil :type (or null list))
   (slack-file-list-buffer :initform nil :type (or null list))
   (slack-message-edit-buffer :initform nil :type (or null list))
   (slack-pinned-items-buffer :initform nil :type (or null list))
   (slack-user-profile-buffer :initform nil :type (or null list))
   (slack-thread-message-buffer :initform nil :type (or null list))
   (slack-message-share-buffer :initform nil :type (or null list))
   (slack-room-message-compose-buffer :initform nil :type (or null list))
   (slack-thread-message-compose-buffer :initform nil :type (or null list))
   (slack-stars-buffer :initform nil :type (or null list))
   (slack-search-result-buffer :initform nil :type (or null list))
   (slack-dialog-buffer :initform nil :type (or null list))
   (slack-dialog-edit-element-buffer :initform nil :type (or null list))
   (slack-room-info-buffer :initform nil :type (or null list))
   (reconnect-url :initform "" :type string)
   (full-and-display-names :initarg :full-and-display-names :initform nil)
   (websocket-connect-timeout-timer :initform nil)
   (websocket-connect-timeout-sec :type number :initform 20) ;; websocket url is valid for 30 seconds.
   (mark-as-read-immediately :initarg :mark-as-read-immediately :initform t)
   (inhibit-reconnection :initform nil)
   (commands :initform '() :type list)
   ))

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
             do (cl-loop for buffer in (slot-value this slot)
                         do (kill-buffer buffer)))))

(defun slack-team-find (id)
  (cl-find-if #'(lambda (team) (string= id (oref team id)))
              slack-teams))

(defmethod slack-team-disconnect ((team slack-team))
  (slack-ws-close team))

(defmethod slack-team-equalp ((team slack-team) other)
  (with-slots (token) team
    (string= token (oref other token))))

(defmethod slack-team-name ((team slack-team))
  (oref team name))

;;;###autoload
(defun slack-register-team (&rest plist)
  "PLIST must contain :name :client-id :client-secret with value.
setting :token will reduce your configuration step.
you will notified when receive message with channel included in subscribed-channels.
if :default is t and `slack-prefer-current-team' is t, skip selecting team when channels listed.
you can change current-team with `slack-change-current-team'"
  (interactive
   (let ((name (read-from-minibuffer "Team Name: "))
         (client-id (read-from-minibuffer "Client Id: "))
         (client-secret (read-from-minibuffer "Client Secret: "))
         (token (read-from-minibuffer "Token: ")))
     (list :name name :client-id client-id :client-secret client-secret
           :token token)))
  (cl-labels ((same-client-id
               (client-id)
               (cl-find-if #'(lambda (team)
                               (string= client-id (oref team client-id)))
                           slack-teams))
              (missing (plist)
                       (cl-remove-if
                        #'null
                        (mapcar #'(lambda (key)
                                    (unless (plist-member plist key)
                                      key))
                                '(:name :client-id :client-secret)))))
    (let ((missing (missing plist)))
      (if missing
          (error "Missing Keyword: %s" missing)))
    (let ((team (apply #'slack-team "team"
                       (slack-collect-slots 'slack-team plist))))
      (let ((same-team (cl-find-if
                        #'(lambda (o) (slack-team-equalp team o))
                        slack-teams)))
        (if same-team
            (progn
              (slack-team-disconnect same-team)
              (slack-start team))))

      (setq slack-teams
            (cons team
                  (cl-remove-if #'(lambda (other)
                                    (slack-team-equalp team other))
                                slack-teams)))
      (if (plist-get plist :default)
          (setq slack-current-team team)))))

(defun slack-team-find-by-name (name)
  (if name
      (cl-find-if #'(lambda (team) (string= name (oref team name)))
                  slack-teams)))

(cl-defun slack-team-select (&optional no-default include-not-connected)
  (cl-labels ((select-team ()
                           (slack-team-find-by-name
                            (funcall slack-completing-read-function
                                     "Select Team: "
                                     (mapcar #'(lambda (team) (oref team name))
                                             (if include-not-connected
                                                 slack-teams
                                               (slack-team-connected-list)))))))
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

(defmethod slack-team-connectedp ((team slack-team))
  (oref team connected))

(defun slack-team-connected-list ()
  (cl-remove-if #'null
                (mapcar #'(lambda (team)
                            (if (slack-team-connectedp team) team))
                        slack-teams)))

(defun slack-change-current-team ()
  (interactive)
  (let ((team (slack-team-find-by-name
               (funcall slack-completing-read-function
                        "Select Team: "
                        (mapcar #'(lambda (team) (oref team name))
                                slack-teams)))))
    (setq slack-current-team team)
    (message "Set slack-current-team to %s" (or (and team (oref team name))
                                                "nil"))
    (setq slack-teams
          (cons team (cl-remove-if #'(lambda (e)
                                       (string= (oref e id)
                                                (oref slack-current-team id)))
                                   slack-teams)))
    (if team
        (slack-team-connect team))))

(defmethod slack-team-connect ((team slack-team))
  (unless (slack-team-connectedp team)
    (slack-start team)))

(defun slack-team-delete ()
  (interactive)
  (let ((selected (slack-team-select t t)))
    (if (yes-or-no-p (format "Delete %s from `slack-teams'?"
                             (oref selected name)))
        (progn
          (setq slack-teams
                (cl-remove-if #'(lambda (team)
                                  (slack-team-equalp selected team))
                              slack-teams))
          (slack-team-disconnect selected)
          (message "Delete %s from `slack-teams'" (oref selected name))))))

(defmethod slack-team-init-ping-check-timers ((team slack-team))
  (oset team ping-check-timers (make-hash-table :test 'equal)))

(defmethod slack-team-get-ping-check-timers ((team slack-team))
  (if (not (slot-boundp team 'ping-check-timers))
      (slack-team-init-ping-check-timers team))
  (oref team ping-check-timers))

(defmethod slack-team-need-token-p ((team slack-team))
  (with-slots (token) team
    (or (not token) (< (length token) 1))))

(defun slack-team-get-unread-messages (team)
  (cl-labels
      ((count-unread (rooms)
                     (cl-reduce #'(lambda (a e) (+ a (oref e unread-count-display)))
                                rooms :initial-value 0)))
    (with-slots (ims channels groups) team
      (let ((rooms (append ims channels groups)))
        (+ (count-unread (if slack-modeline-count-only-subscribed-channel
                             (cl-remove-if-not #'(lambda (e) (slack-room-subscribedp e team))
                                               rooms)
                           rooms)))))))

(defun slack-team-modeline-enabledp (team)
  (oref team modeline-enabled))

(defmethod slack-team-event-log-enabledp ((team slack-team))
  (oref team websocket-event-log-enabled))

(defmethod slack-team-display-profile-imagep ((team slack-team))
  (oref team display-profile-image))

(defmethod slack-team-display-attachment-image-inlinep ((team slack-team))
  (oref team display-attachment-image-inline))

(defmethod slack-team-display-file-image-inlinep ((team slack-team))
  (oref team display-file-image-inline))

(defmethod slack-team-mark-as-read-immediatelyp ((team slack-team))
  (oref team mark-as-read-immediately))

(defvar slack-team-random-numbers-for-client-token
  (let ((result nil))
    (dotimes (_ 10)
      (push (random 10) result))
    (mapconcat #'number-to-string result "")))

(defmethod slack-team-client-token ((team slack-team))
  (format "EmacsSlack-%s-%s"
          (oref team id)
          slack-team-random-numbers-for-client-token))

(provide 'slack-team)
;;; slack-team.el ends here
