;;; slack-room-buffer.el ---                         -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'alert)
(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-request)
(require 'slack-file)
(require 'slack-team)
(require 'slack-buffer)
(require 'slack-message-reaction)
(require 'slack-thread)
(require 'slack-message-notification)
(require 'slack-slash-commands)
(require 'slack-action)
(require 'slack-message-share-buffer)
(require 'slack-reminder)

(defvar slack-completing-read-function)
(defvar slack-alert-icon)
(defvar slack-message-minibuffer-local-map nil)

(defvar slack-expand-email-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
      #'slack-toggle-email-expand)
    map))

(defvar slack-attachment-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-attachment-action-run)
    (define-key keymap [mouse-1] #'slack-attachment-action-run)
    keymap))

(defvar slack-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-action-run)
    (define-key keymap [mouse-1] #'slack-action-run)
    keymap))

(defconst slack-message-delete-url "https://slack.com/api/chat.delete")
(defconst slack-get-permalink-url "https://slack.com/api/chat.getPermalink")

(defclass slack-room-buffer (slack-buffer)
  ((room :initarg :room :type slack-room)))

(cl-defmethod slack-buffer-room ((this slack-room-buffer))
  (oref this room))

(cl-defmethod slack-buffer-toggle-reaction ((this slack-room-buffer) reaction)
  (let* ((reaction-users (oref reaction users))
         (reaction-name (oref reaction name))
         (team (oref this team))
         (room (oref this room))
         (self-id (oref team self-id)))
    (if (cl-find-if #'(lambda (id) (string= id self-id)) reaction-users)
        (slack-message-reaction-remove reaction-name
                                       (slack-get-ts)
                                       room
                                       team)
      (slack-buffer-add-reaction-to-message this
                                            reaction-name
                                            (slack-get-ts)))))

(cl-defmethod slack-buffer-reaction-help-text ((this slack-room-buffer) reaction)
  (with-slots (team) this
    (slack-reaction-help-text reaction team)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-room-buffer)) room team)
  (slack-if-let* ((room-name (slack-room-name room team)))
      (format  "*Slack - %s : %s"
               (oref team name)
               room-name)))

(cl-defmethod slack-buffer-name ((this slack-room-buffer))
  (with-slots (room team) this
    (slack-buffer-name (eieio-object-class-name this) room team)))

(cl-defmethod slack-buffer-delete-message ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (cl-labels
            ((on-delete
              (&key data &allow-other-keys)
              (slack-request-handle-error
               (data "slack-message-delete"))))
          (if (yes-or-no-p "Are you sure you want to delete this message?")
              (slack-request
               (slack-request-create
                slack-message-delete-url
                team
                :type "POST"
                :params (list (cons "ts" (slack-ts message))
                              (cons "channel" (oref room id)))
                :success #'on-delete))
            (message "Canceled"))))))

(cl-defmethod slack-buffer-message-delete ((this slack-room-buffer) ts)
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (lui-delete #'(lambda () (equal (get-text-property (point) 'ts)
                                      ts))))))

(cl-defmethod slack-buffer-copy-link ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts))
                    (template "https://%s.slack.com/archives/%s/p%s%s"))
        (cl-labels
            ((on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-get-permalink")
                          (let ((permalink (plist-get data :permalink)))
                            (kill-new permalink)
                            (message "Link Copied to Clipboard")))))
          (slack-request
           (slack-request-create
            slack-get-permalink-url
            team
            :type "POST"
            :params (list (cons "channel" (oref room id))
                          (cons "message_ts" ts))
            :success #'on-success))))))

(cl-defmethod slack-buffer--replace ((this slack-room-buffer) ts)
  (with-slots (room) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-buffer-replace this message))))

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-room-buffer) file-id)
  (with-slots (room) this
    (slack-if-let* ((ts (get-text-property (point) 'ts))
                    (message (slack-room-find-message room ts))
                    (file (cl-find-if
                           #'(lambda (e) (string= (oref e id)
                                                  file-id))
                           (oref message files))))
        (progn
          (oset file is-expanded (not (oref file is-expanded)))
          (slack-buffer-update this message :replace t)))))

(defun slack-toggle-email-expand ()
  (interactive)
  (let ((buffer slack-current-buffer))
    (slack-if-let* ((file-id (get-text-property (point) 'id)))
        (slack-buffer-toggle-email-expand buffer file-id))))

(cl-defmethod slack-buffer-pins-remove ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-message-pins-request slack-message-pins-remove-url
                                room team ts)))

(cl-defmethod slack-buffer-pins-add ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-message-pins-request slack-message-pins-add-url
                                room team ts)))

(cl-defmethod slack-buffer-remove-star ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-remove-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(cl-defmethod slack-buffer-add-star ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (slack-if-let* ((message (slack-room-find-message room ts)))
        (slack-message-star-api-request slack-message-stars-add-url
                                        (list (cons "channel" (oref room id))
                                              (slack-message-star-api-params message))
                                        team))))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-room-buffer) reaction ts)
  (with-slots (room team) this
    (slack-message-reaction-add reaction ts room team)))

(cl-defmethod slack-buffer-remove-reaction-from-message ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (reactions (slack-message-reactions message))
           (reaction (slack-message-reaction-select reactions)))
      (slack-message-reaction-remove reaction ts room team))))

(cl-defmethod slack-buffer-share-message ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-message-share-buffer room team ts)))
      (slack-buffer-display buf))))

(cl-defmethod slack-buffer-display-edit-message-buffer ((this slack-room-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-edit-message-buffer room team ts)))
      (slack-buffer-display buf))))

(cl-defmethod slack-buffer-update-mark ((_this slack-room-buffer) &key (_force nil)))

(cl-defmethod slack-buffer-builtin-actions ((this slack-room-buffer) ts handler)
  (let ((display-follow nil))
    (with-slots (team room) this
      (cl-labels
          ((get-message () (slack-room-find-message room ts))
           (handle-follow-message () (slack-subscriptions-thread-add room ts team))
           (handle-unfollow-message () (slack-subscriptions-thread-remove room ts team))
           (handle-copy-link () (slack-buffer-copy-link this ts))
           (handle-mark-unread () (slack-buffer-update-mark this :force t))
           (handle-pin () (slack-buffer-pins-add this ts))
           (handle-un-pin () (slack-buffer-pins-remove this ts))
           (handle-delete-message () (slack-buffer-delete-message this ts))
           (handle-star-message () (slack-buffer-add-star this ts))
           (handle-unstar-message () (slack-buffer-remove-star this ts))
           (handle-add-reaction () (let ((reaction (slack-message-reaction-input)))
                                     (slack-buffer-add-reaction-to-message
                                      this reaction ts)))
           (handle-remove-reaction () (slack-buffer-remove-reaction-from-message
                                       this ts))
           (handle-share () (slack-buffer-share-message this ts))
           (handle-edit () (slack-buffer-display-edit-message-buffer this
                                                                     ts))
           (handle-remind () (slack-if-let* ((message (get-message)))
                                 (slack-reminder-add-from-message room
                                                                  message
                                                                  team)))
           (display-pin-p ()
                          (slack-if-let* ((message (get-message)))
                              (not (slack-message-pinned-to-room-p message room))))
           (display-un-pin-p ()
                             (slack-if-let* ((message (get-message)))
                                 (slack-message-pinned-to-room-p message room)))
           (display-follow-p () display-follow)
           (display-unfollow-p () (not display-follow))
           (display-star-p () (slack-if-let* ((message (get-message)))
                                  (not (slack-message-starred-p message))))
           (display-unstar-p () (slack-if-let* ((message (get-message)))
                                    (slack-message-starred-p message)))
           (message-buffer-p () (eq (eieio-object-class this)
                                    'slack-message-buffer)))
        (let ((builtins `(:app_name
                          "Slack"
                          :actions
                          ((:name "Follow message"
                                  :handler ,#'handle-follow-message
                                  :display-p ,#'display-follow-p)
                           (:name "Unfollow message"
                                  :handler ,#'handle-unfollow-message
                                  :display-p ,#'display-unfollow-p)
                           (:name "Star message"
                                  :handler ,#'handle-star-message
                                  :display-p ,#'display-star-p)
                           (:name "Unstar message"
                                  :handler ,#'handle-unstar-message
                                  :display-p ,#'display-unstar-p)
                           (:name "Add reaction to message"
                                  :handler ,#'handle-add-reaction)
                           (:name "Remove reaction from message"
                                  :handler ,#'handle-remove-reaction)
                           (:name "Edit message"
                                  :handler ,#'handle-edit)
                           (:name "Share message"
                                  :handler ,#'handle-share)
                           (:name "Copy link"
                                  :handler ,#'handle-copy-link)
                           (:name "Mark unread"
                                  :display-p ,#'message-buffer-p
                                  :handler ,#'handle-mark-unread)
                           (:name "Remind me about this"
                                  :handler ,#'handle-remind)
                           (:name ,(format "Pin to %s%s"
                                           (if (slack-im-p room) "@" "#")
                                           (slack-room-name room team))
                                  :display-p ,#'display-pin-p
                                  :handler ,#'handle-pin)
                           (:name ,(format "Un-pin from %s%s"
                                           (if (slack-im-p room) "@" "#")
                                           (slack-room-name room team))
                                  :display-p ,#'display-un-pin-p
                                  :handler ,#'handle-un-pin)
                           (:name "Delete message"
                                  :handler ,#'handle-delete-message)))))
          (cl-labels
              ((on-success (subscriptions)
                           (if (cl-find ts subscriptions :test #'string=)
                               (setq display-follow nil)
                             (setq display-follow t))
                           (funcall handler builtins))
               (on-error (_err) (funcall handler builtins)))
            (slack-subscriptions-thread-get room ts team #'on-success #'on-error)))))))

(cl-defmethod slack-buffer-execute-message-action ((this slack-room-buffer) ts)
  (with-slots (team room) this
    (cl-labels
        ((run-action (selected)
                     (slack-if-let*
                         ((action (cdr selected))
                          (app (car selected))
                          (type (plist-get action :type))
                          (action-id (plist-get action :action_id))
                          (app-id (plist-get app :app_id)))
                         (slack-actions-run ts room type action-id app-id team)))
         (handler (builtin actions)
                  (slack-if-let*
                      ((selected (slack-actions-select (cons builtin actions)))
                       (action (cdr selected)))
                      (if (functionp (plist-get action :handler))
                          (funcall (plist-get action :handler))
                        (run-action selected))))
         (on-success
          (actions)
          (slack-buffer-builtin-actions
           this ts
           #'(lambda (builtin) (run-at-time nil nil #'handler builtin actions))))
         (on-error
          (_err)
          (slack-buffer-builtin-actions
           this ts
           #'(lambda (builtin) (run-at-time nil nil #'handler builtin nil)))))
      (slack-actions-list team #'on-success #'on-error))))

(cl-defmethod slack-message-deleted ((message slack-message) room team)
  (if (slack-thread-message-p message)
      (slack-if-let* ((parent (slack-room-find-thread-parent room message))
                      (thread (slack-message-get-thread parent)))
          (progn
            (slack-thread-delete-message thread message)
            (slack-if-let* ((buffer (slack-buffer-find 'slack-thread-message-buffer
                                                       room
                                                       (oref thread thread-ts)
                                                       team)))
                (slack-buffer-message-delete buffer (slack-ts message)))
            (slack-message-update parent team t)))
    (slack-if-let* ((buf (slack-buffer-find 'slack-message-buffer
                                            room
                                            team)))
        (slack-buffer-message-delete buf (slack-ts message))))

  (if slack-message-custom-delete-notifier
      (funcall slack-message-custom-delete-notifier message room team)
    (alert "message deleted"
           :icon slack-alert-icon
           :title (format "\\[%s] from %s"
                          (slack-room-display-name room team)
                          (slack-message-sender-name message team))
           :category 'slack)))

(defun slack-message-delete ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-delete-message buf (slack-get-ts))))

(defun slack-message-copy-link ()
  (interactive)
  (slack-buffer-copy-link slack-current-buffer (slack-get-ts)))

(defun slack-message-test-notification ()
  "Debug notification.
Execute this function when cursor is on some message."
  (interactive)
  (let ((ts (slack-get-ts)))
    (with-slots (room team) slack-current-buffer
      (let ((message (slack-room-find-message room ts)))
        (slack-message-notify message room team)))))

(defun slack--get-channel-id ()
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-if-let* ((buffer slack-current-buffer)
                    (boundp (slot-boundp buffer 'room))
                    (room (oref buffer room)))
        (progn
          (kill-new (oref room id))
          (message "%s" (oref room id))))))

(defun slack-attachment-action-run ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (oref buffer room))
                  (team (oref buffer team))
                  (type (get-text-property (point) 'type))
                  (attachment-id (get-text-property (point) 'attachment-id))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property (point) 'action)))
      (when (slack-attachment-action-confirm action)
        (slack-if-let* ((callback-id (get-text-property (point) 'callback-id))
                        (common-payload (list
                                         (cons "attachment_id" (number-to-string
                                                                attachment-id))
                                         (cons "callback_id" callback-id)
                                         (cons "is_ephemeral" (oref message
                                                                    is-ephemeral))
                                         (cons "message_ts" ts)
                                         (cons "channel_id" (oref room id))))
                        (service-id (if (slack-bot-message-p message)
                                        (slack-message-bot-id message)
                                      "B01")))
            (let ((url "https://slack.com/api/chat.attachmentAction")
                  (params (list (cons "payload"
                                      (json-encode-alist
                                       (slack-attachment-action-run-payload
                                        action
                                        team
                                        common-payload
                                        service-id)))
                                (cons "service_id" service-id)
                                (cons "client_token"
                                      (slack-team-client-token team)))))
              (cl-labels
                  ((log-error (err)
                              (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                                 err
                                                 url
                                                 params)
                                         team
                                         :level 'error))
                   (on-success (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-attachment-action-run" #'log-error))))
                (slack-request
                 (slack-request-create
                  url
                  team
                  :type "POST"
                  :params params
                  :success #'on-success))))
          (slack-if-let* ((url (oref action url)))
              (browse-url url))))))

(defun slack-message-run-action ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (oref buffer room))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (not-ephemeral-messagep (not (oref message is-ephemeral))))
      (slack-buffer-execute-message-action buffer ts)))

(defun slack-action-run ()
  (interactive)
  (slack-if-let* ((bot (get-text-property (point) 'bot))
                  (payload (get-text-property (point) 'payload))
                  (buffer slack-current-buffer)
                  (team (oref buffer team)))
      (let ((url "https://slack.com/api/chat.action")
            (params (list (cons "bot" bot)
                          (cons "payload" payload))))
        (cl-labels
            ((log-error (err) (format "Error: %s, URL: %s, PARAMS: %s"
                                      err
                                      url
                                      params))
             (on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-action-run" #'log-error))))
          (slack-request
           (slack-request-create
            url
            team
            :type "POST"
            :params params
            :success #'on-success))))))

(defun slack-message--send (message)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (oref buf team))
                  (room (oref buf room)))
      (if (string-prefix-p "/" message)
          (slack-if-let* ((command-and-arg (slack-slash-commands-parse message team)))
              (slack-command-run (car command-and-arg)
                                 team
                                 (oref room id)
                                 :text (cdr command-and-arg))
            (error "Unknown slash command: %s"
                   (car (split-string message))))
        (slack-buffer-send-message buf message))))

(defun slack-message-send ()
  (interactive)
  (slack-message--send (slack-message-read-from-minibuffer)))

(defun slack-message-setup-minibuffer-keymap ()
  (unless slack-message-minibuffer-local-map
    (setq slack-message-minibuffer-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'newline)
            (set-keymap-parent map minibuffer-local-map)
            map))))

(defun slack-message-read-from-minibuffer ()
  (let ((prompt "Message: "))
    (slack-message-setup-minibuffer-keymap)
    (read-from-minibuffer
     prompt
     nil
     slack-message-minibuffer-local-map)))

(defun slack-message-follow ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-follow-message buffer)))

(cl-defmethod slack-buffer-follow-message ((this slack-room-buffer))
  (slack-if-let* ((ts (slack-get-ts)))
      (with-slots (room team) this
        (cl-labels
            ((after-success ()
                            (slack-log "Successfully followed."
                                       team :level 'info)))
          (slack-subscriptions-thread-add room ts team
                                          #'after-success)))))

(defun slack-message-unfollow ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-unfollow-message buffer)))

(cl-defmethod slack-buffer-unfollow-message ((this slack-room-buffer))
  (slack-if-let* ((ts (slack-get-ts)))
      (with-slots (room team) this
        (cl-labels
            ((after-success ()
                            (slack-log "Successfully unfollowed."
                                       team :level 'info)))
          (slack-subscriptions-thread-remove room ts team
                                             #'after-success)))))




(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
