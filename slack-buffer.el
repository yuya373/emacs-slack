;;; slack-buffer.el --- slack buffer                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

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

(require 'color)
(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-room)
(require 'slack-image)
(require 'slack-message-formatter)
(declare-function emojify-mode "emojify")

(defvar slack-buffer-function)
(defvar slack-completing-read-function)
(defvar-local slack-current-buffer nil)
(defvar lui-prompt-string "> ")
(defvar slack-typing-visibility)

(defcustom slack-default-directory
  (expand-file-name (concat (or (getenv "HOME") "~") "/"))
  "default directory at Slack Buffer."
  :type 'string
  :group 'slack)

(defvar slack-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-s C-r") #'slack-room-update-messages)
    ;; (define-key map (kbd "C-s C-b") #'slack-message-write-another-buffer)
    map))

(defvar slack-load-more-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-load-more-message)
    map))

(define-derived-mode slack-mode lui-mode "Slack"
  ""
  (setq-local default-directory slack-default-directory)
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-message--send))

(define-derived-mode slack-info-mode lui-mode "Slack Info"
  ""
  (setq-local default-directory slack-default-directory)
  (lui-set-prompt lui-prompt-string))

(defcustom slack-buffer-emojify nil
  "Show emoji with `emojify' if true."
  :type 'boolean
  :group 'slack)

(defcustom slack-buffer-create-on-notify nil
  "Create a room buffer when notification received if it does not yet exist"
  :type 'boolean
  :group 'slack)

(defmacro slack-buffer-widen (&rest body)
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(define-derived-mode slack-buffer-mode lui-mode "Slack Buffer"
  (setq-local default-directory slack-default-directory)
  (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
  (add-hook 'lui-pre-output-hook 'slack-add-face-lazy nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-user-name nil t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-conversation-name nil t)
  (lui-set-prompt " "))

(defclass slack-buffer ()
  ((team-id :initarg :team-id :type string)
   (buf :initarg :buf :initform nil))
  :abstract t)

(cl-defmethod slack-buffer-name ((_this slack-buffer))
  (error "Implement this"))

(cl-defmethod slack-buffer-key ((_class (subclass slack-buffer)) &rest _args)
  (error "Implement this"))

(cl-defmethod slack-buffer-key ((_this slack-buffer))
  (error "Implement this"))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-buffer)))
  (error "Implement this"))

(cl-defmethod slack-team-buffer-key ((this slack-buffer))
  (slack-team-buffer-key (eieio-object-class-name this)))

(cl-defmethod slack-buffer-find ((class (subclass slack-buffer)) team &rest args)
  (let* ((key (apply #'slack-buffer-key class args))
         (ht (or (slot-value team (slack-team-buffer-key class))
                 (make-hash-table :test #'equal))))
    (gethash key ht)))

(cl-defmethod slack-buffer-team ((this slack-buffer))
  (slack-team-find (oref this team-id)))

(cl-defmethod slack-team-set-buffer ((this slack-buffer))
  (let* ((key (slack-buffer-key this))
         (team (slack-buffer-team this))
         (ht (or (let ((ht (slot-value team (slack-team-buffer-key this))))
                   (and (hash-table-p ht) ht))
                 (make-hash-table :test #'equal))))
    (puthash key this ht)
    (setf (slot-value team (slack-team-buffer-key this))
          ht)))

(cl-defmethod slack-buffer-buffer ((this slack-buffer))
  (or (let ((buf (and (slot-boundp this 'buf)
                      (oref this buf))))
        (and (buffer-live-p buf) buf))
      (slack-buffer-init-buffer this)))

(cl-defmethod slack-buffer-set-current-buffer ((this slack-buffer))
  (setq-local slack-current-buffer this))

(cl-defmethod slack-buffer-init-buffer :before ((this slack-buffer))
  (slack-team-set-buffer this)
  (let ((buf (generate-new-buffer (slack-buffer-name this))))
    (oset this buf buf)
    buf))

(cl-defmethod slack-buffer-init-buffer ((this slack-buffer))
  (oref this buf))

(cl-defmethod slack-buffer-init-buffer :after ((this slack-buffer))
  (slack-if-let* ((buf (slack-buffer-buffer this)))
      (progn
        (with-current-buffer buf
          (slack-buffer-enable-emojify)
          (add-hook 'kill-buffer-hook (slack-buffer-create-kill-hook this) nil t))
        buf)))

(cl-defmethod slack-buffer-kill-buffer-window ((this slack-buffer))
  (let ((b (slack-buffer-buffer this)))
    (when (and b (buffer-live-p b))
      (let ((w (get-buffer-window b)))
        (when (and (window-live-p w) (< 1 (count-windows)))
          (delete-window w)))

      (kill-buffer b))))

(cl-defmethod slack-buffer-create-kill-hook ((this slack-buffer))
  #'(lambda ()
      (let* ((key (slack-buffer-key this))
             (team (slack-buffer-team this))
             (ht (slot-value team (slack-team-buffer-key this))))
        (remhash key ht))))

(defvar slack-debug nil)

(cl-defmethod slack-buffer-display ((this slack-buffer))
  (if slack-debug
      (funcall slack-buffer-function (slack-buffer-buffer this))
    (condition-case err
        (funcall slack-buffer-function (slack-buffer-buffer this))
      (error (progn
               (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
                   (kill-buffer buf))
               (ignore-errors
                 (slack-log (format "Backtrace: %S" (with-output-to-string (backtrace)))
                            (slack-buffer-team this)
                            :level 'error))
               (signal (car err) (cdr err)))))))

(cl-defmethod slack-buffer-insert ((this slack-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-time (slack-message-time-stamp message))
        (team (slack-buffer-team this)))
    (lui-insert-with-text-properties
     (slack-message-to-string message team)
     'not-tracked-p not-tracked-p
     'ts (slack-ts message)
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)
    ))

(defun slack-load-more-message ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-load-more buffer)))

(cl-defmethod slack-buffer-insert-load-more ((_this slack-buffer))
  (let ((str (propertize "(load more)\n"
                         'face '(:underline t :weight bold)
                         'keymap slack-load-more-keymap
                         'loading-message t)))
    (let ((lui-time-stamp-position nil))
      (lui-insert str t))))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-buffer))
  (next-single-property-change (point-min) 'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-buffer))
  (let ((loading-message-end
         (slack-buffer-loading-message-end-point this)))
    (delete-region (point-min) loading-message-end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-buffer))
  (set-marker lui-output-marker (point-min)))

(cl-defmethod slack-buffer-insert--history ((this slack-buffer))
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n")))

  (slack-buffer-insert-history this))

(cl-defmethod slack-buffer-load-more ((this slack-buffer))
  (if (slack-buffer-has-next-page-p this)
      (cl-labels
          ((after-success
            ()
            (with-current-buffer (slack-buffer-buffer this)
              (let ((inhibit-read-only t))
                (slack-buffer-delete-load-more-string this)
                (slack-buffer-prepare-marker-for-history this)
                (slack-buffer-insert--history this)
                (lui-recover-output-marker)))))
        (slack-buffer-request-history this #'after-success))
    (message "No more items.")))

(cl-defmethod slack-buffer-cant-execute ((this slack-buffer))
  (error "Can't execute this command from %s" (eieio-object-class-name this)))

(cl-defmethod slack-buffer-update ((this slack-buffer) _message &key _replace)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-display-pins-list ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-pins-add ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-pins-remove ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-display-user-profile ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-copy-link ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-file-upload-params ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-message-action ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-buffer) _reaction _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-send-message ((this slack-buffer) _message)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-room ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-button-block-action((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-conversation-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-channel-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-user-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-static-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-external-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-overflow-menu-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-datepicker-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer--replace ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-has-next-page-p ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-insert-history ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-request-history ((this slack-buffer) _after-success)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-select-file ((this slack-buffer))
  (slack-buffer-cant-execute this))

(defun slack-current-room-and-team ()
  (if (and (bound-and-true-p slack-current-buffer)
           (slot-exists-p slack-current-buffer 'room-id)
           (slot-boundp slack-current-buffer 'room-id)
           (slot-exists-p slack-current-buffer 'team-id)
           (slot-boundp slack-current-buffer 'team-id))
      (list (slack-buffer-room slack-current-buffer)
            (slack-buffer-team slack-current-buffer))
    (list nil nil)))

(defmacro slack-if-let-room-and-team (var-list then &rest else)
  (declare (indent 2) (debug t))
  `(cl-destructuring-bind ,var-list (slack-current-room-and-team)
     (if (and ,@var-list)
         ,then
       ,@else)))

(defun slack-buffer-replace-image (buffer ts)
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (slack-buffer--replace slack-current-buffer ts))))

(defun slack-display-image ()
  (goto-char (point-min))
  (while (re-search-forward "\\[Image\\]" (point-max) t)
    (slack-if-let* ((spec (get-text-property (1- (point)) 'slack-image-spec))
                    (end (point))
                    (beg (previous-single-property-change end 'slack-image-spec))
                    (cur-buffer (current-buffer))
                    (url (car spec))
                    (ts (get-text-property beg 'ts))
                    (path (slack-image-path url)))
        (let* ((no-token-p (get-text-property (1- (point)) 'no-token))
               (team (slack-buffer-team slack-current-buffer))
               (token (and (not no-token-p) (oref team token))))
          (cl-labels
              ((on-success ()
                           (slack-buffer-replace-image cur-buffer ts)))
            (unless (file-exists-p path)
              (slack-url-copy-file url
                                   path
                                   :success #'on-success
                                   :token token)))))))

(cl-defmethod slack-buffer-replace ((this slack-buffer) message)
  (let ((team (slack-buffer-team this)))
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (slack-message-to-string message team)
                   (lambda ()
                     (equal (get-text-property (point) 'ts)
                            (slack-ts message)))))))

(cl-defmethod slack-buffer--subscribe-cursor-event ((_this slack-buffer)
                                                    _window
                                                    _prev-point
                                                    _type))

(defun slack-reaction-echo-description ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property (point) 'reaction))
                  (team (slack-buffer-team buffer)))
      (slack-reaction-help-text reaction
                                team
                                #'(lambda (text) (message text)))))

(defun slack-buffer-subscribe-cursor-event (window prev-point type)
  (slack-if-let* ((buffer slack-current-buffer))
      (progn
        (slack-log (format "CURSOR-EVENT: BUFFER: %s, PREV-POINT: %s, POINT: %s, TYPE: %s"
                           (buffer-name (window-buffer window))
                           prev-point
                           (point)
                           type)
                   (slack-buffer-team buffer)
                   :level 'trace)

        (slack-buffer--subscribe-cursor-event buffer
                                              window
                                              prev-point
                                              type)

        (when (eq type 'entered)
          (add-hook 'post-command-hook 'slack-reaction-echo-description t t))
        (when (eq type 'left)
          (remove-hook 'post-command-hook 'slack-reaction-echo-description t)))))

(defun slack-buffer-enable-emojify ()
  (if slack-buffer-emojify
      (let ((emojify (require 'emojify nil t)))
        (unless emojify
          (error "Emojify is not installed"))
        (emojify-mode t))))

(defun slack-buffer-goto (ts)
  (let ((point (slack-buffer-ts-eq (point-min) (point-max) ts)))
    (when point
      (goto-char point))))

(defun slack-buffer-get-props-range (cur-point prop-name)
  (let* ((start (or (and (get-text-property cur-point prop-name) cur-point)
                    (next-single-property-change cur-point prop-name)))
         (end (and start (next-single-property-change start prop-name))))
    (list start end)))

(defun slack-handle-lazy-conversation-name ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (progn
        (let ((cur-point (point-min)))
          (while (and cur-point (< cur-point (point-max)))
            (cl-destructuring-bind (start end)
                (slack-buffer-get-props-range cur-point
                                              'slack-lazy-conversation-name)
              (when (and start end)
                (slack-if-let* ((room-id (get-text-property start 'slack-conversation-id))
                                (room (slack-room-find room-id team))
                                (name (format "%s%s"
                                              (if (slack-im-p room) "@" "#")
                                              (slack-room-name room team))))
                    (put-text-property start end 'display name)
                  (put-text-property start end 'display "Unknown Conversation")))
              (setq cur-point end)))))))

(defun slack-handle-lazy-user-name ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (progn
        (let ((cur-point (point-min)))
          (while (and cur-point (< cur-point (point-max)))
            (cl-destructuring-bind (start end)
                (slack-buffer-get-props-range cur-point
                                              'slack-lazy-user-name)
              (when (and start end)
                (slack-if-let* ((user-id (get-text-property start 'slack-user-id))
                                (user-name (propertize
                                            (or (slack-user-name user-id team)
                                                "Unknown User"))))
                    (put-text-property start end 'display user-name)))
              (setq cur-point end)))))))

(defun slack-add-face-lazy ()
  (let ((cur-point (point-min)))
    (while (and cur-point (< cur-point (point-max)))
      (let* ((start (or (and (get-text-property cur-point 'slack-defer-face) cur-point)
                        (next-single-property-change cur-point 'slack-defer-face)))
             (end (and start (next-single-property-change start 'slack-defer-face))))
        (when (and start end)
          (let ((face-or-func (get-text-property start 'slack-defer-face)))
            (if (functionp face-or-func)
                (funcall face-or-func start end)
              (add-text-properties start end
                                   (list 'face face-or-func)))))
        (setq cur-point end)))))

(defun slack-buffer-buttonize-link ()
  (let ((regex "\\(<\\)\\(http://\\|https://\\)\\(.*?\\)\\(?:|\\([[:ascii:][:nonascii:]]*?\\)\\)?\\(\\)>"))
    (ignore-errors
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let* ((url-begin (match-beginning 2))
               (cur-point (point))
               (disabled (get-text-property cur-point 'slack-disable-buttonize))
               (url (concat (match-string 2) (match-string 3)))
               (replace (or (match-string 4) url)))
          (replace-match replace nil)
          (unless disabled
            (make-button (1- url-begin)
                         (+ (1- url-begin) (length replace))
                         'type 'lui-button
                         'action 'lui-button-activate
                         'lui-button-function 'browse-url
                         'lui-button-arguments (list url))))))))

(defun slack-buffer-show-typing-p (buffer)
  (cl-case slack-typing-visibility
    ('frame (slack-buffer-in-current-frame buffer))
    ('buffer (slack-buffer-current-p buffer))
    ('never nil)))

(defun slack-buffer-current-p (buffer)
  (if buffer
      (string= (buffer-name buffer)
               (buffer-name (current-buffer)))))

(defun slack-buffer-in-current-frame (buffer)
  (if buffer
      (cl-member (buffer-name buffer)
                 (mapcar #'buffer-name
                         (mapcar #'window-buffer (window-list)))
                 :test #'string=)))

(defmacro slack-buffer-goto-char (find-point &rest else)
  (let ((ts (car else))
        (else (cdr else)))
    `(let* ((cur-point (point))
            (ts (or (get-text-property cur-point 'ts) ,ts)))
       (let ((next-point ,find-point))
         (if next-point
             (goto-char next-point)
           (if (< 0 (length ',else))
               ,@else))))))

(defun slack-buffer-goto-next-message ()
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-next-point cur-point (point-max) ts)
   "0"
   (message "You are on Last Message.")))

(defun slack-buffer-goto-prev-message ()
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-prev-point cur-point (point-min) ts)
   "z"
   (message "You are on First Message.")))

(defun slack-buffer-goto-first-message ()
  (interactive)
  (goto-char
   (slack-buffer-next-point (point-min) (point-max) "0")))

(defun slack-buffer-goto-last-message ()
  (interactive)
  (goto-char
   (slack-buffer-prev-point (point-max) (point-min) (format-time-string "%s"))))

(defun slack-buffer-next-point (start end ts)
  (cl-loop for i from start to end
           for next-ts = (get-text-property i 'ts)
           if (and next-ts (string< ts next-ts))
           return i))

(defun slack-buffer-prev-point (start end ts)
  (cl-loop for i from start downto end
           for prev-ts = (get-text-property i 'ts)
           if (and prev-ts (string< prev-ts ts))
           return i))

(defun slack-buffer-ts-eq (start end ts)
  (when (and start end)
    (if (<= start end)
        (cl-loop for i from start to end
                 if (string= (get-text-property i 'ts)
                             ts)
                 return i)
      (cl-loop for i from start downto end
               if (string= (get-text-property i 'ts)
                           ts)
               return i))))

(defun slack-group-rename ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-rename room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-group-names team #'(lambda (groups)
                                                  (cl-remove-if #'slack-room-archived-p
                                                                groups)))
                      "Select Channel: "))))
      (slack-conversations-rename room team))))

(defun slack-group-invite ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-invite room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-group-names team
                                         #'(lambda (rooms)
                                             (cl-remove-if #'slack-room-archived-p
                                                           rooms)))
                      "Select Channel: "))))
      (slack-conversations-invite room team))))

(defun slack-group-leave ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-leave room team)
    (let* ((team (slack-team-select))
           (group (slack-select-from-list
                      ((slack-group-names team)
                       "Select Channel: "))))
      (slack-conversations-leave group team))))

(defun slack-group-archive ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-archive room team)
    (let* ((team (slack-team-select))
           (group (slack-select-from-list
                      ((slack-group-names
                        team
                        #'(lambda (groups)
                            (cl-remove-if #'slack-room-archived-p
                                          groups)))
                       "Select Channel: "))))
      (slack-conversations-archive group team))))

(defun slack-group-unarchive ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-unarchive room team)
    (let* ((team (slack-team-select))
           (group (slack-select-from-list
                      ((slack-group-names
                        team
                        #'(lambda (groups)
                            (cl-remove-if-not #'slack-room-archived-p
                                              groups)))
                       "Select Channel: "))))
      (slack-conversations-unarchive group team))))

(defun slack-group-mpim-close ()
  "Close mpim."
  (interactive)
  (cl-labels
      ((on-success (room team)
                   #'(lambda (data)
                       (when (and (eq t (plist-get data :no_op)))
                         (oset room is-member nil)
                         (slack-team-set-room team room)
                         (slack-log (format "%s closed"
                                            (slack-room-name room team))
                                    team :level 'info)))))
    (slack-if-let-room-and-team (room team)
        (slack-conversations-close room
                                   team
                                   (on-success room team))
      (let* ((team (slack-team-select))
             (mpim (slack-select-from-list
                       ((slack-group-names
                         team
                         #'(lambda (groups)
                             (cl-remove-if-not #'(lambda (e) (and (slack-mpim-p e)
                                                                  (oref e is-member)))
                                               groups)))
                        "Select Channel: "))))
        (slack-conversations-close mpim team (on-success mpim team))))))

(defun slack-im-close ()
  "Close direct message."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-close room team)
    (let* ((team (slack-team-select))
           (im (slack-select-from-list
                   ((slack-im-names team)
                    "Select Channel: "))))
      (slack-conversations-close im team))))

(defun slack-channel-rename ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-rename room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-channel-names team #'(lambda (channels)
                                                    (cl-remove-if #'slack-room-member-p
                                                                  channels)))
                      "Select Channel: "))))
      (slack-conversations-rename room team))))

(defun slack-channel-invite ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-invite room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-channel-names team
                                           #'(lambda (rooms)
                                               (cl-remove-if #'slack-room-archived-p
                                                             rooms)))
                      "Select Channel: "))))
      (slack-conversations-invite room team))))

(defun slack-channel-leave (&optional team)
  (interactive)
  (slack-if-let-room-and-team (cur-room cur-team)
      (slack-conversations-leave cur-room cur-team)
    (let* ((team (or team (slack-team-select)))
           (channel (slack-select-from-list ((slack-channel-names
                                              team
                                              #'(lambda (channels)
                                                  (cl-remove-if-not
                                                   #'slack-room-member-p
                                                   channels)))
                                             "Select Channel: "))))
      (slack-conversations-leave channel team))))

(defun slack-channel-join (&optional team)
  (interactive)
  (slack-if-let-room-and-team (cur-room cur-team)
      (slack-conversations-join cur-room cur-team)
    (cl-labels
        ((filter-channel (channels)
                         (cl-remove-if
                          #'(lambda (c)
                              (or (slack-room-member-p c)
                                  (slack-room-archived-p c)))
                          channels)))
      (let* ((team (or team (slack-team-select)))
             (channel (slack-select-from-list
                          ((slack-channel-names team
                                                #'filter-channel)
                           "Select Channel: "))))
        (slack-conversations-join channel team)))))

(defun slack-channel-archive ()
  "Archive selected channel."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-archive room team)
    (let* ((team (slack-team-select))
           (channel (slack-select-from-list
                        ((slack-channel-names
                          team
                          #'(lambda (channels)
                              (cl-remove-if #'slack-room-archived-p
                                            channels)))
                         "Select Channel: "))))
      (slack-conversations-archive channel team))))

(defun slack-channel-unarchive ()
  "Unarchive selected channel."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-unarchive room team)
    (let* ((team (slack-team-select))
           (channel (slack-select-from-list
                        ((slack-channel-names
                          team
                          #'(lambda (channels)
                              (cl-remove-if-not #'slack-room-archived-p
                                                channels)))
                         "Select Channel: "))))
      (slack-conversations-unarchive channel team))))

(defun slack-file-upload (file filetype filename)
  "Uploads FILE with FILETYPE and FILENAME."
  (interactive
   (let ((file (expand-file-name (car (find-file-read-args "Select File: " t)))))
     (list file
           (slack-file-select-filetype (file-name-extension file))
           (read-from-minibuffer "Filename: " (file-name-nondirectory file)))))

  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (slack-buffer-team buffer))
       (initial-comment (read-from-minibuffer "Message: ")))
      (cl-labels
          ((on-file-upload (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-file-upload"))))

        (slack-request
         (slack-request-create
          slack-file-upload-url
          team
          :type "POST"
          :params (append (slack-file-upload-params buffer)
                          (list
                           (cons "filename" filename)
                           (cons "filetype" filetype)
                           (if initial-comment
                               (cons "initial_comment" initial-comment))))
          :files (list (cons "file" file))
          :headers (list (cons "Content-Type" "multipart/form-data"))
          :success #'on-file-upload)))
    (error "Call from message buffer or thread buffer")))

(defun slack-clipboard-image-upload ()
  "Uploads png image from clipboard."
  (interactive)

  (let* ((file (make-temp-file "clip" nil ".png"))
         (selection-coding-system 'no-conversion)
         (coding-system-for-write 'binary))

    (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD"))
                  nil file nil 'quiet)

    (slack-file-upload file "png" "image.png")))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
