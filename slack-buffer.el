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

(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-room)

(define-derived-mode slack-buffer-mode lui-mode "Slack Buffer"
  (setq-local default-directory slack-default-directory)
  (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
  (add-hook 'lui-pre-output-hook 'slack-add-face-lazy nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (lui-set-prompt " "))

(defvar-local slack-current-buffer nil)

(defclass slack-buffer ()
  ((team :initarg :team :type slack-team)))

(defun slack-buffer-push-new-3 (class a team)
  (let ((buf (get-buffer (slack-buffer-name class a team))))
    (unless (slack-buffer-find class a team)
      (push buf
            (slot-value team class)))
    buf))

(defun slack-buffer-push-new-4 (class a b team)
  (let ((buf (get-buffer (slack-buffer-name class a b team))))
    (unless (slack-buffer-find class a b team)
      (push buf (slot-value team class)))
    buf))

(defmethod slack-buffer-find :static ((class slack-buffer) room team)
  (slack-if-let* ((buf (cl-find-if
                  #'(lambda (buf)
                      (string= (buffer-name buf)
                               (slack-buffer-name class room team)))
                  (slot-value team class))))
      (with-current-buffer buf slack-current-buffer)))

(defmethod slack-buffer-buffer ((this slack-buffer))
  (or (get-buffer (slack-buffer-name this))
      (slack-buffer-init-buffer this)))

(defmethod slack-buffer-display ((this slack-buffer))
  (condition-case err
      (funcall slack-buffer-function (slack-buffer-buffer this))
    (error (progn
             (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
                 (kill-buffer buf))
             (signal (car err) (cdr err))))))

(defmethod slack-buffer-name ((this slack-buffer))
  "*Slack*")

(defun slack-message-buffer-on-killed ()
  (slack-if-let* ((buf slack-current-buffer)
            (class (eieio-object-class-name buf))
            (cb (current-buffer)))
      (set-slot-value (oref buf team) class
                      (cl-remove-if #'(lambda (e) (equal e cb))
                                    (slot-value (oref buf team) class)))))

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
                    (path (slack-image-path url))
                    (token (oref (oref slack-current-buffer team) token)))
        (cl-labels
            ((on-success ()
                         (slack-buffer-replace-image cur-buffer ts)))
          (unless (file-exists-p path)
            (slack-url-copy-file url
                                 path
                                 :success #'on-success
                                 :token token))))))

(defmethod slack-buffer-init-buffer :after (this)
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (with-current-buffer buf
          (slack-buffer-enable-emojify)
          (add-hook 'kill-buffer-hook 'slack-message-buffer-on-killed nil t))
        buf)))

(defmethod slack-buffer-set-current-buffer ((this slack-buffer))
  (setq-local slack-current-buffer this))


(defmethod slack-buffer-init-buffer ((this slack-buffer))
  (generate-new-buffer (slack-buffer-name this)))

(defmethod slack-buffer-replace ((this slack-buffer) message)
  (with-slots (team) this
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (slack-message-to-string message team)
                   (lambda ()
                     (equal (get-text-property (point) 'ts)
                            (oref message ts)))))))

(defun slack-buffer-subscribe-cursor-event (window prev-point type)
  (slack-if-let* ((buffer slack-current-buffer))
      (progn
        (slack-log (format "CURSOR-EVENT: BUFFER: %s, PREV-POINT: %s, POINT: %s, TYPE: %s"
                           (buffer-name (window-buffer window))
                           prev-point
                           (point)
                           type)
                   (oref buffer team)
                   :level 'trace)

        (when (eq type 'entered)
          (unless (slack-team-mark-as-read-immediatelyp (oref buffer team))
            (slack-buffer-update-mark buffer))
          (add-hook 'post-command-hook 'slack-reaction-echo-description t t))

        (when (eq type 'left)
          (remove-hook 'post-command-hook 'slack-reaction-echo-description t)))))

(defmethod slack-buffer-insert ((this slack-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-time (slack-message-time-stamp message))
        (team (oref this team)))
    (lui-insert-with-text-properties
     (slack-message-to-string message team)
     'not-tracked-p not-tracked-p
     'ts (slack-ts message)
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)
    ))

(defmethod slack-buffer-insert-load-more ((this slack-buffer))
  (let ((str (propertize "(load more)\n"
                         'face '(:underline t :weight bold)
                         'keymap (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "RET")
                                     #'(lambda ()
                                         (interactive)
                                         (slack-buffer-load-more this)))
                                   map)
                         'loading-message t)))
    (let ((lui-time-stamp-position nil))
      (lui-insert str))))

(defmethod slack-buffer-loading-message-end-point ((this slack-buffer))
  (next-single-property-change (point-min) 'loading-message))

(defmethod slack-buffer-delete-load-more-string ((this slack-buffer))
  (let ((loading-message-end
         (slack-buffer-loading-message-end-point this)))
    (delete-region (point-min) loading-message-end)))

(defmethod slack-buffer-prepare-marker-for-history ((_this slack-buffer))
  (set-marker lui-output-marker (point-min)))

(defmethod slack-buffer-insert--history ((this slack-buffer))
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n")))

  (slack-buffer-insert-history this))

(defmethod slack-buffer-load-more ((this slack-buffer))
  (with-slots (team) this
    (let ((cur-point (point)))
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
        (message "No more items.")))))

(defmethod slack-buffer-display-file ((this slack-buffer) file-id)
  (with-slots (team) this
    (cl-labels
        ((open (file _)
               (slack-buffer-display
                (slack-create-file-info-buffer team file))))
      (slack-file-request-info file-id 1 team #'open))))

(defmethod slack-buffer-cant-execute ((this slack-buffer))
  (message "Can't execute this command from %s" (eieio-object-class-name this)))

(defmethod slack-buffer-display-pins-list ((this slack-buffer))
  (slack-buffer-cant-execute this))
(defmethod slack-buffer-pins-add ((this slack-buffer) ts)
  (slack-buffer-cant-execute this))
(defmethod slack-buffer-pins-remove ((this slack-buffer) ts)
  (slack-buffer-cant-execute this))
(defmethod slack-buffer-display-user-profile ((this slack-buffer))
  (slack-buffer-cant-execute this))
(defmethod slack-buffer-copy-link ((this slack-buffer) ts)
  (slack-buffer-cant-execute this))

(defvar lui-prompt-string "> ")

(defvar slack-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-s C-r") #'slack-room-update-messages)
    ;; (define-key map (kbd "C-s C-b") #'slack-message-write-another-buffer)
    map))

(defcustom slack-default-directory
  (expand-file-name (concat (or (getenv "HOME") "~") "/"))
  "default directory at Slack Buffer.")

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
  :group 'slack)

(defcustom slack-buffer-create-on-notify nil
  "Create a room buffer when notification received if it does not yet exist"
  :group 'slack)

(defmacro slack-buffer-widen (&rest body)
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

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

(defface slack-preview-face
  (let* ((default-bg (or (face-background 'default) "unspecified-bg"))
         (light-bg (if (equal default-bg "unspecified-bg")
                       "unspecified-bg"
                     (color-darken-name default-bg 3)))
         (dark-bg (if (equal default-bg "unspecified-bg")
                      "unspecified-bg"
                    (color-lighten-name default-bg 3))))
    `((default :inherit (fixed-pitch shadow) :slant normal :weight normal)
      (((type graphic) (class color) (background dark)) (:background ,dark-bg))
      (((type graphic) (class color) (background light)) (:background ,light-bg))))
  "Used preview text and code blocks"
  :group 'slack)

(defun slack-put-preview-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'slack-preview-face)))

(defalias 'slack-put-email-body-overlay 'slack-put-preview-overlay)
(defalias 'slack-put-code-block-overlay 'slack-put-preview-overlay)

(defun slack-search-code-block ()
  (while (re-search-forward "`\\([^`]\\|\n\\)" (point-max) t)
    (let* ((block-begin (- (point) 4))
           (block-p (and (<= (point-min) block-begin)
                         (string= (buffer-substring-no-properties block-begin
                                                                  (+ block-begin 3))
                                  "```")))
           (beg (or (and block-p block-begin) (- (point) 2)))
           (end-regex (or (and block-p "```") "[^`]`[^`]")))

      (goto-char (+ beg (or (and block-p 3) 1)))

      (if (re-search-forward end-regex (point-max) t)
          (let* ((end (or (and block-p (1+ (point))) (- (point) 1))))
            (slack-put-code-block-overlay beg end)
            (goto-char end))))))


(defun slack-add-face-lazy ()
  (let* ((start (or (get-text-property (point-min) 'slack-defer-face)
                    (next-single-property-change (point-min) 'slack-defer-face)))
         (end (and start (next-single-property-change start 'slack-defer-face))))
    (when (and start end)
      (let ((face-or-func (get-text-property start 'slack-defer-face)))
        (if (functionp face-or-func)
            (funcall face-or-func start end)
          (add-text-properties start end (list 'face face)))))))

(defun slack-buffer-buttonize-link ()
  (let ((regex "<\\(http://\\|https://\\)\\(.*?\\)|\\([[:ascii:][:nonascii:]]*?\\)>"))
    (ignore-errors
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let* ((url-begin (match-beginning 1))
               (cur-point (point))
               (disabled (get-text-property cur-point 'slack-disable-buttonize)))
          (unless disabled
            (let ((url (concat (match-string 1) (match-string 2)))
                  (replace (match-string 3)))
              (replace-match replace nil)
              (make-button (1- url-begin)
                           (+ (1- url-begin) (length replace))
                           'type 'lui-button
                           'action 'lui-button-activate
                           'lui-button-function 'browse-url
                           'lui-button-arguments (list url)))))))))

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
  `(let* ((cur-point (point))
          (ts (get-text-property cur-point 'ts)))
     (let ((next-point ,find-point))
       (if next-point
           (goto-char next-point)
         (if (< 0 (length ',else))
             ,@else)))))

(defun slack-buffer-goto-next-message ()
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-next-point cur-point (point-max) ts)
   (message "You are on Last Message.")))

(defun slack-buffer-goto-prev-message ()
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-prev-point cur-point (point-min) ts)
   (message "You are on First Message.")))

(defun slack-buffer-goto-first-message ()
  (interactive)
  (goto-char
   (slack-buffer-next-point (point-min) (point-max) "0")))

(defun slack-buffer-goto-last-message ()
  (interactive)
  (goto-char
   (slack-buffer-prev-point (point-max) (point-min) (format-time-string "%s"))))

(defun slack-buffer-header-p (point)
  (let ((face (get-text-property point 'face)))
    (string= (format "%s" face) "slack-message-output-header")))

(defun slack-buffer-next-point (start end ts)
  (cl-loop for i from start to end
           if (and (string< ts
                            (get-text-property i 'ts))
                   (slack-buffer-header-p i))
           return i))

(defun slack-buffer-prev-point (start end ts)
  (cl-loop for i from start downto end
           if (and (string< (get-text-property i 'ts)
                            ts)
                   (slack-buffer-header-p i))
           return i))

(defun slack-buffer-ts-eq (start end ts)
  (if (and start end)
      (cl-loop for i from start to end
               if (string= (get-text-property i 'ts)
                           ts)
               return i)))

(defun slack--get-channel-id ()
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-if-let* ((buffer slack-current-buffer)
                    (boundp (slot-boundp buffer 'room))
                    (room (oref buffer room)))
        (progn
          (kill-new (oref room id))
          (message "%s" (oref room id))))))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
