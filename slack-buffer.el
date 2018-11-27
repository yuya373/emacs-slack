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
(declare-function emojify-mode "emojify")

(defvar slack-buffer-function)
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
  (lui-set-prompt " "))

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

(cl-defmethod slack-buffer-find ((class (subclass slack-buffer)) room team)
  (slack-if-let* ((buf (cl-find-if
                        #'(lambda (buf)
                            (string= (buffer-name buf)
                                     (slack-buffer-name class room team)))
                        (slot-value team class))))
      (with-current-buffer buf slack-current-buffer)))

(cl-defmethod slack-buffer-buffer ((this slack-buffer))
  (or (get-buffer (slack-buffer-name this))
      (slack-buffer-init-buffer this)))

(cl-defmethod slack-buffer-display ((this slack-buffer))
  (condition-case err
      (funcall slack-buffer-function (slack-buffer-buffer this))
    (error (progn
             (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
                 (kill-buffer buf))
             (signal (car err) (cdr err))))))

(cl-defmethod slack-buffer-name ((_this slack-buffer))
  "*Slack*")

(defun slack-message-buffer-on-killed ()
  (slack-if-let* ((buf slack-current-buffer)
                  (class (eieio-object-class-name buf))
                  (cb (current-buffer)))
      (setf (slot-value (oref buf team) class)
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
                    (path (slack-image-path url)))
        (let* ((no-token-p (get-text-property (1- (point)) 'no-token))
               (token (and (not no-token-p)
                           (oref (oref slack-current-buffer team)
                                 token))))
          (cl-labels
              ((on-success ()
                           (slack-buffer-replace-image cur-buffer ts)))
            (unless (file-exists-p path)
              (slack-url-copy-file url
                                   path
                                   :success #'on-success
                                   :token token)))))))

(cl-defmethod slack-buffer-init-buffer :after (this)
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (with-current-buffer buf
          (slack-buffer-enable-emojify)
          (add-hook 'kill-buffer-hook 'slack-message-buffer-on-killed nil t))
        buf)))

(cl-defmethod slack-buffer-set-current-buffer ((this slack-buffer))
  (setq-local slack-current-buffer this))


(cl-defmethod slack-buffer-init-buffer ((this slack-buffer))
  (generate-new-buffer (slack-buffer-name this)))

(cl-defmethod slack-buffer-replace ((this slack-buffer) message)
  (with-slots (team) this
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
                  (team (oref buffer team)))
      (message (slack-reaction-help-text reaction team))))

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

        (slack-buffer--subscribe-cursor-event buffer
                                              window
                                              prev-point
                                              type)

        (when (eq type 'entered)
          (add-hook 'post-command-hook 'slack-reaction-echo-description t t))
        (when (eq type 'left)
          (remove-hook 'post-command-hook 'slack-reaction-echo-description t)))))

(cl-defmethod slack-buffer-insert ((this slack-buffer) message &optional not-tracked-p)
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
  (with-slots (team) this
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
      (message "No more items."))))

(defun slack-buffer-find-4 (class a b team)
  (slack-if-let* ((buf (cl-find-if #'(lambda (buf)
                                       (string= (buffer-name buf)
                                                (slack-buffer-name class a b team)))
                                   (slot-value team class))))
      (with-current-buffer buf slack-current-buffer)))

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
(cl-defmethod slack-buffer-display-unread-threads ((this slack-buffer))
  (slack-buffer-cant-execute this))

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
            (put-text-property beg end 'slack-disable-buttonize t)
            (goto-char end))))))

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
  (let ((regex "<\\(http://\\|https://\\)\\(.*?\\)|\\([[:ascii:][:nonascii:]]*?\\)>"))
    (ignore-errors
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let* ((url-begin (match-beginning 1))
               (cur-point (point))
               (disabled (get-text-property cur-point 'slack-disable-buttonize))
               (replace (match-string 3)))
          (if disabled
              (replace-match replace nil)
            (let ((url (concat (match-string 1) (match-string 2))))
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

(cl-defmethod slack-buffer--replace ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-has-next-page-p ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-insert-history ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-request-history ((this slack-buffer) _after-success)
  (slack-buffer-cant-execute this))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
