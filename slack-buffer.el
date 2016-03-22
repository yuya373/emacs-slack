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

(defvar lui-prompt-string "> ")

(defvar slack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s C-r") #'slack-room-update-messages)
    (define-key map (kbd "C-s C-b") #'slack-message-write-another-buffer)
    map))

(define-derived-mode slack-mode lui-mode "Slack"
  ""
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-message--send))

(define-derived-mode slack-info-mode lui-mode "Slack Info"
  ""
  (lui-set-prompt lui-prompt-string))

(defvar slack-current-room-id)
(defvar slack-current-team-id)
(defvar slack-current-message nil)
(defcustom slack-buffer-emojify nil
  "Show emoji with `emojify' if true."
  :group 'slack)

(defun slack-get-buffer-create (buf-name)
  (let ((buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (slack-mode)
        (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
        (add-hook 'lui-pre-output-hook 'slack-buffer-add-last-ts-property nil t)
        (add-hook 'lui-post-output-hook 'slack-buffer-add-ts-property nil t)))
    buffer))

(defun slack-buffer-set-current-room-id (room)
  (set (make-local-variable 'slack-current-room-id) (oref room id)))

(defun slack-buffer-set-current-team-id (team)
  (set (make-local-variable 'slack-current-team-id) (oref team id)))

(defun slack-buffer-enable-emojify ()
  (if slack-buffer-emojify
      (let ((emojify (require 'emojify nil t)))
        (unless emojify
          (error "Emojify is not installed"))
        (emojify-mode t))))

(defun slack-buffer-insert-previous-link (oldest-msg)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert (concat (propertize "(load more message)"
                                  'face '(:underline t)
                                  'oldest (oref oldest-msg ts)
                                  'keymap (let ((map (make-sparse-keymap)))
                                            (define-key map (kbd "RET")
                                              #'slack-room-load-prev-messages)
                                            map))
                    "\n\n"))
    (set-marker lui-output-marker (point))))

(cl-defun slack-buffer-create (room team
                                    &optional
                                    (insert-func
                                     #'slack-buffer-insert-messages)
                                    (type 'message))
  (cl-labels
      ((get-buffer (type buf-name)
                   (cl-ecase type
                     (message (slack-get-buffer-create buf-name))
                     (info (slack-get-info-buffer-create buf-name)))))
    (let* ((buf-name (slack-room-buffer-name room))
           (buffer (get-buffer type buf-name)))
      (with-current-buffer buffer
        (if insert-func
            (funcall insert-func room team))
        (slack-buffer-set-current-room-id room)
        (slack-buffer-set-current-team-id team)
        (slack-buffer-enable-emojify))
      buffer)))

(defun slack-buffer-add-last-ts-property ()
  (when slack-current-message
    (add-text-properties
     (point-min) (point-max)
     `(slack-last-ts ,lui-time-stamp-last))))

(defun slack-buffer-add-ts-property ()
  (when slack-current-message
    (add-text-properties
     (point-min) (point-max)
     `(ts ,(oref slack-current-message ts)))))

(defun slack-buffer-insert (message team &optional not-tracked-p)
  (let ((lui-time-stamp-time (slack-message-time-stamp message))
        (beg lui-input-marker)
        (inhibit-read-only t))
    (let ((slack-current-message message))
      (lui-insert (slack-message-to-string message team) not-tracked-p))))

(defun slack-buffer-insert-messages (room team)
  (let* ((sorted (slack-room-sorted-messages room))
         (messages (nreverse
                    (slack-room-latest-messages room sorted))))
    (if messages
        (progn
          (slack-buffer-insert-previous-link (cl-first messages))
          (mapc (lambda (m)
                  (slack-buffer-insert m team t))
                messages)
          (let ((latest-message (car (last messages))))
            (slack-room-update-last-read room latest-message)
            (slack-room-update-mark room team latest-message)))
      (unless (eq 0 (oref room unread-count-display))
        (let ((latest-message (car sorted)))
          (slack-room-update-mark room team latest-message))))))


(cl-defun slack-buffer-update (room msg &key replace team)
  (cl-labels ((do-update (buf room msg)
                         (with-current-buffer buf
                           (slack-room-update-last-read room msg)
                           (slack-buffer-insert msg team))))
    (let* ((buf-name (slack-room-buffer-name room))
           (buffer (get-buffer buf-name))
           (win-buf-names (mapcar #'buffer-name (mapcar #'window-buffer
                                                        (window-list)))))
      (if (cl-member buf-name win-buf-names :test #'string=)
          (slack-room-update-mark room team msg)
        (cl-incf (oref room unread-count-display)))
      (if buffer
          (if replace (slack-buffer-replace buffer msg)
            (do-update buffer room msg))))))

(defun slack-buffer-ts-eq (start end ts)
  (cl-loop for i from start to end
           if (string= (get-text-property i 'ts)
                       ts)
           return i))

(defun slack-buffer-ts-not-eq (start end ts)
  (cl-loop for i from start to end
           if (not (string= (get-text-property i 'ts)
                            ts))
           return i))

(defun slack-buffer-replace (buffer msg)
  (with-current-buffer buffer
    (let* ((cur-point (point))
           (ts (oref msg ts))
           (beg (slack-buffer-ts-eq (point-min) (point-max) ts))
           (end (slack-buffer-ts-not-eq beg (point-max) ts))
           (lui-time-stamp-last (get-text-property beg 'slack-last-ts)))
      (if (and beg end)
          (let ((inhibit-read-only t))
            (delete-region beg end)
            (set-marker lui-output-marker beg)
            (slack-buffer-insert msg
                                 (slack-team-find slack-current-team-id))
            (goto-char cur-point)
            (slack-buffer-recover-lui-output-marker))))))

(defun slack-buffer-recover-lui-output-marker ()
  (set-marker lui-output-marker (- (marker-position
                                    lui-input-marker)
                                   (length lui-prompt-string))))

(defun slack-buffer-update-notification (buf-name string)
  (let ((buffer (slack-get-buffer-create buf-name)))
    (with-current-buffer buffer
      (lui-insert string))))

(defun slack-get-info-buffer-create (buf-name)
  (let ((buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (slack-info-mode)))
    buffer))

(defun slack-buffer-create-info (buf-name insert-func)
  (let ((buf (slack-get-info-buffer-create buf-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (funcall insert-func)
      (goto-char (point-max))
      (setq buffer-read-only t)
      (slack-buffer-enable-emojify))
    buf))

(defun slack-reset-room-last-read ()
  (let ((room (slack-room-find slack-current-room-id
                               (slack-team-find slack-current-team-id))))
    (slack-room-update-last-read room
                                 (slack-message "msg" :ts "0"))))

(provide 'slack-buffer)
;;; slack-buffer.el ends here
