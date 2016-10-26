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
(require 'slack-room)

(defvar lui-prompt-string "> ")

(defvar slack-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-s C-r") #'slack-room-update-messages)
    ;; (define-key map (kbd "C-s C-b") #'slack-message-write-another-buffer)
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

(defmacro slack-buffer-widen (&rest body)
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defun slack-get-buffer-create (room)
  (let* ((buf-name (slack-room-buffer-name room))
         (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (slack-mode)
        (slack-buffer-insert-previous-link room)
        (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
        (add-hook 'lui-pre-output-hook 'slack-buffer-add-last-ts-property nil t)
        (add-hook 'lui-post-output-hook 'slack-buffer-add-ts-property nil t)
        (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)))
    buffer))

(defmethod slack-buffer-set-current-room-id ((room slack-room))
  (set (make-local-variable 'slack-current-room-id) (oref room id)))

(defun slack-buffer-set-current-team-id (team)
  (set (make-local-variable 'slack-current-team-id) (oref team id)))

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

(defmethod slack-buffer-insert-previous-link ((room slack-room))
  (let ((oldest (slack-room-prev-link-info room)))
    (if oldest
        (slack-buffer-widen
         (let ((inhibit-read-only t))
           (goto-char (point-min))
           (insert
            (concat
             (propertize "(load more message)"
                         'face '(:underline t)
                         'oldest oldest
                         'keymap (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "RET")
                                     #'slack-room-load-prev-messages)
                                   map))
             "\n\n"))
           (set-marker lui-output-marker (point)))))))

(defmethod slack-buffer-insert-prev-messages ((room slack-room) team oldest-ts)
  (slack-buffer-widen
   (let ((messages (slack-room-prev-messages room oldest-ts)))
     (if messages
         (progn
           (slack-buffer-insert-previous-link room)
           (cl-loop for m in messages
                    do (slack-buffer-insert m team t)))
       (set-marker lui-output-marker (point-min))
       (lui-insert "(no more messages)\n"))
     (slack-buffer-recover-lui-output-marker))))

(cl-defun slack-buffer-create (room team
                                    &key
                                    (insert-func
                                     #'slack-buffer-insert-messages)
                                    (type 'message))
  (cl-labels
      ((get-buffer (type room)
                   (cl-ecase type
                     (message (slack-get-buffer-create room))
                     (info (slack-get-info-buffer-create room)))))
    (let* ((buffer (get-buffer type room)))
      (with-current-buffer buffer
        (if insert-func
            (funcall insert-func room team))
        (slack-buffer-set-current-room-id room)
        (slack-buffer-set-current-team-id team)
        (slack-buffer-enable-emojify))
      buffer)))

(defun slack-buffer-buttonize-link ()
  (let ((regex "<\\(http://\\|https://\\)\\(.*?\\)|\\(.*?\\)>"))
    (while (re-search-forward regex nil t)
      (let ((url-begin (match-beginning 1))
            (url (concat (match-string 1) (match-string 2)))
            (replace (match-string 3)))
        (replace-match replace nil)

        (make-button (1- url-begin)
                     (+ (1- url-begin) (length replace))
                     'type 'lui-button
                     'action 'lui-button-activate
                     'lui-button-function 'browse-url
                     'lui-button-arguments (list url))))))

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
         (messages (slack-room-latest-messages room sorted)))
    (if messages
        (progn
          ;; (slack-buffer-insert-previous-link room)
          (cl-loop for m in messages
                   do (slack-buffer-insert m team t))
          (let ((latest-message (car (last messages))))
            (slack-room-update-last-read room latest-message)
            (slack-room-update-mark room team latest-message)))
      (unless (eq 0 (oref room unread-count-display))
        (let ((latest-message (car (last sorted))))
          (slack-room-update-mark room team latest-message))))))

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

(cl-defun slack-buffer-update (room msg team &key replace)
  (let* ((buf-name (slack-room-buffer-name room))
         (buffer (get-buffer buf-name)))
    (if buffer
        (progn
          (if (slack-buffer-in-current-frame buffer)
              (slack-room-update-mark room team msg)
            (slack-room-inc-unread-count room))
          (if replace
              (slack-buffer-replace buffer msg)
            (with-current-buffer buffer
              (slack-room-update-last-read room msg)
              (slack-buffer-insert msg team))))
      (slack-room-inc-unread-count room))))

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
   (slack-buffer-goto-first-message)))

(defun slack-buffer-goto-prev-message ()
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-prev-point cur-point (point-min) ts)
   (slack-buffer-goto-last-message)))

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

(defun slack-buffer-ts-not-eq (start end ts)
  (if (and start end)
      (cl-loop for i from start to end
               if (not (string= (get-text-property i 'ts)
                                ts))
               return i)))

(defun slack-buffer-replace (buffer msg)
  (with-current-buffer buffer
    (slack-buffer-widen
     (let* ((cur-point (point))
            (ts (oref msg ts))
            (beg (slack-buffer-ts-eq (point-min) (point-max) ts))
            (end (slack-buffer-ts-not-eq beg (point-max) ts)))
       (if (and beg end)
           (let ((inhibit-read-only t)
                 (lui-time-stamp-last (get-text-property beg 'slack-last-ts)))
             (delete-region beg end)
             (set-marker lui-output-marker beg)
             (slack-buffer-insert msg
                                  (slack-team-find slack-current-team-id))

             (slack-buffer-recover-lui-output-marker)
             (slack-buffer-goto ts)))))))

(defun slack-buffer-recover-lui-output-marker ()
  (set-marker lui-output-marker (- (marker-position
                                    lui-input-marker)

                                   (length lui-prompt-string))))

(defun slack-get-info-buffer-create (room)
  (let* ((buf-name (slack-room-buffer-name room))
         (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (slack-info-mode)
        (slack-buffer-insert-previous-link room)
        (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
        (add-hook 'lui-pre-output-hook 'slack-buffer-add-last-ts-property nil t)
        (add-hook 'lui-post-output-hook 'slack-buffer-add-ts-property nil t)))
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
