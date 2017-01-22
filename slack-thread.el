;;; slack-thread.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-message)

(defvar lui-prompt-string "> ")
(define-derived-mode slack-thread-mode lui-mode "Slack - Thread"
  ""
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-thread-message--send))

(defclass slack-thread ()
  ((thread-ts :initarg :thread_ts :initform "")
   (messages :initarg :messages :initform '())
   (has-unreads :initarg :has_unreads :initform nil)
   (mention-count :initarg :mention_count :initform 0)
   (reply-count :initarg :reply_count :initform 0)
   (replies :initarg :replies :initform '())
   (active :initarg :active :initform t)
   ))
(defun slack-thread-buf-name (room thread-ts)
  (format "%s %s - %s" (slack-room-buffer-name room) "Thread" thread-ts))

(defun slack-thread-get-buffer-create (room team thread-ts)
  (let* ((buf-name (slack-thread-buf-name room thread-ts))
         (buf (get-buffer buf-name)))
    (when buf
      (with-current-buffer buf (kill-buffer))
      (setq buf nil))
    (unless buf
      (setq buf (generate-new-buffer buf-name))
      (with-current-buffer buf
        (slack-thread-mode)
        (slack-buffer-enable-emojify)
        (goto-char lui-input-marker)
        (set (make-local-variable 'thread-ts) thread-ts)
        (set (make-local-variable 'slack-current-team-id) (oref team id))
        (set (make-local-variable 'slack-current-room-id) (oref room id))
        ;; (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
        (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)))
    buf))

(defun slack-thread-show-messages ()
  (interactive)
  (let* ((line (thing-at-point 'line))
         (team-id slack-current-team-id)
         (team (and team-id (slack-team-find team-id)))
         (room-id slack-current-room-id)
         (room (and room-id team (slack-room-find room-id team)))
         (ts (slack-get-ts)))
    (if (or (not team) (not room) (not ts))
        (error "Can't find: %s" (or (and (not team) "team")
                                    (and (not room) "room")
                                    (and (not ts) "ts"))))
    (let ((thread (oref (slack-room-find-message room ts) thread))
          (buf (slack-thread-get-buffer-create room team ts)))

      (unless thread (error "Can't find thread"))

      (with-current-buffer buf
        (cl-loop for m in (slack-room-sort-messages (copy-sequence (oref thread messages)))
                 do (slack-buffer-insert m team)))

      (funcall slack-buffer-function buf))))

(defmethod slack-thread-to-string ((m slack-message) team)
  (with-slots (thread) m
    (if thread
        (progn
          (propertize
           (format "%s reply from %s"
                   (oref thread reply-count)
                   (mapconcat #'identity
                              (cl-remove-duplicates
                               (mapcar #'(lambda (reply)
                                           (slack-user-name
                                            (plist-get reply :user)
                                            team))
                                       (oref thread replies))
                               :test #'string=)
                              " "))
           'face '(:underline t)))
      "")))

(defun slack-thread-create (thread-ts)
  (when thread-ts
    (make-instance 'slack-thread :thread_ts thread-ts)))

(defmethod slack-thread-set-messages ((thread slack-thread) messages)
  (let ((count (length messages)))
    (oset thread messages messages)
    (oset thread reply-count count)
    (oset thread replies (mapcar #'(lambda (m) (list :user (slack-message-get-user-id m)
                                                     :ts (oref m ts)))
                                 messages))))

(defmethod slack-thread-add-message ((thread slack-thread) msg)
  (with-slots (messages) thread
    (cl-pushnew msg messages :test #'slack-message-equal)
    (setq messages (slack-room-sort-messages (copy-sequence messages)))))

(provide 'slack-thread)
;;; slack-thread.el ends here
