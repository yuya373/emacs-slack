;;; slack-message-editor.el ---  edit message interface  -*- lexical-binding: t; -*-

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
(require 'slack-message-sender)

(defconst slack-message-edit-url "https://slack.com/api/chat.update")
(defconst slack-message-edit-buffer-name "*Slack - Edit message*")
(defconst slack-message-write-buffer-name "*Slack - Write message*")
(defconst slack-message-share-buffer-name "*Slack - Share message*")
(defconst slack-share-url "https://slack.com/api/chat.shareMessage")
(defvar slack-buffer-function)
(defvar slack-target-ts)
(make-local-variable 'slack-target-ts)
(defvar slack-message-edit-buffer-type)
(make-local-variable 'slack-message-edit-buffer-type)
(defvar slack-current-room-id)
(defvar slack-current-team-id)

(defvar slack-edit-message-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-s C-m") #'slack-message-embed-mention)
    (define-key keymap (kbd "C-s C-c") #'slack-message-embed-channel)
    (define-key keymap (kbd "C-c C-k") #'slack-message-cancel-edit)
    (define-key keymap (kbd "C-c C-c") #'slack-message-send-from-buffer)
    keymap))

(define-derived-mode slack-edit-message-mode fundamental-mode "Slack Edit Msg"
  ""
  (slack-buffer-enable-emojify))

(defun slack-message-share ()
  (interactive)
  (let ((ts (slack-get-ts))
        (buf (get-buffer-create slack-message-share-buffer-name))
        (room-id slack-current-room-id)
        (team slack-current-team))
    (unless ts
      (error "Can't find message."))
    (with-current-buffer buf
      (slack-message-setup-edit-buf (slack-room-find room-id team)
                                    'share
                                    :ts ts
                                    :team team))
    (funcall slack-buffer-function buf)))

(defun slack-message-share--send (team room ts msg)
  (let* ((slack-room-list (or (and (object-of-class-p room 'slack-channel)
                                   (slack-message-room-list team))
                              (list (cons (slack-room-name-with-team-name room)
                                          room))))
         (share-channel-id (oref (slack-select-from-list
                                  (slack-room-list
                                   "Select Channel: "
                                   :initial
                                   (slack-room-name room)))
                                 id)))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-message-share"))))
      (slack-request
       slack-share-url
       team
       :type "POST"
       :params (list (cons "channel" (oref room id))
                     (cons "timestamp" ts)
                     (cons "text" msg)
                     (cons "share_channel" share-channel-id))
       :sync nil
       :success))))

(defun slack-message-write-another-buffer ()
  (interactive)
  (let* ((cur-buf (current-buffer))
         (cur-mode (with-current-buffer cur-buf major-mode))
         (team (slack-team-find slack-current-team-id))
         (target-room (if (boundp 'slack-current-room-id)
                          (slack-room-find slack-current-room-id
                                           team)
                        (slack-message-read-room team)))
         (buf (get-buffer-create slack-message-write-buffer-name)))
    (cl-case cur-mode
      ('slack-mode
       (with-current-buffer buf
         (slack-message-setup-edit-buf target-room 'new
                                       :team team)))
      ('slack-thread-mode
       (let ((ts slack-target-ts))
         (with-current-buffer buf
           (slack-thread-setup-edit-buf ts
                                        target-room
                                        team
                                        'thread-message-new)))))

    (funcall slack-buffer-function buf)))

(defmethod slack-message-get-user-id ((m slack-user-message))
  (oref m user))

(defmethod slack-message-get-user-id ((m slack-file-comment-message))
  (oref (oref m comment) user))

(defun slack-message-edit ()
  (interactive)
  (let* ((team (slack-team-find slack-current-team-id))
         (room (slack-room-find slack-current-room-id
                                team))
         (ts (slack-get-ts))
         (msg (slack-room-find-message room ts)))
    (unless msg
      (error "Can't find original message"))
    (unless (string= (oref team self-id) (slack-message-get-user-id msg))
      (error "Cant't edit other user's message"))
    (slack-message-edit-text msg room)))

(defmethod slack-message-edit-type ((_m slack-message))
  'edit)

(defmethod slack-message-edit-type ((_m slack-file-comment-message))
  'edit-file-comment)

(defmethod slack-message-get-text ((m slack-message))
  (oref m text))

(defmethod slack-message-get-text ((m slack-file-comment-message))
  (oref (oref m comment) comment))

(defun slack-message-edit-text (msg room)
  (let ((buf (get-buffer-create slack-message-edit-buffer-name))
        (team (slack-team-find slack-current-team-id)))
    (with-current-buffer buf
      (slack-edit-message-mode)
      (slack-message-setup-edit-buf room
                                    (slack-message-edit-type msg)
                                    :ts (oref msg ts)
                                    :team team)
      (insert (slack-message-get-text msg)))
    (funcall slack-buffer-function buf)))

(cl-defun slack-message-setup-edit-buf (room buf-type &key ts team)
  (slack-edit-message-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (set (make-local-variable 'slack-target-ts) ts)
  (set (make-local-variable 'slack-message-edit-buffer-type) buf-type)
  (slack-buffer-set-current-room-id room)
  (slack-buffer-set-current-team-id team)
  (message "C-c C-c to send edited msg"))

(defun slack-message-cancel-edit ()
  (interactive)
  (let* ((team (slack-team-find slack-current-team-id))
         (room (slack-room-find slack-current-room-id
                                team)))
    (erase-buffer)
    (delete-window)
    (slack-room-make-buffer-with-room room team)))

(defun slack-message-send-from-buffer ()
  (interactive)
  (cl-labels ((get-team () (slack-team-find slack-current-team-id))
              (get-room (team) (slack-room-find slack-current-room-id team)))
    (let ((buf-string (buffer-substring (point-min) (point-max))))
      (cl-case slack-message-edit-buffer-type
        ('edit-file-comment
         (slack-file-comment-edit slack-current-room-id
                                  slack-current-team-id
                                  slack-target-ts
                                  buf-string))
        ('edit
         (let ((team (get-team)))
           (slack-message--edit (oref (get-room team) id)
                                team
                                slack-target-ts
                                buf-string)))
        ('share
         (let ((team (get-team)))
           (slack-message-share--send team
                                      (get-room team)
                                      slack-target-ts
                                      buf-string)))
        ('new (slack-message--send buf-string))
        ('thread-message-new (slack-thread-message--send buf-string)))
      (kill-buffer)
      (delete-window))))

(defun slack-message--edit (channel team ts text)
  (cl-labels ((on-edit (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-message--edit"))))
    (slack-request
     slack-message-edit-url
     team
     :type "POST"
     :sync nil
     :params (list (cons "channel" channel)
                   (cons "ts" ts)
                   (cons "text" text))
     :success #'on-edit)))

(provide 'slack-message-editor)
;;; slack-message-editor.el ends here
