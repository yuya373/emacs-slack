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
(require 'slack-util)
(require 'slack-room)
(require 'slack-message-sender)

(defconst slack-message-edit-url "https://slack.com/api/chat.update")
(defconst slack-message-edit-buffer-name "*Slack - Edit message*")
(defconst slack-message-write-buffer-name "*Slack - Write message*")
(defconst slack-message-share-buffer-name "*Slack - Share message*")
(defconst slack-share-url "https://slack.com/api/chat.shareMessage")
(defvar slack-completing-read-function)
(defvar slack-buffer-function)
(defvar slack-target-ts)
(make-local-variable 'slack-target-ts)
(defvar slack-message-edit-buffer-type)
(make-local-variable 'slack-message-edit-buffer-type)

(defvar slack-edit-message-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-s C-m") #'slack-message-embed-mention)
    (define-key keymap (kbd "C-s C-c") #'slack-message-embed-channel)
    (define-key keymap (kbd "C-c C-k") #'slack-message-cancel-edit)
    (define-key keymap (kbd "C-c C-c") #'slack-message-send-from-buffer)
    keymap))

(define-derived-mode slack-edit-message-mode fundamental-mode "Slack Edit Msg"
  ""
  (slack-enable-wysiwyg)
  (slack-buffer-enable-emojify))

(defun slack-message-share--send (team room ts msg)
  (let* ((slack-room-list (or (and (object-of-class-p room 'slack-channel)
                                   (slack-message-room-list team))
                              (list (cons (slack-room-display-name room team)
                                          room))))
         (share-channel-id (oref (slack-select-from-list
                                     (slack-room-list
                                      "Select Channel: "
                                      :initial
                                      (slack-room-name room team)))
                                 id)))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-message-share"))))
      (slack-request
       (slack-request-create
        slack-share-url
        team
        :type "POST"
        :params (list (cons "channel" (oref room id))
                      (cons "timestamp" ts)
                      (cons "share_channel" share-channel-id)
                      (cons "blocks"
                            (json-encode (cdr (car (with-temp-buffer
                                                     (insert msg)
                                                     (slack-create-blocks-from-buffer)))))))
        :success #'on-success)))))

(defun slack-message-cancel-edit ()
  (interactive)
  (let ((buffer (slack-buffer-buffer slack-current-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (if (> (count-windows) 1) (delete-window)))))

(defun slack-message-send-from-buffer ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (text (buffer-substring-no-properties (point-min) (point-max))))
      (slack-buffer-send-message buf text)))

(defun slack-message--edit (channel team ts text)
  (cl-labels ((on-edit (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-message--edit"))))
    (slack-request
     (slack-request-create
      slack-message-edit-url
      team
      :type "POST"
      :headers (list (cons "Content-Type"
                           "application/json;charset=utf-8"))
      :data (json-encode (apply #'list
                                (cons "channel" channel)
                                (cons "ts" ts)
                                (with-temp-buffer
                                  (insert text)
                                  (slack-create-blocks-from-buffer))))
      :success #'on-edit))))

(provide 'slack-message-editor)
;;; slack-message-editor.el ends here
