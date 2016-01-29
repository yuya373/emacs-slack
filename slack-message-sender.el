;;; slack-message-sender.el --- slack message concern message sending  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
(require 'json)
(require 'slack-websocket)
(require 'slack-im)
(require 'slack-group)
(require 'slack-message)
(require 'slack-channel)

(defvar slack-message-id 0)
(defvar slack-message-minibuffer-local-map nil)
(defvar slack-sent-message (make-hash-table))
(defvar slack-buffer-function)
(defvar slack-my-user-id)

(defun slack-message-send ()
  (interactive)
  (slack-message--send (slack-message-read-from-minibuffer)))

(defun slack-message--send (message)
  (cl-incf slack-message-id)
  (let* ((m (list :id slack-message-id
                  :channel (slack-message-get-room-id)
                  :type "message"
                  :user slack-my-user-id
                  :text message))
         (json (json-encode m))
         (obj (slack-message-create m)))
    (slack-ws-send json)
    (puthash slack-message-id obj slack-sent-message)))

(defun slack-message-get-room-id ()
  (if (boundp 'slack-current-room)
      (oref slack-current-room id)
    (oref (slack-message-read-room) id)))

(defun slack-message-read-room ()
  (let* ((list (slack-message-room-list))
         (choices (mapcar #'car list))
         (room-name (slack-message-read-room-list "Select Room: " choices))
         (room (cdr (cl-assoc room-name list :test #'string=))))
    room))

(defun slack-message-read-room-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s" prompt)
                     choices nil t nil nil choices)))

(defun slack-message-room-list ()
  (append (slack-group-names) (slack-im-names)))

(defun slack-message-read-from-minibuffer ()
  (let ((prompt "Message: "))
    (slack-message-setup-minibuffer-keymap)
    (read-from-minibuffer
     prompt
     nil
     slack-message-minibuffer-local-map)))

(defun slack-message-setup-minibuffer-keymap ()
  (unless slack-message-minibuffer-local-map
    (setq slack-message-minibuffer-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'newline)
            (set-keymap-parent map minibuffer-local-map)
            map))))

(defun slack-message-embed-channel ()
  (interactive)
  (let* ((list (slack-channel-names))
         (candidates (mapcar #'car list)))
    (slack-select-from-list
     (candidates "Select Channel: ")
     (let* ((room (cdr (cl-assoc selected
                                 list
                                 :test #'string=)))
            (room-name (slack-room-name room)))
       (insert (concat "<#" (oref room id) "|" room-name "> "))))))

(defun slack-message-embed-mention ()
  (interactive)
  (let* ((pre-defined (list (cons "here" "here")
                            (cons "channel" "channel")))
         (list (append pre-defined (slack-user-names)))
         (candidates (mapcar #'car list)))
    (slack-select-from-list
     (candidates "Select User: ")
     (if (or (string= selected "here")
             (string= selected "channel"))
         (insert (concat "<!" selected "> "))
       (let* ((user-id (cdr (cl-assoc selected
                                      list
                                      :test #'string=)))
              (user-name (slack-user-name user-id)))
         (insert (concat "<@" user-id "|" user-name "> ")))))))


(provide 'slack-message-sender)
;;; slack-message-sender.el ends here
