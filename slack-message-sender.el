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

(defvar slack-message-minibuffer-local-map nil)
(defvar slack-buffer-function)

(defun slack-message-send ()
  (interactive)
  (slack-message--send (slack-message-read-from-minibuffer)))

(defun slack-message-inc-id (team)
  (with-slots (message-id) team
    (if (eq message-id (1- most-positive-fixnum))
        (setq message-id 1)
      (cl-incf message-id))))

(defun slack-message--send (message)
  (if slack-current-team-id
      (let ((team (slack-team-find slack-current-team-id)))
        (slack-message-inc-id team)
        (with-slots (message-id sent-message self-id) team
          (let* ((m (list :id message-id
                          :channel (slack-message-get-room-id)
                          :type "message"
                          :user self-id
                          :text message))
                 (json (json-encode m))
                 (obj (slack-message-create m)))
            (slack-ws-send json team)
            (puthash message-id obj sent-message))))
    (error "Call from Slack Buffer")))

(defun slack-message-get-room-id ()
  (if (and (boundp 'slack-current-room-id)
           (boundp 'slack-current-team-id))
      (oref (slack-room-find slack-current-room-id
                             (slack-team-find slack-current-team-id))
            id)
    (oref (slack-message-read-room (slack-team-select)) id)))

(defun slack-message-read-room (team)
  (let* ((list (slack-message-room-list team))
         (choices (mapcar #'car list))
         (room-name (slack-message-read-room-list "Select Room: " choices))
         (room (cdr (cl-assoc room-name list :test #'string=))))
    room))

(defun slack-message-read-room-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s" prompt)
                     choices nil t nil nil choices)))

(defun slack-message-room-list (team)
  (append (slack-group-names team)
          (slack-im-names team)
          (slack-channel-names team)))

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
  (let ((team (slack-team-select)))
    (let* ((alist (slack-channel-names team)))
      (slack-select-from-list
       (alist "Select Channel: ")
       (let* ((room selected)
              (room-name (slack-room-name room)))
         (insert (concat "<#" (oref room id) "|" room-name "> ")))))))

(defun slack-message-embed-mention ()
  (interactive)
  (let ((team (slack-team-select)))
    (let* ((pre-defined (list (cons "here" "here")
                              (cons "channel" "channel")))
           (alist (append pre-defined (slack-user-names team))))
      (slack-select-from-list
       (alist "Select User: ")
       (let ((user-id (plist-get selected :id)))
         (if (or (string= user-id "here")
                 (string= user-id "channel"))
             (insert (concat "<!" selected "> "))
           (let* ((user-name (slack-user-name user-id team)))
             (insert (concat "<@" user-id "|" user-name "> ")))))))))


(provide 'slack-message-sender)
;;; slack-message-sender.el ends here
