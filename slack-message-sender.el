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
(require 'slack-util)
(require 'slack-websocket)
(require 'slack-room)
(require 'slack-im)
(require 'slack-group)
(require 'slack-message)
(require 'slack-channel)
(require 'slack-slash-commands)

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

(defun slack-escape-message (message)
  "Escape '<,' '>' & '&' in MESSAGE."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string "&" "&amp;" message))))

(defun slack-link-users (message team)
  "Add links to all references to valid users in MESSAGE."
  (replace-regexp-in-string
   "@\\<\\([A-Za-z0-9._\-]+\\)\\>"
   #'(lambda (text)
       (let* ((username (match-string 1 text))
              (id (slack-user-get-id username team)))
         (if id
             (format "<@%s|%s>" id username)
           (cond
            ((string= username "here") "<!here|here>")
            ((cl-find username '("channel" "group") :test #'string=) "<!channel>")
            ((string= username "everyone") "<!everyone>")
            (t text)))))
   message t))

(defun slack-link-channels (message team)
  "Add links to all references to valid channels in MESSAGE."
  (let ((channel-ids
         (mapcar #'(lambda (x)
                     (let ((channel (cdr x)))
                       (cons (slack-room-name channel team)
                             (slot-value channel 'id))))
                 (slack-channel-names team))))
    (replace-regexp-in-string
     "#\\<\\([A-Za-z0-9_\-]+\\)\\>"
     #'(lambda (text)
         (let* ((channel (match-string 1 text))
                (id (cdr (assoc channel channel-ids))))
           (if id
               (format "<#%s|%s>" id channel)
             text)))
     message t)))

(defun slack-message-prepare-links (message team)
  (slack-link-channels (slack-link-users message team) team))

(defun slack-message--send (message)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (oref buf team))
                  (room (oref buf room)))
      (if (string-prefix-p "/" message)
          (slack-if-let* ((command-and-arg (slack-slash-commands-parse message team)))
              (slack-command-run (car command-and-arg)
                                 team
                                 (oref room id)
                                 :text (cdr command-and-arg))
            (error "Unknown slash command: %s"
                   (car (split-string message))))
        (slack-buffer-send-message buf message))))

(defun slack-message-send-internal (message channel-id team)
  (slack-message-inc-id team)
  (with-slots (message-id sent-message self-id) team
    (let* ((m (list :id message-id
                    :channel channel-id
                    :type "message"
                    :user self-id
                    :text (slack-message-prepare-links
                           (slack-escape-message message)
                           team)))
           (obj (slack-message-create m team)))
      (slack-ws-send m team)
      (puthash message-id obj sent-message))))

(defun slack-message-read-room (team)
  (let* ((list (slack-message-room-list team))
         (choices (mapcar #'car list))
         (room-name (slack-message-read-room-list "Select Room: " choices))
         (room (cdr (cl-assoc room-name list :test #'string=))))
    room))

(defun slack-message-read-room-list (prompt choices)
  (let ((completion-ignore-case t))
    (funcall slack-completing-read-function (format "%s" prompt)
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
  (slack-if-let* ((buf slack-current-buffer))
      (with-slots (team) buf
        (slack-select-from-list
            ((slack-channel-names team) "Select Channel: ")
            (insert (concat "#" (slack-room-name selected team)))))))

(defun slack-message-embed-mention ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (with-slots (team) buf
        (let* ((pre-defined (list (list "here" :name "here")
                                  (list "channel" :name "channel")))
               (alist (append pre-defined (slack-user-names team))))
          (slack-select-from-list
              (alist "Select User: ")
              (insert (concat "@" (plist-get selected :name))))))))

(provide 'slack-message-sender)
;;; slack-message-sender.el ends here
