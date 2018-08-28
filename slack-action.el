;;; slack-action.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@yuya373>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-util)

(defvar slack-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-action-run)
    (define-key keymap [mouse-1] #'slack-action-run)
    keymap))

(defface slack-message-action-face
  '((t (:box (:line-width 1 :style released-button))))
  "Face used to action."
  :group 'slack)

(defun slack-message-run-action ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (oref buffer room))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (_not-ephemeral-messagep (not (oref message is-ephemeral))))
      (slack-buffer-execute-message-action buffer ts)))

(defun slack-action-run ()
  (interactive)
  (slack-if-let* ((bot (get-text-property (point) 'bot))
                  (payload (get-text-property (point) 'payload))
                  (buffer slack-current-buffer)
                  (team (oref buffer team)))
      (let ((url "https://slack.com/api/chat.action")
            (params (list (cons "bot" bot)
                          (cons "payload" payload))))
        (cl-labels
            ((log-error (err) (format "Error: %s, URL: %s, PARAMS: %s"
                                      err
                                      url
                                      params))
             (on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-action-run" #'log-error))))
          (slack-request
           (slack-request-create
            url
            team
            :type "POST"
            :params params
            :success #'on-success))))))

(defun slack-display-inline-action ()
  (goto-char (point-min))
  (let ((regexp "<slack-action://\\(.*?\\)/\\(.*?\\)|\\(.*?\\)>"))
    (while (re-search-forward regexp (point-max) t)
      (let ((bot (match-string 1))
            (payload (match-string 2))
            (label (match-string 3))
            (beg (- (match-beginning 1) 16))
            (end (+ (match-end 3) 1)))
        (replace-match (propertize label
                                   'face 'slack-message-action-face
                                   'bot bot
                                   'payload payload
                                   'org-text (match-string 0)
                                   'keymap slack-action-keymap))))))

(provide 'slack-action)
;;; slack-action.el ends here
