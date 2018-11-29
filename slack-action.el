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
(require 'slack-request)

(defvar slack-current-buffer)
(defvar slack-action-keymap)
(defvar slack-completing-read-function)
;; POST
(defconst slack-actions-list-url "https://slack.com/api/apps.actions.list")
;; POST
;; params (action_id, type, app_id, channel, message_ts)
(defconst slack-actions-run-url "https://slack.com/api/apps.actions.run")

(defface slack-message-action-face
  '((t (:box (:line-width 1 :style released-button))))
  "Face used to action."
  :group 'slack)

(defun slack-display-inline-action ()
  (goto-char (point-min))
  (let ((regexp "<slack-action://\\(.*?\\)/\\(.*?\\)|\\(.*?\\)>"))
    (while (re-search-forward regexp (point-max) t)
      (let ((bot (match-string 1))
            (payload (match-string 2))
            (label (match-string 3)))
        (replace-match (propertize label
                                   'face 'slack-message-action-face
                                   'bot bot
                                   'payload payload
                                   'org-text (match-string 0)
                                   'keymap slack-action-keymap))))))

(defun slack-actions-run (ts room type action-id app-id team)
  (slack-if-let*
      ((params (list (cons "message_ts" ts)
                     (cons "channel" (oref room id))
                     (cons "type" type)
                     (cons "action_id" action-id)
                     (cons "app_id" app-id)
                     (cons "client_token"
                           (slack-team-client-token team)))))
      (cl-labels
          ((success (&key data &allow-other-keys)
                    (slack-request-handle-error
                     (data "slack-actions-run"
                           #'(lambda (err) (slack-log (format "%s" err)
                                                      team
                                                      :level 'error))))))
        (slack-request
         (slack-request-create
          slack-actions-run-url
          team
          :type "POST"
          :params params
          :success #'success)))))

(defun slack-actions-list (team &optional after-success handle-error)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-actions-list" handle-error)
                 (when (functionp after-success)
                   (funcall after-success (plist-get data :app_actions))))))
    (slack-request
     (slack-request-create
      slack-actions-list-url
      team
      :type "POST"
      :success #'success))))

(defun slack-actions-select (actions)
  (cl-labels ((display-p (action)
                         (if (functionp (plist-get action :display-p))
                             (funcall (plist-get action :display-p))
                           t))
              (build-choice (action app)
                            (cons (format "%s - %s"
                                          (plist-get action :name)
                                          (plist-get app :app_name))
                                  (cons app action))))
    (let* ((choices (cl-loop for app in actions
                             nconc (mapcar #'(lambda (action) (build-choice action app))
                                           (cl-remove-if #'(lambda (action)
                                                             (not (display-p action)))
                                                         (plist-get app :actions)))))
           (choice (funcall slack-completing-read-function
                            "Select Action: "
                            choices
                            nil t)))
      (cdr (cl-assoc choice choices :test #'string=)))))

(provide 'slack-action)
;;; slack-action.el ends here
