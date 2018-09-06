;;; slack-conversations.el ---                       -*- lexical-binding: t; -*-

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
(require 'slack-request)
(require 'slack-room)

(defconst slack-conversations-archive-url
  "https://slack.com/api/conversations.archive")
(defconst slack-conversations-invite-url
  "https://slack.com/api/conversations.invite")

(cl-defun slack-conversations-success-handler (&key data &allow-other-keys)
  (slack-request-handle-error
   (data "conversations")))

(defun slack-conversations-archive (room team)
  (let ((id (oref room id)))
    (slack-request
     (slack-request-create
      slack-conversations-archive-url
      team
      :type "POST"
      :params (list (cons "channel" id))
      :success #'slack-conversations-success-handler))))

(defun slack-conversations-invite (room team)
  (let ((channel (oref room id))
        (users (plist-get (slack-select-from-list
                              ((slack-user-names team)
                               "Select User: ")) :id)))
    (slack-request
     (slack-request-create
      slack-conversations-invite-url
      team
      :type "POST"
      :params (list (cons "channel" channel)
                    (cons "users" users))
      :success #'slack-conversations-success-handler
      ))))


(provide 'slack-conversations)
;;; slack-conversations.el ends here
