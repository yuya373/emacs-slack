;;; slack-reaction-event.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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
(require 'eieio)
(require 'slack-util)
(require 'slack-event)
(require 'slack-message-buffer)

(defclass slack-reaction-event (slack-event slack-message-event-processable) ())
(defclass slack-message-reaction-event (slack-reaction-event) ())
(defclass slack-message-reaction-added-event (slack-message-reaction-event) ())
(defclass slack-message-reaction-removed-event (slack-message-reaction-event) ())

(defun slack-create-reaction-event (payload)
  (let* ((type (plist-get payload :type))
         (item (plist-get payload :item))
         (item-type (plist-get item :type)))
    (cond ((string= "message" item-type)
           (cond ((string= "reaction_added" type)
                  (slack-create-message-reaction-added-event payload))
                 ((string= "reaction_removed" type)
                  (slack-create-message-reaction-removed-event payload)))))))

(defun slack-create-message-reaction-added-event (payload)
  (slack-message-reaction-added-event :type (plist-get payload :type)
                                      :payload payload))

(defun slack-create-message-reaction-removed-event (payload)
  (slack-message-reaction-removed-event :type (plist-get payload :type)
                                        :payload payload))

(cl-defmethod slack-event-find-message ((this slack-message-reaction-event) team)
  (let* ((payload (oref this payload))
         (item (plist-get payload :item))
         (channel (plist-get item :channel))
         (room (slack-room-find channel team))
         (ts (plist-get item :ts)))
    (when room
      (slack-room-find-message room ts))))

(cl-defmethod slack-event-save-message ((this slack-message-reaction-removed-event) message _team)
  (let* ((payload (oref this payload))
         (user-id (plist-get payload :user))
         (reaction (slack-reaction :name (plist-get payload :reaction)
                                   :count 1
                                   :users (list user-id))))
    (slack-if-let* ((old-reaction (slack-reaction-find message reaction)))
        (if (< 1 (oref old-reaction count))
            (slack-reaction-remove-user old-reaction user-id)
          (slack-reaction-delete message reaction)))))

(cl-defmethod slack-event-save-message ((this slack-message-reaction-added-event) message _team)
  (let* ((payload (oref this payload))
         (reaction (slack-reaction :name (plist-get payload :reaction)
                                   :count 1
                                   :users (list (plist-get payload :user)))))
    (slack-if-let* ((old-reaction (slack-reaction-find message reaction)))
        (slack-reaction-join old-reaction reaction)
      (slack-reaction-push message reaction))))

(cl-defmethod slack-event-update-buffer ((_this slack-message-reaction-event) message team)
  (slack-message-replace-buffer message team))


(provide 'slack-reaction-event)
;;; slack-reaction-event.el ends here
