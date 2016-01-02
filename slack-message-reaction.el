;;; slack-message-reaction.el --- adding, removing reaction from message  -*- lexical-binding: t; -*-

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

(require 'slack-message)
(require 'slack-reaction)
(require 'slack-room)

(defconst slack-message-reaction-add-url "https://slack.com/api/reactions.add")
(defconst slack-message-reaction-remove-url "https://slack.com/api/reactions.remove")
(defvar slack-current-room)
(make-local-variable 'slack-current-room)
(defvar slack-token)

(defun slack-message-add-reaction ()
  (interactive)
  (let* ((word (thing-at-point 'word))
         (ts (get-text-property 0 'ts word))
         (reaction (slack-message-reaction-input)))
    (slack-message-reaction-add reaction ts slack-current-room)))

(defun slack-message-remove-reaction ()
  (interactive)
  (let* ((room slack-current-room)
         (word (thing-at-point 'word))
         (ts (get-text-property 0 'ts word))
         (msg (slack-room-find-message room ts))
         (reactions (oref msg reactions))
         (reaction (slack-message-reaction-select reactions)))
    (slack-message-reaction-remove reaction ts room)))

(defun slack-message-show-reaction-users ()
  (interactive)
  (let* ((reaction (ignore-errors (get-text-property (point) 'reaction))))
    (if reaction
        (let ((user-names (slack-reaction-user-names reaction)))
          (message "reacted users: %s" (mapconcat #'identity user-names ", ")))
      (message "Can't get reaction:"))))

(defun slack-message-reaction-select (reactions)
  (let ((list (mapcar #'(lambda (r) (oref r name))
                      reactions)))
    (slack-select-from-list
     (list "Select Reaction: ")
     selected)))

(defun slack-message-reaction-input ()
  (let ((prompt "Emoji: "))
    (read-from-minibuffer prompt)))

(defun slack-message-reaction-add (reaction ts room)
  (cl-labels ((on-reaction-add
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-reaction-add"))))
    (slack-request
     slack-message-reaction-add-url
     :type "POST"
     :sync nil
     :params (list (cons "token" slack-token)
                   (cons "channel" (oref room id))
                   (cons "timestamp" ts)
                   (cons "name" reaction))
     :success #'on-reaction-add)))

(defun slack-message-reaction-remove (reaction ts room)
  (cl-labels ((on-reaction-remove
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-reaction-remove"))))
    (slack-request
     slack-message-reaction-remove-url
     :type "POST"
     :sync nil
     :params (list (cons "token" slack-token)
                   (cons "channel" (oref room id))
                   (cons "timestamp" ts)
                   (cons "name" reaction))
     :success #'on-reaction-remove)))

(cl-defmacro slack-message-find-reaction ((m reaction) &body body)
  `(let ((same-reaction (cl-find-if #'(lambda (r) (slack-reaction-equalp r ,reaction))
                                    (oref ,m reactions))))
     ,@body))

(defmethod slack-message-append-reaction ((m slack-message) reaction)
  (slack-message-find-reaction (m reaction)
                           (if same-reaction
                               (join same-reaction reaction)
                             (push reaction (oref m reactions)))))

(defmethod slack-message-pop-reaction ((m slack-message) reaction)
  (slack-message-find-reaction (m reaction)
                               (if same-reaction
                                   (if (eq 1 (oref same-reaction count))
                                       (oset m reactions
                                             (cl-delete-if #'(lambda (r)
                                                               (slack-reaction-equalp same-reaction r))
                                                           (oref m reactions)))
                                     (cl-decf (oref same-reaction count))))))

(provide 'slack-message-reaction)
;;; slack-message-reaction.el ends here
