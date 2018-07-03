;;; slack-reaction.el ---  deal with reactions       -*- lexical-binding: t; -*-

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

(defclass slack-reaction ()
  ((name :initarg :name :type string)
   (count :initarg :count :type integer)
   (users :initarg :users :initform ())))

(defmethod slack-reaction-join ((r slack-reaction) other)
  (if (string= (oref r name) (oref other name))
      (progn
        (cl-incf (oref r count))
        (oset r users (append (oref other users) (oref r users)))
        r)))

(defmethod slack-reaction-user-names ((r slack-reaction) team)
  (with-slots (users) r
    (mapcar #'(lambda (u) (slack-user-name u team))
            users)))

(defmethod slack-equalp ((r slack-reaction) other)
  (slack-reaction-equalp r other))

(defmethod slack-reaction-equalp ((r slack-reaction) other)
  (string= (oref r name) (oref other name)))

(defmethod slack-reaction-help-text ((r slack-reaction) team)
  (let ((user-names (slack-reaction-user-names r team)))
    (format "%s reacted with :%s:"
            (mapconcat #'identity user-names ", ")
            (oref r name))))

(defvar slack-reaction-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-reaction-toggle)
    (define-key keymap [mouse-1] #'slack-reaction-toggle)
    keymap))

(defun slack-reaction-toggle ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property (point) 'reaction))
                  (reaction-name (oref reaction name))
                  (reaction-users (oref reaction users))
                  (team (oref buffer team))
                  (self-id (oref team self-id)))
      (if (cl-find-if #'(lambda (id) (string= id self-id)) reaction-users)
          (slack-message-reaction-remove reaction-name
                                         (slack-get-ts)
                                         (oref buffer room)
                                         team)
        (slack-message--add-reaction buffer reaction-name))))

(defun slack-reaction-help-echo (_window _string pos)
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property pos 'reaction))
                  (team (oref buffer team)))
      (slack-reaction-help-text reaction team)))

(defmethod slack-reaction-to-string ((r slack-reaction) team)
  (propertize (format " :%s: %d " (oref r name) (oref r count))
              'face 'slack-message-output-reaction
              'mouse-face 'highlight
              'keymap slack-reaction-keymap
              'reaction r
              'help-echo #'slack-reaction-help-echo))

(defun slack-reaction-echo-description ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property (point) 'reaction))
                  (team (oref buffer team)))
      (message (slack-reaction-help-text reaction team))))

(defun slack-reaction-notify (payload team room)
  (let* ((user-id (plist-get payload :user))
         (reaction (plist-get payload :reaction))
         (msg (slack-user-message "msg"
                                  :text (format "added reaction %s" reaction)
                                  :user user-id)))
    (slack-message-notify msg room team)))

(defun slack-reaction--find (reactions reaction)
  (cl-find-if #'(lambda (e) (slack-reaction-equalp e reaction))
              reactions))

(defun slack-reaction--delete (reactions reaction)
  (cl-delete-if #'(lambda (e) (slack-reaction-equalp e reaction))
                reactions))

(provide 'slack-reaction)
;;; slack-reaction.el ends here

