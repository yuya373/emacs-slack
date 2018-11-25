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
(require 'slack-util)
;; (require 'slack-room-buffer)
(declare-function slack-buffer-toggle-reaction "slack-room-buffer")
(declare-function slack-buffer-reaction-help-text "slack-room-buffer")
(require 'slack-user)

(defvar slack-current-buffer)

(defclass slack-reaction ()
  ((name :initarg :name :type string)
   (count :initarg :count :type integer)
   (users :initarg :users :initform ())))

(cl-defmethod slack-reaction-join ((r slack-reaction) other)
  (if (string= (oref r name) (oref other name))
      (progn
        (cl-incf (oref r count))
        (oset r users (append (oref other users) (oref r users)))
        r)))

(cl-defmethod slack-reaction-user-names ((r slack-reaction) team)
  (with-slots (users) r
    (mapcar #'(lambda (u) (slack-user-name u team))
            users)))

(cl-defmethod slack-equalp ((r slack-reaction) other)
  (slack-reaction-equalp r other))

(cl-defmethod slack-reaction-equalp ((r slack-reaction) other)
  (string= (oref r name) (oref other name)))

(defvar slack-reaction-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-reaction-toggle)
    (define-key keymap [mouse-1] #'slack-reaction-toggle)
    keymap))

(defun slack-reaction-toggle ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property (point) 'reaction)))
      (slack-buffer-toggle-reaction buffer reaction)))

(cl-defmethod slack-reaction-help-text ((r slack-reaction) team)
  (let ((user-names (slack-reaction-user-names r team)))
    (format "%s reacted with :%s:"
            (mapconcat #'identity user-names ", ")
            (oref r name))))

(defun slack-reaction-help-echo (_window _string pos)
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property pos 'reaction)))
      (slack-buffer-reaction-help-text buffer reaction)))

(cl-defmethod slack-reaction-to-string ((r slack-reaction))
  (propertize (format " :%s: %d " (oref r name) (oref r count))
              'face 'slack-message-output-reaction
              'mouse-face 'highlight
              'keymap slack-reaction-keymap
              'reaction r
              'help-echo #'slack-reaction-help-echo))

(defun slack-reaction--find (reactions reaction)
  (cl-find-if #'(lambda (e) (slack-reaction-equalp e reaction))
              reactions))

(defun slack-reaction--delete (reactions reaction)
  (cl-delete-if #'(lambda (e) (slack-reaction-equalp e reaction))
                reactions))

(cl-defmethod slack-merge ((old slack-reaction) new)
  (with-slots (count users) old
    (setq count (oref new count))
    (setq users (cl-remove-duplicates (append users (oref new users))
                                      :test #'string=))))

(provide 'slack-reaction)
;;; slack-reaction.el ends here

