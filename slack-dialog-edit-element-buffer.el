;;; slack-dialog-edit-element-buffer.el ---          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-dialog-buffer)

(defclass slack-dialog-edit-element-buffer (slack-buffer)
  ((dialog-buffer :initarg :dialog-buffer :type slack-dialog-buffer)
   (element :initarg :element :type slack-dialog-element)))

(define-derived-mode slack-dialog-edit-element-buffer-mode fundamental-mode "Slack Dialog Edit Element Buffer"
  (setq-local default-directory slack-default-directory))

(setq slack-dialog-edit-element-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
      #'slack-dialog-edit-buffer-save-content)
    (define-key map (kbd "C-c C-k")
      #'slack-dialog-edit-buffer-abort)
    map))

(defun slack-dialog-edit-buffer-save-content ()
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min)
                                                 (point-max)))
        (buffer slack-current-buffer))
    (with-slots (element dialog-buffer) buffer
      (slack-dialog-edit-buffer-abort)
      (slack-dialog-buffer-save-element-value dialog-buffer
                                              (oref element name)
                                              content))))

(defun slack-dialog-edit-buffer-abort ()
  (interactive)
  (let* ((buffer-name (slack-buffer-name slack-current-buffer))
         (buf (get-buffer buffer-name))
         (win (get-buffer-window buf)))
    (kill-buffer buf)
    (when (< 1 (count-windows))
      (delete-window win))))

(defmethod slack-buffer-name :static ((_class slack-dialog-edit-element-buffer)
                                      dialog-buffer element team)
  (with-slots (dialog dialog-id) dialog-buffer
    (with-slots (name) element
      (with-slots (title) dialog
        (format "* Slack Dialog Edit Element - %s [%s] edit %s : %s"
                title dialog-id name (slack-team-name team))))))

(defmethod slack-buffer-find :static ((class slack-dialog-edit-element-buffer)
                                      dialog-buffer element team)
  (slack-buffer-find-4 class dialog-buffer element team))

(defmethod slack-buffer-name ((this slack-dialog-edit-element-buffer))
  (with-slots (dialog-buffer element team) this
    (slack-buffer-name 'slack-dialog-edit-element-buffer
                       dialog-buffer element team)))

(defun slack-create-dialog-element-edit-buffer (dialog-buffer element team)
  (slack-if-let*
      ((buf (slack-buffer-find 'slack-dialog-edit-element-buffer
                               dialog-buffer
                               element
                               team)))
      buf
    (make-instance 'slack-dialog-edit-element-buffer
                   :dialog-buffer dialog-buffer
                   :element element
                   :team team)))

(defmethod slack-buffer-init-buffer ((this slack-dialog-edit-element-buffer))
  (let* ((buf (generate-new-buffer (slack-buffer-name this)))
         (element (oref this element)))
    (with-current-buffer buf
      (slack-dialog-edit-element-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char (point-min))
      (with-slots (value label) element
        (setq-local header-line-format
                    (format "%s: C-c to save content" label))
        (insert (or value ""))))
    (slack-buffer-push-new-4 'slack-dialog-edit-element-buffer
                             (oref this dialog-buffer)
                             element
                             (oref this team))))


(provide 'slack-dialog-edit-element-buffer)
;;; slack-dialog-edit-element-buffer.el ends here
