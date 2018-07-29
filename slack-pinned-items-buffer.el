;;; slack-pinned-items-buffer.el ---                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017

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

(require 'eieio)
(require 'slack-util)
(require 'slack-room-buffer)

(define-derived-mode slack-pinned-items-buffer-mode slack-buffer-mode "Slack Pinned Items")

(defclass slack-pinned-items-buffer (slack-room-buffer)
  ((items :initarg :items :type list)))

(defmethod slack-buffer-name :static ((class slack-pinned-items-buffer) room team)
  (format "%s %s" (call-next-method) "Pinned Items"))

(defmethod slack-buffer-name ((this slack-pinned-items-buffer))
  (with-slots (room team) this
    (slack-buffer-name 'slack-pinned-items-buffer
                       room team)))

(defmethod slack-buffer-buffer ((this slack-pinned-items-buffer))
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (slack-pinned-items-buffer-insert-items this)
        buf)
    (slack-buffer-init-buffer this)))

(defmethod slack-pinned-items-buffer-insert-items ((this slack-pinned-items-buffer))
  (let* ((buf (get-buffer (slack-buffer-name this)))
         (header-face '(:underline t :weight bold))
         (buf-header (propertize "Pinned Items\n" 'face header-face)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) lui-output-marker))
      (let ((lui-time-stamp-position nil))
        (lui-insert buf-header t))
      (with-slots (team items) this
        (if (< 0 (length items))
            (cl-loop for m in items
                     do (slack-buffer-insert this m t))
          (let ((inhibit-read-only t))
            (insert "No Pinned Items")))))))

(defmethod slack-buffer-init-buffer ((this slack-pinned-items-buffer))
  (let* ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-pinned-items-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (slack-pinned-items-buffer-insert-items this)
    (with-slots (room team) this
      (slack-buffer-push-new-3 'slack-pinned-items-buffer room team))
    buf))

(defun slack-create-pinned-items-buffer (room team items)

  (cl-labels
      ((create-item
        (item)
        (let ((type (plist-get item :type)))
          (slack-pinned-item-create
           (cond
            ((string= type "message")
             (slack-message-create (plist-get item :message)
                                   team :room room))
            ((string= type "file")
             (or (slack-file-find (plist-get (plist-get item :file) :id) team)
                 (slack-file-create (plist-get item :file)))))))))
    (slack-if-let* ((buf (slack-buffer-find 'slack-pinned-items-buffer room team)))
        (progn
          (oset buf items (mapcar #'create-item items))
          buf)
      (slack-pinned-items-buffer :room room
                                 :team team
                                 :items (mapcar #'create-item items)))))



(provide 'slack-pinned-items-buffer)
;;; slack-pinned-items-buffer.el ends here
