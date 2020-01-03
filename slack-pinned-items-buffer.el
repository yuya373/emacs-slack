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
(require 'slack-pinned-item)

(define-derived-mode slack-pinned-items-buffer-mode slack-buffer-mode "Slack Pinned Items")

(defclass slack-pinned-items-buffer (slack-room-buffer)
  ((items :initarg :items :type list)))

(cl-defmethod slack-buffer-name ((this slack-pinned-items-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (concat "*Slack - "
            (slack-team-name team)
            " : "
            (slack-room-name room team)
            " Pinned Items")))

(cl-defmethod slack-buffer-key ((_class (subclass slack-pinned-items-buffer)) room)
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-pinned-items-buffer))
  (slack-buffer-key 'slack-pinned-items-buffer (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-pinned-items-buffer)))
  'slack-pinned-items-buffer)

(cl-defmethod slack-pinned-items-buffer-insert-items ((this slack-pinned-items-buffer))
  (let* ((header-face '(:underline t :weight bold))
         (buf-header (propertize "Pinned Items\n" 'face header-face)))
    (let ((inhibit-read-only t))
      (delete-region (point-min) lui-output-marker))
    (let ((lui-time-stamp-position nil))
      (lui-insert buf-header t))
    (let ((items (oref this items)))
      (if (< 0 (length items))
          (cl-loop for m in items
                   do (slack-buffer-insert this m t))
        (let ((inhibit-read-only t))
          (lui-insert "No Pinned Items" t))))))

(cl-defmethod slack-buffer-init-buffer ((this slack-pinned-items-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-pinned-items-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-pinned-items-buffer-insert-items this))
    buf))

(defun slack-create-pinned-items-buffer (room team items)
  (slack-if-let* ((buf (slack-buffer-find 'slack-pinned-items-buffer team room)))
      (progn
        (oset buf items items)
        buf)
    (slack-pinned-items-buffer :room-id (oref room id)
                               :team-id (oref team id)
                               :items items)))

(cl-defmethod slack-buffer--replace ((this slack-pinned-items-buffer) ts)
  (with-slots (items) this
    (slack-if-let* ((message (cl-find-if #'(lambda (m) (string= ts (slack-ts m)))
                                         items)))
        (slack-buffer-replace this message))))


(provide 'slack-pinned-items-buffer)
;;; slack-pinned-items-buffer.el ends here
