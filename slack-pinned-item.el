;;; slack-pinned-item.el ---                         -*- lexical-binding: t; -*-

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

(defclass slack-pinned-item ()
  ((message :initarg :message)))

(defun slack-pinned-item-create (message)
  (slack-pinned-item :message message))

(defmethod slack-ts ((this slack-pinned-item))
  (slack-ts (oref this message)))

(defmethod slack-message-to-string ((this slack-pinned-item) team)
  (with-slots (message) this
    (cond
     ((object-of-class-p message 'slack-file-comment)
      (format "%s%s"
              (slack-message-to-string message team)
              (slack-file-link-info (oref message file-id) "\n(more info)\n")))
     (t
      (slack-message-to-string message team)))))

(provide 'slack-pinned-item)
;;; slack-pinned-item.el ends here
