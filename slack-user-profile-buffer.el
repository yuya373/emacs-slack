;;; slack-user-profile-buffer.el ---                 -*- lexical-binding: t; -*-

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
(require 'slack-buffer)

(defclass slack-user-profile-buffer (slack-buffer)
  ((user-id :initarg :user-id :type string)))

(defmethod slack-buffer-name ((this slack-user-profile-buffer))
  (with-slots (user-id team) this
    (format "%s %s" (call-next-method) (slack-user-name user-id team))))

(defmethod slack-buffer-init-buffer ((this slack-user-profile-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-info-mode)
      (setq slack-current-buffer this)
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (goto-char (point-min))
        (with-slots (user-id team) this
          (insert (slack-user-profile-to-string user-id team)))
        (setq buffer-read-only t)
        (slack-buffer-enable-emojify)
        (goto-char (point-min))))
    buf))

(defmethod slack-buffer-display-im ((this slack-user-profile-buffer))
  (with-slots (user-id team) this
    (let ((im (slack-im-find-by-user-id user-id team)))
      (slack-room-display im team))))

(defun slack-create-user-profile-buffer (team user-id)
  (slack-user-profile-buffer :team team
                             :user-id user-id))



(provide 'slack-user-profile-buffer)
;;; slack-user-profile-buffer.el ends here
