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
(require 'slack-util)
(require 'slack-buffer)

(define-derived-mode slack-user-profile-buffer-mode slack-buffer-mode "Slack User Profile")

(defclass slack-user-profile-buffer (slack-buffer)
  ((user-id :initarg :user-id :type string)))

(defun slack-create-user-profile-buffer (team user-id)
  (slack-if-let* ((buf (slack-buffer-find 'slack-user-profile-buffer
                                    user-id team)))
      buf
    (slack-user-profile-buffer :team team
                               :user-id user-id)))

(defmethod slack-buffer-buffer ((this slack-user-profile-buffer))
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (slack-buffer--insert this)
        buf)
    (slack-buffer-init-buffer this)))

(defmethod slack-buffer-name :static ((class slack-user-profile-buffer) user-id team)
  (format "*Slack - %s : Profile - %s*" (oref team name) (slack-user-name user-id team)))

(defmethod slack-buffer-name ((this slack-user-profile-buffer))
  (with-slots (user-id team) this
    (slack-buffer-name 'slack-user-profile-buffer
                       user-id
                       team)))

(defmethod slack-buffer--insert ((this slack-user-profile-buffer))
  (let ((buf (get-buffer (slack-buffer-name this))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (goto-char (point-min))
        (with-slots (user-id team) this
          (insert (propertize (slack-user-profile-to-string user-id team)
                              'ts 'dummy)))
        (setq buffer-read-only t)
        (slack-buffer-enable-emojify)
        (goto-char (point-min))
        (slack-display-image)))))

(defmethod slack-buffer-init-buffer ((this slack-user-profile-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-user-profile-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (slack-buffer--insert this)
    buf))

(defmethod slack-buffer-display-im ((this slack-user-profile-buffer))
  (with-slots (user-id team) this
    (let ((im (slack-im-find-by-user-id user-id team)))
      (slack-room-display im team))))

(defmethod slack-buffer--replace ((this slack-user-profile-buffer) _ts)
  (with-current-buffer (current-buffer)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (slack-buffer--insert this))))



(provide 'slack-user-profile-buffer)
;;; slack-user-profile-buffer.el ends here
