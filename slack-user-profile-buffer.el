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
(require 'slack-user)
(require 'slack-im)
(require 'slack-image)

(defvar slack-open-direct-message-keymap)
(defvar slack-completing-read-function)
(define-derived-mode slack-user-profile-buffer-mode slack-buffer-mode "Slack User Profile")

(defclass slack-user-profile-buffer (slack-buffer)
  ((user-id :initarg :user-id :type string)))

(defun slack-create-user-profile-buffer (team user-id)
  "Create User Profile Buffer of USER-ID in TEAM."
  (slack-if-let* ((buf (slack-buffer-find 'slack-user-profile-buffer
                                          user-id team)))
      buf
    (slack-user-profile-buffer :team team
                               :user-id user-id)))

(cl-defmethod slack-buffer-buffer ((this slack-user-profile-buffer))
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (slack-buffer--insert this)
        buf)
    (slack-buffer-init-buffer this)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-user-profile-buffer)) user-id team)
  (format "*Slack - %s : Profile - %s*" (oref team name) (slack-user-name user-id team)))

(cl-defmethod slack-buffer-name ((this slack-user-profile-buffer))
  (with-slots (user-id team) this
    (slack-buffer-name 'slack-user-profile-buffer
                       user-id
                       team)))

(cl-defmethod slack-buffer--insert ((this slack-user-profile-buffer))
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

(cl-defmethod slack-buffer-init-buffer ((this slack-user-profile-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-user-profile-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (slack-buffer--insert this)
    buf))

(cl-defmethod slack-buffer--replace ((this slack-user-profile-buffer) _ts)
  (with-current-buffer (current-buffer)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (slack-buffer--insert this))))

(defun slack-user-profile-to-string (id team)
  "Print user's profile according to ID in TEAM."
  (let* ((user (slack-user--find id team))
         (image (slack-image-string (list (slack-user-image-url user 512)
                                          nil nil nil (window-width
                                                       (get-buffer-window
                                                        (current-buffer))
                                                       t))
                                    nil t))
         (profile (slack-user-profile user))
         (header (propertize (slack-user-header user)
                             'face 'slack-user-profile-header-face))
         (presence (slack-user-property-to-str (plist-get user :presence) "Presence"))
         (status (slack-user-property-to-str (slack-user-status id team) "Status"))
         (timezone (slack-user-property-to-str (slack-user-timezone user) "Timezone"))
         (email (slack-user-property-to-str (plist-get profile :email) "Email"))
         (phone (slack-user-property-to-str (plist-get profile :phone) "Phone"))
         (skype (slack-user-property-to-str (plist-get profile :skype) "Skype"))
         (body (mapconcat #'identity
                          (cl-remove-if #'null
                                        (list presence status timezone email phone skype))
                          "\n"))
         (dm-button (propertize "[Open Direct Message]"
                                'face '(:underline t)
                                'keymap slack-open-direct-message-keymap)))
    (format "\n%s\n\n%s%s\n%s\n\n%s" image header (format "  (%s)" id) body dm-button)))

(defun slack-user-select ()
  "Select user from team, then display the user's profile."
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-user-name-alist
                 team
                 :filter #'(lambda (users)
                             (cl-remove-if #'slack-user-hidden-p users)))))
    (slack-select-from-list (alist "Select User: ")
        (let ((buf (slack-create-user-profile-buffer team (plist-get selected :id))))
          (slack-buffer-display buf)))))

(provide 'slack-user-profile-buffer)
;;; slack-user-profile-buffer.el ends here
