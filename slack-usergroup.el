;;; slack-user-group.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@archlinux>
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
(require 'slack-request)
(require 'slack-team)

(defconst slack-usergroup-list-url "https://slack.com/api/usergroups.list")

(defclass slack-usergroup ()
  ((id :initarg :id :type string)
   (team-id :initarg :team_id :type string)
   (is-usergroup :initarg :is_usergroup :type boolean)
   (is-subteam :initarg :is_subteam :type boolean)
   (name :initarg :name :type string)
   (description :initarg :description :type string :initform "")
   (handle :initarg :handle :type string)
   (is-external :initarg :is_external :type boolean)
   (date-create :initarg :date_create :type number)
   (date-update :initarg :date_update :type number)
   (date-delete :initarg :date_delete :type number)
   (auto-type :initarg :auto_type :type (or null string))
   (auto-provision :initarg :auto_provision :type boolean)
   (created-by :initarg :created_by :type string)
   (updated-by :initarg :updated_by :type (or null string))
   (deleted-by :initarg :deleted_by :type (or null string))
   (user-count :initarg :user_count :type number)
   (users :initarg :users :type list :initform '())
   (prefs :initarg :prefs)
   ;; "enterprise_subteam_id": "S00",
   ;; "prefs": {
   ;; "channels": [],
   ;; "groups": []
   ;; },
   ))

(defun slack-usergroup-create (usergroup)
  (apply #'make-instance 'slack-usergroup
         (slack-collect-slots 'slack-usergroup usergroup)))

(defun slack-usergroup-list-update (team)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-usergroup-list-request")
                    (let ((usergroups (mapcar #'slack-usergroup-create
                                              (plist-get data :usergroups))))
                      (oset team usergroups usergroups)))))
    (slack-request
     (slack-request-create
      slack-usergroup-list-url
      team
      :success #'on-success
      :params (list (cons "include_count" "true")
                    (cons "include_users" "true"))))))

(defun slack-usergroup-find (id team)
  (cl-find-if #'(lambda (e) (string= id (oref e id)))
              (oref team usergroups)))

(defun slack-usergroup-get-id (handle team)
  (slack-if-let*
      ((group (cl-find-if #'(lambda (e) (string= handle (oref e handle)))
                          (oref team usergroups))))
      (oref group id)))

(cl-defmethod slack-usergroup-deleted-p ((this slack-usergroup))
  (not (eq 0 (oref this date-delete))))

(provide 'slack-usergroup)
;;; slack-usergroup.el ends here
