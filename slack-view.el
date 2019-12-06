;;; slack-view.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
(require 'slack-block)

(defclass slack-view-state ()
  ((values :initarg :values :type list))) ;; plist (`block-id' `(`action-id' `slack-block-element')') ?

(defclass slack-view ()
  ((id :initarg :id :type string)
   (team-id :initarg :team_id :type string)
   (type :initarg :type :type string) ;; "modal" only?
   (blocks :initarg :blocks :type list) ;; list of slack-layout-block
   (private-metadata  :initarg :private_metadata :type string)
   (callback-id :initarg :callback_id :type string)
   (state :initarg :state :type slack-view-state)
   (hash :initarg :hash :type string)
   (title :initarg :title :type slack-text-message-composition-object)
   (clear-on-close :initarg :clear_on_close :type boolean :initform nil)
   (notify-on-close :initarg :notify_on_close :type boolean :initform nil)
   (close :initarg :close :type (or null slack-text-message-composition-object) :initform nil)
   (submit :initarg :submit :type (or null slack-text-message-composition-object) :initform nil)
   (previous-view-id :initarg :previous_view_id :type (or null string) :initform nil)
   (root-view-id :initarg :root_view_id :type string)
   (app-id :initarg :app_id :type string)
   (external-id :initarg :external_id :type string)
   (app-installed-team-id :initarg :app_installed_team_id :type string)
   (bot-id :initarg :bot_id :type string)))

(defun slack-create-view-state (payload)
  (make-instance 'slack-view-state
                 :values (plist-get payload :values)))

(defun slack-create-view (payload)
  (when payload
    (make-instance 'slack-view
                   :id (plist-get payload :id)
                   :team_id (plist-get payload :team_id)
                   :type (plist-get payload :type)
                   :blocks (mapcar #'slack-create-layout-block (plist-get payload :blocks))
                   :private_metadata (plist-get payload :private_metadata)
                   :callback_id (plist-get payload :callback_id)
                   :state (slack-create-view-state (plist-get payload :state))
                   :hash (plist-get payload :hash)
                   :title (slack-create-text-message-composition-object
                           (plist-get payload :title))
                   :clear_on_close (eq t (plist-get payload :clear_on_close))
                   :notify_on_close (eq t (plist-get payload :notify_on_close))
                   :close (slack-create-text-message-composition-object
                           (plist-get payload :close))
                   :submit (slack-create-text-message-composition-object
                            (plist-get payload :submit))
                   :previous_view_id (plist-get payload :previous_view_id)
                   :root_view_id (plist-get payload :root_view_id)
                   :app_id (plist-get payload :app_id)
                   :external_id (plist-get payload :external_id)
                   :app_installed_team_id (plist-get payload :app_installed_team_id)
                   :bot_id (plist-get payload :bot_id)
                   )))


(provide 'slack-view)
;;; slack-view.el ends here
