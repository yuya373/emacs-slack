;;; slack-dnd-status.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019

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
(require 'slack-log)
(require 'slack-request)
(require 'slack-team)

(defconst slack-dnd-team-info-url "https://slack.com/api/dnd.teamInfo")

(defclass slack-dnd-status ()
  ((dnd-enabled :initarg :dnd_enabled :initform nil :type boolean)
   (next-dnd-start-ts :initarg :next_dnd_start_ts :initform nil :type (or null number))
   (next-dnd-end-ts :initarg :next_dnd_end_ts :initform nil :type (or null number))))

(defun slack-create-dnd-status (payload)
  (make-instance 'slack-dnd-status
                 :dnd_enabled (eq t (plist-get payload :dnd_enabled))
                 :next_dnd_start_ts (plist-get payload :next_dnd_start_ts)
                 :next_dnd_end_ts (plist-get payload :next_dnd_end_ts)))


(cl-defmethod slack-dnd-in-range-p ((this slack-dnd-status))
  (with-slots (dnd-enabled next-dnd-start-ts next-dnd-end-ts) this
    (when dnd-enabled
      (let ((current (time-to-seconds)))
        (and (<= next-dnd-start-ts current)
             (<= current next-dnd-end-ts))))))

(defun slack-dnd-status-team-info (team &optional after-success)
  (cl-labels
      ((on-success
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-dnd-status-team-info")
         (let* ((users (plist-get data :users))
                (statuses (make-hash-table :test 'equal)))
           (slack-plist-each users
               (puthash key (slack-create-dnd-status value) statuses))
           (oset team dnd-status statuses)))
        (when (functionp after-success)
          (funcall after-success team))))
    (let ((user-ids (let ((result))
                      (cl-loop for im in (slack-team-ims team)
                               do (when (slack-room-open-p im)
                                    (push (oref im user) result)))
                      result)))
      (when (< 0 (length user-ids))
        (slack-request
         (slack-request-create
          slack-dnd-team-info-url
          team
          :params (list (cons "users" (mapconcat #'identity user-ids ",")))
          :success #'on-success))))))

(provide 'slack-dnd-status)
;;; slack-dnd-status.el ends here
