;;; slack-reaction.el ---  deal with reactions       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'slack-user)

(defclass slack-reaction ()
  ((name :initarg :name :type string)
   (count :initarg :count :type integer)
   (users :initarg :users :initform ())
   (user-loading :type boolean :initform nil)))

(cl-defmethod slack-reaction-join ((r slack-reaction) other)
  (if (string= (oref r name) (oref other name))
      (progn
        (cl-incf (oref r count))
        (oset r users (append (oref other users) (oref r users)))
        r)))

(cl-defmethod slack-equalp ((r slack-reaction) other)
  (slack-reaction-equalp r other))

(cl-defmethod slack-reaction-equalp ((r slack-reaction) other)
  (string= (oref r name) (oref other name)))

(cl-defmethod slack-reaction-fetch-users ((this slack-reaction) team cb)
  (cl-labels
      ((collect-users (user-ids)
                      (cl-remove-if #'null
                                    (mapcar #'(lambda (id) (slack-user--find id team))
                                            user-ids))))
    (let* ((users (collect-users (oref this users)))
           (user-loaded-p (<= (length (oref this users))
                              (length users))))
      (if user-loaded-p (funcall cb users)
        (progn
          (unless (oref this user-loading)
            (oset this user-loading t)
            (slack-users-info-request (slack-team-missing-user-ids  team (oref this users))
                                      team
                                      :after-success
                                      #'(lambda ()
                                          (oset this user-loading nil)
                                          (funcall cb (collect-users (oref this users)))))))))))

(cl-defmethod slack-reaction-help-text ((r slack-reaction) team cb)
  (slack-reaction-fetch-users r team #'(lambda (users)
                                         (let ((user-names (mapcar #'(lambda (user)
                                                                       (slack-user--name user team))
                                                                   users)))
                                           (funcall cb
                                                    (format "%s reacted with :%s:"
                                                            (mapconcat #'identity user-names ", ")
                                                            (oref r name)))))))

(defun slack-reaction--find (reactions reaction)
  (cl-find-if #'(lambda (e) (slack-reaction-equalp e reaction))
              reactions))

(cl-defmethod slack-reaction-delete ((this slack-reaction) reactions)
  (cl-delete-if #'(lambda (e) (slack-reaction-equalp e this))
                reactions))

(cl-defmethod slack-reaction-remove-user ((this slack-reaction) user-id)
  (with-slots (count users) this
    (setq users (cl-remove-if #'(lambda (old-user) (string= old-user user-id))
                              users))
    (setq count (length users))))

(cl-defmethod slack-merge ((old slack-reaction) new)
  (with-slots (count users) old
    (setq count (oref new count))
    (setq users (cl-remove-duplicates (append users (oref new users))
                                      :test #'string=))))

(provide 'slack-reaction)
;;; slack-reaction.el ends here

