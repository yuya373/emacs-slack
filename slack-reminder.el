;;; slack-reminder.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-room)
(require 'slack-team)
(require 'slack-request)

(defconst slack-reminder-list-url "https://slack.com/api/reminders.list")
(defconst slack-reminder-delete-url "https://slack.com/api/reminders.delete")
(defconst slack-reminder-complete-url "https://slack.com/api/reminders.complete")
(defconst slack-reminder-info-url "https://slack.com/api/reminders.info")

(defclass slack-reminder-base ()
  ((id :initarg :id :type string)
   (creator :initarg :creator :type string)
   (user :initarg :user :type string)
   (text :initarg :text :type string)))

(defclass slack-recurring-reminder (slack-reminder-base)
  ())

(defclass slack-reminder (slack-reminder-base)
  ((time :initarg :time :type integer)
   (complete-ts :initarg :complete_ts :type integer)))

(defmethod slack-reminder-user ((r slack-reminder-base) team)
  (slack-user-find r team))

(defmethod slack-reminder-creator ((r slack-reminder-base) team)
  (slack-user--find (oref r creator) team))

(defmethod slack-reminder-completedp ((r slack-reminder))
  (not (eq 0 (oref r complete-ts))))

(defmethod slack-reminder-completedp ((_r slack-recurring-reminder))
  nil)

(defun slack-reminder-create (payload)
  (let ((klass (if (eq :json-false (plist-get payload :recurring))
                   'slack-reminder
                 'slack-recurring-reminder)))
    (apply #'make-instance klass
           (slack-collect-slots klass payload))))

(defmethod slack-reminder-to-body ((r slack-reminder))
  (with-slots (text time complete-ts) r
    (let ((time-str (format "Remind At: %s"
                            (slack-message-time-to-string
                             (number-to-string time))))
          (completed (format "Completed: %s"
                             (if (eq complete-ts 0)
                                 "Not Yet"
                               (slack-message-time-to-string
                                (number-to-string complete-ts))))))
      (format "%s\n%s\n\n%s" time-str completed text))))

(defmethod slack-reminder-to-body ((r slack-recurring-reminder))
  (oref r text))

(defmethod slack-reminder-to-string ((r slack-reminder-base) team)
  (with-slots (creator user) r
    (let* ((header (slack-message-put-header-property
                    (format "From: %s To: %s"
                            (slack-user-name creator team)
                            (slack-user-name user team))))
           (body (slack-reminder-to-body r)))
      (format "%s\n%s\n\n" header body))))

(defmethod slack-create-reminder-buffer ((team slack-team))
  (let* ((buf-name "*Slack - Reminders*")
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (with-slots (reminders) team
        (cl-loop for reminder in reminders
                 do (insert (slack-reminder-to-string reminder team))))
      (setq buffer-read-only t))
    buf))

(defmethod slack-reminder-sort-key ((r slack-reminder))
  (oref r time))

(defmethod slack-reminder-sort-key ((r slack-recurring-reminder))
  0)

(defun slack-reminder-sort (team)
  (with-slots (reminders) team
    (setq reminders
          (cl-sort reminders #'<
                   :key #'(lambda (r) (slack-reminder-sort-key r))))))

(defun slack-reminder-list ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels
        ((on-reminder-list
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-reminder-list")
           (oset team reminders
                 (cl-loop
                  for payload in (slack-decode
                                  (append (plist-get data :reminders)
                                          nil))
                  collect (slack-reminder-create payload)))
           (slack-reminder-sort team)
           (if (< 0 (length (oref team reminders)))
               (funcall
                slack-buffer-function
                (slack-create-reminder-buffer team))
             (message "No Reminders!")))))
      (slack-request
       (slack-request-create
        slack-reminder-list-url
        team
        :success #'on-reminder-list)))))

(defmethod slack-reminders-alist ((team slack-team) &optional filter)
  (cl-labels ((text (r)
                    (with-slots (creator user text) r
                      (format "Creator: %s Target: %s Content: %s"
                              (slack-user-name creator team)
                              (slack-user-name user team)
                              text))))
    (with-slots (reminders) team
      (mapcar #'(lambda (r) (cons (text r) r))
              (if filter
                  (cl-remove-if-not #'(lambda (r) (funcall filter r))
                                    reminders)
                reminders)))))

(defmethod slack-team-delete-reminder ((team slack-team) r)
  (with-slots (reminders) team
    (setq reminders
          (cl-remove-if #'(lambda (e)
                            (string= (oref e id) (oref r id)))
                        reminders))))

(defun slack-reminder-select (team &optional filter)
  (slack-select-from-list
      ((slack-reminders-alist team filter) "Select: ")))

(defun slack-reminder-delete ()
  (interactive)
  (let* ((team (slack-team-select))
         (reminder (slack-reminder-select team)))
    (cl-labels
        ((on-reminder-delete (&key data &allow-other-keys)
                             (slack-request-handle-error
                              (data "slack-reminder-delete")
                              (slack-team-delete-reminder team reminder)
                              (message "Reminder Deleted!"))))
      (slack-request
       (slack-request-create
        slack-reminder-delete-url
        team
        :params (list (cons "reminder" (oref reminder id)))
        :success #'on-reminder-delete)))))

(defmethod slack-reminder-info ((r slack-reminder-base) team callback)
  (cl-labels
      ((on-reminder-info (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-reminder-info")
                          (let ((reminder (slack-reminder-create
                                           (plist-get (slack-decode data)
                                                      :reminder))))
                            (funcall callback reminder)))))
    (slack-request
     (slack-request-create
      slack-reminder-info-url
      team
      :params (list (cons "reminder" (oref r id)))
      :success #'on-reminder-info))))

(defmethod slack-reminder-refresh ((r slack-reminder-base) team)
  (slack-reminder-info
   r team
   #'(lambda (reminder)
       (with-slots (reminders) team
         (setq reminders
               (cl-remove-if #'(lambda (e) (string= (oref e id)
                                                    (oref reminder id)))
                             reminders))
         (push reminder reminders))
       (message "Reminder Updated!"))))

(defun slack-reminder-complete ()
  (interactive)
  (let* ((team (slack-team-select))
         (reminder (slack-reminder-select
                    team
                    #'(lambda (r)
                        (not (slack-reminder-completedp r))))))
    (cl-labels
        ((on-reminder-complete (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-reminder-complete")
                                (slack-reminder-refresh reminder team))))
      (slack-request
       (slack-request-create
        slack-reminder-complete-url
        team
        :params (list (cons "reminder" (oref reminder id)))
        :success #'on-reminder-complete)))))

(defmethod slack-user-find ((r slack-reminder-base) team)
  (slack-user--find (oref r user) team))

(provide 'slack-reminder)
;;; slack-reminder.el ends here
