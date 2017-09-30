;;; slack-request.el ---slack request function       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'json)
(require 'request)

(defcustom slack-request-timeout 10
  "Request Timeout in seconds."
  :group 'slack)

(defun slack-parse ()
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (json-read)))

(defun slack-request-parse-payload (payload)
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (condition-case err-var
        (json-read-from-string payload)
      (json-end-of-file nil))))

(defclass slack-request-request ()
  ((url :initarg :url)
   (team :initarg :team)
   (type :initarg :type :initform "GET")
   (success :initarg :success)
   (error :initarg :error :initform nil)
   (params :initarg :params :initform nil)
   (data :initarg :data :initform nil)
   (parser :initarg :parser :initform #'slack-parse)
   (sync :initarg :sync :initform nil)
   (files :initarg :files :initform nil)
   (headers :initarg :headers :initform nil)
   (timeout :initarg :timeout :initform `,slack-request-timeout)))

(cl-defun slack-request-create
    (url team &key type success error params data parser sync files headers timeout)
  (let ((args (list
               :url url :team team :type type
               :success success :error error
               :params params :data data :parser parser
               :sync sync :files files :headers headers
               :timeout timeout))
        (ret nil))
    (mapc #'(lambda (maybe-key)
              (let ((value (plist-get args maybe-key)))
                (when value
                  (setq ret (plist-put ret maybe-key value)))))
          args)
    (apply #'make-instance 'slack-request-request ret)))

(defmethod slack-request-suspend-request ((req slack-request-request))
  (cl-pushnew req (oref team waiting-requests) :test #'equal))

(defmethod slack-request-retry-request ((req slack-request-request) retry-after)
  (slack-request-suspend-request req)
  (unless (slack-team-request-suspended-p (oref req team))
    (slack-team-run-retry-request-timer (oref req team) retry-after)))

(defmethod slack-request ((req slack-request-request))
  (let ((team (oref req team)))
    (if (slack-team-request-suspended-p team)
        (slack-request-suspend-request req)

      (with-slots (url type success error params data parser sync files headers timeout) req
        (cl-labels
            ((on-success (&key data &allow-other-keys)
                         (funcall success :data data)
                         (oset team retry-after-timer nil))
             (on-error (&key error-thrown symbol-status response data)
                       (let ((retry-after-sec (request-response-header response "retry-after")))
                         (when retry-after-sec
                           (slack-log (format "!!!!retry-after-sec: %s, %s" retry-after-sec (numberp retry-after-sec)) team)
                           ;; (slack-request-retry-request req (string-to-number retry-after-sec))
                           ))

                       (when (functionp error)
                         (funcall error
                                  :error-thrown error-thrown
                                  :symbol-status symbol-status
                                  :response response
                                  :data data))))
          (request
           url
           :type type
           :sync sync
           :params (cons (cons "token" (oref team token))
                         params)
           :data data
           :files files
           :headers headers
           :parser parser
           :success #'on-success
           :error #'on-error
           :timeout timeout))))))


(cl-defmacro slack-request-handle-error ((data req-name &optional handler) &body body)
  "Bind error to e if present in DATA."
  `(if (eq (plist-get ,data :ok) :json-false)
       (if ,handler
           (funcall ,handler (plist-get ,data :error))
         (message "Failed to request %s: %s"
                  ,req-name
                  (plist-get ,data :error)))
     (progn
       ,@body)))

(provide 'slack-request)
;;; slack-request.el ends here
