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
(require 'slack-util)

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
  ((response :initform nil)
   (url :initarg :url)
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
   (timeout :initarg :timeout :initform `,slack-request-timeout)
   (execute-at :initform 0.0 :type float)))

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

(defmethod slack-equalp ((this slack-request-request) other)
  (and (string= (oref (oref this team) id)
                (oref (oref other team) id))
       (string= "GET" (oref this type))
       (string= "GET" (oref other type))
       (string= (oref this url)
                (oref other url))
       (equalp (oref this params)
               (oref other params))))

(defmethod slack-request-retry-request ((req slack-request-request) retry-after team)
  (oset req execute-at (+ retry-after (time-to-seconds)))
  (slack-request-worker-push req))

(cl-defmethod slack-request ((req slack-request-request)
                             &key (on-success nil) (on-error nil))
  (let ((team (oref req team)))
    (with-slots (url type success error params data parser sync files headers timeout) req
      (cl-labels
          ((on-success (&key data &allow-other-keys)
                       (funcall success :data data)
                       (slack-log
                        (format "Request Finished. URL: %s, PARAMS: %s"
                                url
                                params)
                        team :level 'trace)
                       (when (functionp on-success)
                         (funcall on-success)))
           (on-error (&key error-thrown symbol-status response data)
                     (slack-if-let* ((retry-after (request-response-header response "retry-after"))
                                     (retry-after-sec (string-to-number retry-after)))
                         (progn
                           (slack-request-retry-request req retry-after-sec team)
                           (slack-log (format "Retrying Request After: %s second, URL: %s, PARAMS: %s"
                                              retry-after-sec
                                              url
                                              params)
                                      team))
                       (slack-log (format "Request Failed. URL: %s, PARAMS: %s, ERROR-THROWN: %s, SYMBOL-STATUS: %s, DATA: %s"
                                          url
                                          params
                                          error-thrown
                                          symbol-status
                                          data)
                                  team
                                  :level 'error)
                       (when (functionp error)
                         (funcall error
                                  :error-thrown error-thrown
                                  :symbol-status symbol-status
                                  :response response
                                  :data data))
                       (when (functionp on-error)
                         (funcall on-error)))))
        (oset req response
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
