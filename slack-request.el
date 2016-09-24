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

(require 'json)
(require 'request)

(defcustom slack-request-timeout 5
  "Request Timeout in seconds."
  :group 'slack)

(defun slack-parse-to-hash ()
  (let ((json-object-type 'hash-table))
    (let ((res (json-read-from-string (buffer-string))))
      res)))

(defun slack-parse-to-plist ()
  (let ((json-object-type 'plist))
    (json-read)))

(defun slack-request-parse-payload (payload)
  (let ((json-object-type 'plist))
    (json-read-from-string payload)))

(cl-defun slack-request (url team &key
                             (type "GET")
                             (success)
                             (error nil)
                             (params nil)
                             (parser #'slack-parse-to-plist)
                             (sync t)
                             (files nil)
                             (headers nil)
                             (timeout slack-request-timeout))
  (request
   url
   :type type
   :sync sync
   :params (cons (cons "token" (oref team token))
                 params)
   :files files
   :headers headers
   :parser parser
   :success success
   :error error
   :timeout timeout))

(cl-defmacro slack-request-handle-error ((data req-name) &body body)
  "Bind error to e if present in DATA."
  `(if (eq (plist-get ,data :ok) :json-false)
       (message "Failed to request %s: %s"
                ,req-name
                (plist-get ,data :error))
     (progn
       ,@body)))

(provide 'slack-request)
;;; slack-request.el ends here
