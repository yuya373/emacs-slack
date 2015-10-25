(defun slack-parse-to-hash ()
  (let ((json-object-type 'hash-table))
    (let ((res (json-read-from-string (buffer-string))))
      res)))

(defun debug (&key data error-thrown &allow-other-keys &rest _)
  (message "data: %s" data)
  (message "error: %s" error-thrown))

(cl-defun slack-request (url &key
                             (success #'debug)
                             (error #'debug)
                             (params nil)
                             (parser #'slack-parse-to-hash)
                             (sync t))
  (request
   url
   :sync sync
   :params params
   :parser parser
   :success success
   :error error))

(provide 'slack-request)
