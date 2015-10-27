(defun slack-parse-to-hash ()
  (let ((json-object-type 'hash-table))
    (let ((res (json-read-from-string (buffer-string))))
      res)))

(defun slack-parse-to-plist ()
  (let ((json-object-type 'plist))
    (json-read)))

(cl-defun slack-request (url &key
                             (success)
                             (error nil)
                             (params nil)
                             (parser #'slack-parse-to-plist)
                             (sync t))
  (request
   url
   :sync sync
   :params params
   :parser parser
   :success success
   :error error))

(provide 'slack-request)















