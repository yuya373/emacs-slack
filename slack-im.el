(defvar slack-ims)
(defvar slack-im-open-url "https://slack.com/api/im.open")
(defvar slack-im-history-url "https://slack.com/api/im.history")
(defvar slack-im-buffer-name "*Slack - Direct Messages*")

(defun slack-im-find (id)
  (let ((im (find-if
             #'(lambda (im) (string= id (gethash "id" im)))
             slack-ims)))
    (unless im (error "slack-im-find failed %s" id))
    im))

(defun slack-im-find-by-name (name)
  (let* ((user (slack-user-find-by-name name))
        (user-id (gethash "id" user)))
    (find-if #'(lambda (im) (string= user-id (gethash "user" im)))
             slack-ims)))

(defun slack-im-get-user-name (im)
  (let ((user (slack-user-find (gethash "user" im))))
    (if user
        (gethash "name" user))))

(defun slack-im-names ()
  (mapcar #'(lambda (im) (slack-im-get-user-name im))
          slack-ims))

(defun slack-im-id (name))

(defun slack-imp (im)
  (let ((id (gethash "id" im)))
    (string-suffix-p "D" id)))


(defun slack-im-push (im)
  (let ((s-im (slack-im-find (gethash "id" im))))
    (unless s-im
      (push im slack-ims))))

(defun slack-im-set-messages (id messages)
  (let ((im (slack-im-find id)))
    (puthash "messages" (append messages nil) im)))

(defun slack-im-history (id)
  (cl-labels ((on-im-history
               (&key data &allow-other-keys)
               (unless (gethash "ok" data)
                 (error "slack-im-history failed %s" data))
               (slack-im-set-messages id
                                      (gethash "messages" data))))
    (slack-request
     slack-im-history-url
     :params (list (cons "token" slack-token)
                   (cons "channel" id))
     :success #'on-im-history)))

(defun slack-im-open (user-name)
  (let* ((user (slack-user-find-by-name user-name))
         (user-id (gethash "id" user)))
    (cl-labels ((on-im-open
                 (&key data &allow-other-keys)
                 (unless (gethash "ok" data)
                   (error "slack-im-open failed"))
                 (slack-im-history
                  (gethash "id"
                           (gethash "channel" data)))))
      (slack-request
       slack-im-open-url
       :params (list (cons "token" slack-token)
                     (cons "user" user-id))
       :success #'on-im-open))))

(defun slack-im-get-buffer-name (im)
  (let ((user-name (slack-im-get-user-name im)))
    (concat slack-im-buffer-name " : " user-name)))

(defun slack-im (user-name)
  (interactive (list (slack-im-read-list
                      "Select User: "
                      (slack-im-names))))
  (let ((im (slack-im-find-by-name user-name)))
    (unless im
      (slack-im-open user-name))
    (slack-im-history (gethash "id" im))
    (switch-to-buffer-other-window
     (slack-buffer-create im
                          (slack-im-get-buffer-name im)))))

(defun slack-im-read-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s" prompt)
                     choices nil t nil nil choices)))

(provide 'slack-im)
