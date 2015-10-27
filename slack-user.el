(defvar slack-users)
(defvar slack-user-name nil)

(defun slack-user-find (id)
  (find-if (lambda (user)
             (string= id (gethash "id" user)))
           slack-users))

(defun slack-user-find-by-name (name)
  (find-if (lambda (user)
             (string= name (gethash "name" user)))
           slack-users))

(defun slack-user-name (id)
  (let ((user (slack-user-find id)))
    (if user
        (gethash "name" user))))

(defun slack-my-user-id ()
  (gethash "id"
           (slack-user-find-by-name slack-user-name)))

(defun slack-user-names ()
  (mapcar (lambda (u) (gethash "name" u))
          slack-users))

(provide 'slack-user)
