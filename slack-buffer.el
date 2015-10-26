(defface slack-output-text
  '((t (:weight normal :height 0.9)))
  "Face used to text message."
  :group 'slack-buffer)

(defface slack-output-header
  '((t (:weight bold :height 1.0 :underline t)))
  "Face used to text message."
  :group 'slack-buffer)

(defun slack-message-name-and-text (message)
  (let ((user-id (gethash "user" message))
        (bot-id (gethash "bot_id" message)))
    (if user-id
        (cl-values (slack-user-name user-id) (gethash "text" message))
      (cl-values (slack-bot-name bot-id) (gethash "fallback" (aref (gethash "attachments" message) 0))))))

(defun slack-buffer-message-name (message)
  (let ((user-id (gethash "user" message)))
    (if user-id
        (slack-user-name user-id)
      (let ((bot-id (gethash "bot_id" message)))
        (slack-bot-name bot-id)))))

(defun slack-buffer-message-text (message)
  (let ((user-id (gethash "user" message)))
    (if user-id
        (gethash "text" message)
      (gethash "fallback"
               (aref (gethash "attachments" message) 0)))))


(defun slack-buffer---insert-message-reactions (reactions)
  (when reactions
    (cl-labels ((insert-reaction
                 (r)
                 (insert (format "\s\s%s/%d"
                                 (gethash "name" r)
                                 (gethash "count" r)))))
      (mapc #'insert-reaction reactions))))

(defun slack-buffer---insert-message-text (text)
  (put-text-property 0 (length text)
                     'face 'slack-output-text
                     text)
  (insert (format "%s\n" text)))

(defun slack-buffer---insert-message-header (name time)
  (let ((text (concat name "\s\s\s\s" time)))
    (put-text-property 0 (length text)
                       'face 'slack-output-header text)
    (insert text)))

(defun slack-buffer--insert-message (msg)
  (let ((name (slack-buffer-message-name msg))
        (text (slack-buffer-message-text msg))
        (reactions (gethash "reactions" msg))
        (type (gethash "type" msg))
        (time (slack-buffer-time-to-string
               (gethash "ts" msg))))
    (when (and (string= type "message") name text)
      (slack-buffer---insert-message-header name time)
      (slack-buffer---insert-message-reactions reactions)
      (insert (format "\n"))
      (slack-buffer---insert-message-text text)
      (insert (format "\n")))))


(defun slack-buffer--insert-messages (messages)
  (mapc #'slack-buffer--insert-message
        (reverse messages)))

(defun slack-buffer-make-id (room)
  (set (make-local-variable 'slack-room-id)
       (gethash "id" room)))

(defun slack-buffer-insert-messages (room)
  (let ((messages (gethash "messages" room)))
    (slack-buffer--insert-messages messages)))

(defun slack-buffer-create (buf-name header messages)
  (let ((buffer (get-buffer-create buf-name)))
    (if buffer
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (setq line-spacing 0.5)
          (erase-buffer)
          (slack-mode)
          (slack-buffer-make-id room)
          (insert header)
          (insert "Messages:\n")
          (mapc #'(lambda (m) (insert (slack-message-to-string m)))
                (reverse messages))
          (setq buffer-read-only t)))
    buffer))

(defun slack-buffer-update (buf-name message)
  (let ((buffer (get-buffer buf-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (slack-buffer--insert-message message)
      (goto-char (point-max))
      (setq buffer-read-only t))))

(defun slack-buffer-update-notification (buf-name string)
  (let ((buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (setq line-spacing 0.5)
      (slack-mode)
      (goto-char (point-max))
      (insert string)
      (insert "\n")
      (goto-char (point-max))
      (setq buffer-read-only t))))

(provide 'slack-buffer)
