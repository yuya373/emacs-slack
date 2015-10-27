(defun slack-buffer-make-id (room-id)
  (set (make-local-variable 'slack-room-id) room-id))

(defun slack-buffer-create (buf-name room-id header messages)
  (let ((buffer (get-buffer-create buf-name)))
    (if buffer
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (slack-mode)
          (slack-buffer-make-id room-id)
          (insert header)
          (insert "Messages:\n")
          (mapc #'(lambda (m) (insert (slack-message-to-string m)))
                (reverse messages))
          (setq line-spacing 0.5)
          (setq buffer-read-only t)))
    buffer))

(defun slack-buffer-update (buf-name m)
  (let ((buffer (get-buffer buf-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert (slack-message-to-string m))
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
