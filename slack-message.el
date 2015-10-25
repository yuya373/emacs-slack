(defvar slack-message-id 0)
(defvar slack-sent-message ())
(defvar slack-message-minibuffer-local-map nil)

(defun slack-message-set (room messages)
  (puthash "messages" (append messages nil) room))

(defun slack-message-find (room message)
  (let ((messages (gethash "messages" room)))
    (find-if #'(lambda (m) (slack-message-equal m message))
             messages)))

(defun slack-message-equal (m n)
  (and (string= (gethash "name" m) (gethash "name" n))
       (string= (gethash "ts" m) (gethash "ts" n))
       (string= (gethash "text" m) (gethash "text" n))))

(defun slack-message-update (message)
  (let ((room (slack-message-find-room message)))
    (slack-message-popup-tip room message)
    (if (and room (gethash "messages" room))
        (unless (slack-message-find room message)
          (setcar (gethash "messages" room) message)
          (slack-buffer-update
           (slack-message-get-buffer-name room
                                          message)
           message)))))

(defun slack-message-find-room-type (msg)
  (let* ((channel (gethash "channel" msg))
         (initial (substring channel 0 1)))
    (cond
     ((string= "G" initial) 'group)
     ((string= "D" initial) 'im)
     (t nil))))

(defun slack-message-find-room (msg)
  (let ((type (slack-message-find-room-type msg))
        (channel (gethash "channel" msg)))
    (case type
      ('group (slack-group-find channel))
      ('im (slack-im-find channel)))))

(defun slack-message-get-buffer-name (room msg)
  (let ((type (slack-message-find-room-type msg)))
    (case type
      ('group (slack-group-get-buffer-name room))
      ('im (slack-im-get-buffer-name room)))))

(defun slack-message-popup-tip (room message)
  (let ((sender-id (gethash "user" message)))
    (unless (string= sender-id (slack-user-my-id))
      (popup-tip (slack-message-to-string room message)))))


(defun slack-message-to-string (room message)
  (let* ((room-name (if room
                        (gethash "name" room)
                      "Unkown Room"))
         (sender-id (gethash "user" message))
         (sender-name (gethash "name"
                               (slack-user-find sender-id)))
         (text (gethash "text" message)))
    (concat "Incoming Message:\s" room-name "\n"
            "From:\s" sender-name "\n"
            text)))

(defun slack-message-send ()
  (interactive)
  (let* ((json (json-encode
                (list :id slack-message-id
                      :channel (slack-message-get-room-id)
                      :type "message"
                      :user (slack-user-my-id)
                      :text (slack-message-read-from-minibuffer))))
         (json-object-type 'hash-table)
         (hash (json-read-from-string (format "%s" json))))
    (incf slack-message-id)
    (slack-ws-send json)
    (push hash slack-sent-message)))

(defun slack-message-get-room-id ()
  (if (boundp 'slack-room-id)
      slack-room-id
    (let* ((room-name (slack-message-read-room-list))
           (room (slack-message-find-room-by-name room-name)))
      (unless room
        (error "Slack Room Not Found: %s" room-name))
      (gethash "id" room))))

(defun slack-message-read-room-list ()
  (let ((completion-ignore-case t)
        (choices (slack-message-room-list)))
    (completing-read "Select Room"
                     choices nil t nil nil choices)))

(defun slack-message-room-list ()
  (append (slack-group-names) (slack-im-names)))

(defun slack-message-find-room-by-name (name)
  (or (slack-group-find-by-name name)
      (slack-im-find-by-name name)))

(defun slack-message-handle-reply (payload)
  (let* ((id (gethash "reply_to" payload))
         (msg (slack-message-find-sent id)))
    (if msg
        (progn
          (puthash "ts" (gethash "ts" payload) msg)
          (slack-message-update msg)))))

(defun slack-message-find-sent (id)
  (find-if #'(lambda (m)
               (eq id (gethash "id" m)))
           slack-sent-message))

(defun slack-message-read-from-minibuffer ()
  (let ((prompt "Message: "))
    (slack-message-setup-minibuffer-keymap)
    (read-from-minibuffer
     prompt
     nil
     slack-message-minibuffer-local-map)))

(defun slack-message-setup-minibuffer-keymap ()
  (unless slack-message-minibuffer-local-map
    (setq slack-message-minibuffer-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'newline)
            (set-keymap-parent map minibuffer-local-map)
            map))))

(defun slack-update-reply (payload)
  (let* ((reply-to (gethash "reply_to" payload))
         (timestamp (gethash "ts" payload))
         (msg (slack-find-sent-message reply-to)))
    (if msg
        (progn
          (puthash "ts" timestamp msg)
          (slack-message-update msg)))))

(provide 'slack-message)
