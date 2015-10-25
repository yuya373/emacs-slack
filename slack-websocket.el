(defvar slack-ws-url nil)
(defvar slack-ws nil)

(defun slack-ws-open (slack-ws-url)
  (unless slack-ws
    (setq slack-ws (websocket-open
                   slack-ws-url
                   :on-message #'slack-ws-on-message))))

(defun slack-ws-close ()
  (if slack-ws
      (progn
        (websocket-close slack-ws)
        (setq slack-ws nil))))

(defun slack-ws-send (payload)
  (let ((frame (make-websocket-frame :opcode 'text
                                     :payload payload
                                     :completep t)))
    (websocket-send slack-ws frame)))

(defun slack-ws-on-message (websocket frame)
  (message "%s" (websocket-frame-payload frame))
  (when (websocket-frame-completep frame)
    (let* ((json-object-type 'hash-table)
           (payload (json-read-from-string
                     (websocket-frame-payload frame)))
           (type (gethash "type" payload)))

      (cond ((string= type "hello")
             (message "Slack Websocket Is Ready!"))
            ((string= type "message")
             (slack-ws-handle-message payload))
            ((slack-ws-replyp payload)
             (slack-ws-handle-reply payload))))))

(defun slack-ws-replyp (payload)
  (and (gethash "ok" payload)
       (eq (1- slack-message-id)
           (gethash "reply_to" payload))))

(defun slack-ws-handle-message (payload)
  (message "slack-ws-handle-message")
  (puthash "text"
           (decode-coding-string
            (gethash "text" payload)
            'utf-8-unix)
           payload)
  (slack-message-update payload))

(defun slack-ws-handle-reply (payload)
  (message "slack-ws-handle-reply")
  (let ((ok (gethash "ok" payload))
        (e (gethash "error" payload)))
    (if ok
        (slack-message-handle-reply payload)
      (error "code: %s msg: %s"
             (gethash "code" e)
             (gethash "msg" e)))))

(provide 'slack-websocket)
