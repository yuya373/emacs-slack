(defvar slack-ws-url nil)
(defvar slack-ws nil)

(defun slack-ws-open (slack-ws-url)
  (unless slack-ws
    (setq slack-ws (websocket-open
                   slack-ws-url
                   :on-message #'slack-ws-on-message))))

(defun slack-ws-close ()
  (interactive)
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
    (let* ((json-object-type 'plist)
           (payload (json-read-from-string
                     (websocket-frame-payload frame)))
           (type (plist-get payload :type)))
      (cond
       ((string= type "hello")
        (message "Slack Websocket Is Ready!"))
       ((string= type "message")
        (slack-ws-handle-message payload))
       ((plist-get payload :reply_to)
        (slack-ws-handle-reply payload))))))

(defun slack-ws-handle-message (payload)
  (let ((m (slack-message-create payload)))
    (if m
        (progn
          (oset m text (slack-ws-decode-string (oref m text)))
          (slack-message-update m)))))

(defun slack-ws-handle-reply (payload)
  (let ((ok (plist-get payload :ok))
        (e (plist-get payload :error)))
    (if ok
        (slack-message-handle-reply
         (slack-message-create payload))
      (error "code: %s msg: %s"
             (plist-get :code e)
             (plist-get :msg e)))))

(defun slack-ws-handle-attachments-message (attachments payload)
  (let* ((attachment (aref attachments 0))
         (title (gethash "title" attachment))
         (text (gethash "text" attachment))
         (pretext (gethash "pretext" attachment))
         (fallback (gethash "fallback" attachment)))
    (if (or text pretext fallback)
        (progn
          (puthash "text"
                   (slack-ws-decode-string
                    (if (> (length text) 0)
                        (concat title "\n" pretext "\n" text)
                      fallback))
                   payload)
          (slack-message-update payload)))))

(defun slack-ws-handle-user-message (payload)
  (let ((text (gethash "text" payload)))
    (if text
        (progn
          (puthash "text" (slack-ws-decode-string text) payload)
          (slack-message-update payload)))))

(defun slack-ws-decode-string (text)
  (decode-coding-string text 'utf-8-unix))

(provide 'slack-websocket)
