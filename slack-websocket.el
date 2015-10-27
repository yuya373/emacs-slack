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

(setq debug-on-error t)
(defvar slack-frames '())
(defvar slack-unhandled '())
(defun slack-ws-on-message (websocket frame)
  (message "%s" (websocket-frame-payload frame))
  (when (websocket-frame-completep frame)
    (let* ((json-object-type 'plist)
           (payload (json-read-from-string
                     (websocket-frame-payload frame)))
           (type (plist-get payload :type)))
      (push frame slack-frames)
      (if (string= type "hello")
          (message "Slack Websocket Is Ready!")
        (slack-ws-handle-message payload)))))

(defun slack-ws-handle-message (payload)
  (let ((m (slack-message-create payload)))
    (unless m
        (push m slack-unhandled)
     (slack-message-update m))))

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

(defun slack-ws-handle-reply (payload)
  (let ((ok (gethash "ok" payload))
        (e (gethash "error" payload)))
    (if ok
        (slack-message-handle-reply payload)
      (error "code: %s msg: %s"
             (gethash "code" e)
             (gethash "msg" e)))))

(provide 'slack-websocket)
