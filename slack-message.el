(defvar slack-message-id 0)
(defvar slack-sent-message ())
(defvar slack-message-minibuffer-local-map nil)
(defvar slack-message-notification-buffer-name "*Slack - notification*")
(defvar slack-message-notification-subscription ())

(require 'eieio)

(defclass slack-message ()
  ((type :initarg :type :type string)
   (room :initarg :room)
   (subtype :initarg :subtype)
   (channel :initarg :channel)
   (ts :initarg :ts :type string)
   (text :initarg :text)
   (item-type :initarg :item_type)
   (attachments :initarg :attachments :type list)
   (reactions :initarg :reactions :type list)
   (is-starred :initarg :is_starred :type boolean)
   (pinned-to :initarg :pinned_to :type list)))

(defclass slack-attachments ()
  ((fallback :initarg :fallback :type string)
   (title :initarg :title)
   (title-link :initarg :title_link)
   (pretext :initarg :pretext)
   (text :initarg :text)
   (author-name :initarg :author_name)
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :type list)
   (image-url :initarg :image_url)
   (thumb-url :initarg :thumb_url)))

(defclass slack-file-message (slack-message)
  ((file :initarg :file)
   ;; (bot-id :initarg :bot_id :type (or null string))
   ;; (username :initarg :username)
   ;; (display-as-bot :initarg :display_as_bot)
   (upload :initarg :upload)
   (user :initarg :user)))

(defclass slack-file ()
  ((id :initarg :id)
   (created :initarg :created)
   (timestamp :initarg :timestamp)
   (name :initarg :name)
   (size :initarg :size)
   (public :initarg :public)
   (url :initarg :url)
   (url-download :initarg :url_download)
   (url-private :initarg :url_private)
   (channels :initarg :channels :type list)
   (groups :initarg :groups :type list)
   (ims :initarg :ims :type list)
   (reactions :initarg :reactions :type list)
   (username :initarg :username)
   (bot-id :initarg :bot_id)
   (ts :initarg :ts :type string)))

(defclass slack-reply (slack-message)
  ((reply-to :initarg :reply_to :type string)))

(defclass slack-bot-message (slack-message)
  ((bot-id :initarg :bot_id :type string)
   (username :initarg :username)
   (icons :initarg :icons)))

(defun slack-message-collect-slots (class m)
  (mapcan #'(lambda (property)
              (if (and (symbolp property)
                       (slot-exists-p
                        class
                        (intern (substring
                                 (symbol-name property) 1))))
                  (list property (plist-get m property))))
          m))

(defun slack-message-create (m &key room)
  (plist-put m :reactions (append (plist-get m :reactions) nil))
  (plist-put m :attachments (append (plist-get m :attachments) nil))
  (plist-put m :pinned_to (append (plist-get m :pinned_to) nil))
  (let ((subtype (plist-get m :subtype)))
    (cond
     ((and subtype (string-prefix-p "file" subtype))
      (apply #'slack-file-message "file-msg" :room room
             (slack-message-collect-slots slack-file-message m)))

     ((plist-member m :user)
      (apply #'slack-user-message "user-msg" :room room
             (slack-message-collect-slots slack-user-message
                                          m)))
     ((plist-member m :bot_id)
      (apply #'slack-bot-message "bot-msg" :room room
             (slack-message-collect-slots slack-bot-message
                                          m)))
     ((plist-member m :reply_to)
      (apply #'slack-reply "reply" :room room
             (slack-message-collect-slots slack-reply
                                          m))))))

(defun slack-message-create-with-room (messages room)
  (mapcar (lambda (m) (slack-message-create m room))
          messages))

(defun slack-message-set (room messages)
  (let ((messages (mapcar #'slack-message-create messages)))
    (puthash "messages" messages room)))

(defun slack-message-find (message messages)
  (let ((messages (gethash "messages" room)))
    (find-if #'(lambda (m) (slack-message-equal message m))
             messages)))

(defun slack-message-equal (m n)
  (and (string= (gethash "name" m) (gethash "name" n))
       (string= (gethash "ts" m) (gethash "ts" n))
       (string= (gethash "text" m) (gethash "text" n))))

(defun slack-message-update (message)
  (let ((room (slack-message-find-room message)))
    (slack-message-notify room message)
    (if room
        (let ((messages (gethash "messages" room)))
          (unless (slack-message-find message
                                      messages)
            (setcar messages message)
            (slack-buffer-update
             (slack-message-get-buffer-name room
                                            message)
             message))))))

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
      (group (slack-group-find channel))
      (im (slack-im-find channel)))))

(defun slack-message-get-buffer-name (room msg)
  (let ((type (slack-message-find-room-type msg)))
    (case type
      (group (slack-group-get-buffer-name room))
      (im (slack-im-get-buffer-name room)))))

(defun slack-message-notify (room message)
  (let ((room-type (slack-message-find-room-type message))
        (sender-id (gethash "user" message)))
        (if (or (eq room-type 'im)
                (null sender-id)
                (not (string= sender-id (slack-user-my-id))))
            (progn
              (slack-message-update-notification-buffer room message)
              (slack-message-popup-tip room message)))))

(defun slack-message-update-notification-buffer (room message)
  (slack-buffer-update-notification
   slack-message-notification-buffer-name
   (slack-message-to-string room message)))

(defun slack-message-popup-tip (room message)
  (let* ((channel (gethash "channel" message))
         (group-name (slack-group-name channel)))
    (if (or (eq 'im (slack-message-find-room-type message))
            (and group-name (memq (intern group-name)
                                  slack-message-notification-subscription)))
        (popup-tip (slack-message-to-string room message)))))

(defmethod slack-message-time-to-string ((m slack-message))
  (format-time-string "%Y-%m-%d %H:%M"
                      (seconds-to-time
                       (string-to-number (oref m 'ts)))))

(defun slack-message-to-popup (m)
  (let* ((room-name (if room (gethash "name" room)))
         (sender-name (or
                       (slack-user-name (gethash "user" message))
                       (slack-bot-name (gethash "bot_id" message))))
         (text (gethash "text" message)))
    (concat "Incoming Message:\s" room-name "\n"
            "From:\s" sender-name "\n" text "\n")))

(defun slack-message-put-header-property (header)
  (put-text-property 0 (length header)
                       'face 'slack-output-header header))

(defun slack-message-put-text-property (text)
  (put-text-property 0 (length text)
                       'face 'slack-output-text text))

(defmethod slack-message-to-string ((m slack-message))
  (let ((ts (slack-message-to-string (oref m 'ts)))
        (text (oref m 'text)))
    (slack-message-put-header-property ts)
    (slack-message-put-text-property text)
    (concat ts "\n" text "\n")))

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
