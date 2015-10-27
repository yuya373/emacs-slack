(defvar slack-message-id 0)
(defvar slack-sent-message ())
(defvar slack-message-minibuffer-local-map nil)
(defvar slack-message-notification-buffer-name "*Slack - notification*")
(defvar slack-message-notification-subscription ())

(defface slack-output-text
  '((t (:weight normal :height 0.9)))
  "Face used to text message."
  :group 'slack-buffer)

(defface slack-output-header
  '((t (:weight bold :height 1.0 :underline t)))
  "Face used to text message."
  :group 'slack-buffer)

(require 'eieio)

(defclass slack-message ()
  ((type :initarg :type :type string)
   (room :initarg :room :initform nil)
   (subtype :initarg :subtype)
   (channel :initarg :channel :initform nil)
   (ts :initarg :ts :type string)
   (text :initarg :text)
   (item-type :initarg :item_type)
   (attachments :initarg :attachments :type (or null list))
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean)
   (pinned-to :initarg :pinned_to :type (or null list))))

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
                       (slot-exists-p class (intern (substring (symbol-name property) 1))))
                  (list property (plist-get m property))))
          m))

(cl-defun slack-message-create (m &key room)
  (plist-put m :reactions (append (plist-get m :reactions) nil))
  (plist-put m :attachments (append (plist-get m :attachments) nil))
  (plist-put m :pinned_to (append (plist-get m :pinned_to) nil))
  (plist-put m :room room)
  (let ((subtype (plist-get m :subtype)))
    (cond
     ((and subtype (string-prefix-p "file" subtype))
      (apply #'slack-file-message "file-msg"
             (slack-message-collect-slots 'slack-file-message m)))
     ((plist-member m :user)
      (apply #'slack-user-message "user-msg"
             (slack-message-collect-slots 'slack-user-message m)))
     ((plist-member m :bot_id)
      (apply #'slack-bot-message "bot-msg"
             (slack-message-collect-slots 'slack-bot-message m)))
     ((plist-member m :reply_to)
      (apply #'slack-reply "reply"
             (slack-message-collect-slots 'slack-reply m))))))

(defun slack-message-create-with-room (messages room)
  (mapcar (lambda (m) (slack-message-create m :room room))
          messages))

(defun slack-message-set (room messages)
  (let ((messages (mapcar #'slack-message-create messages)))
    (puthash "messages" messages room)))

(defmethod slack-message-equal (m n)
  (and (string= (gethash "ts" m) (gethash "ts" n))
       (string= (gethash "text" m) (gethash "text" n))))

(defmethod slack-message-update ((m slack-message))
  (let ((room (or (oref m room) (slack-message-find-room m))))
    (if room
        (progn
          (slack-message-popup-tip m room)
          (slack-message-notify-buffer m room)
          (cl-pushnew (gethash "messages" room) m
                      :test #'slack-message-equal)
          (slack-buffer-update (slack-message-get-buffer-name room)
                               m)))))

(defun slack-message-room-type (msg)
  (let ((channel (oref msg channel)))
    (cond
     ((string-prefix-p "G" channel) 'group)
     ((string-prefix-p "D" channel) 'im)
     (t nil))))

(defun slack-message-find-room (msg)
  (let ((type (slack-message-room-type msg))
        (channel (oref msg channel)))
    (case type
      (group (slack-group-find channel))
      (im (slack-im-find channel)))))

(defun slack-message-get-buffer-name (room)
  (if (slack-imp room)
      (slack-im-get-buffer-name room)
    (slack-group-get-buffer-name room)))

(defmethod slack-message-sender-equalp ((m slack-message) sender-id)
  nil)

(defmethod slack-message-notify-buffer ((m slack-message) room)
  (if (or (slack-imp room)
          (not (slack-message-sender-equalp m (slack-my-user-id))))
      (slack-buffer-update-notification slack-message-notification-buffer-name
                                        (slack-message-to-string m))))

(defmethod slack-message-popup-tip ((m slack-message) room)
  (if (or (slack-imp room)
          (slack-group-subscribedp room))
      (popup-tip (concat (if room (concat (gethash "name" room) "\n"))
                         (slack-message-to-string m)))))

(defmethod slack-message-time-to-string ((m slack-message))
  (format-time-string "%Y-%m-%d %H:%M"
                      (seconds-to-time
                       (string-to-number (oref m ts)))))

(defun slack-message-put-header-property (header)
  (put-text-property 0 (length header)
                       'face 'slack-message-output-header header))

(defun slack-message-put-text-property (text)
  (put-text-property 0 (length text)
                       'face 'slack-message-output-text text))

(defmethod slack-message-to-string ((m slack-message))
  (let ((ts (slack-message-time-to-string m))
        (text (oref m text)))
    (slack-message-put-header-property ts)
    (slack-message-put-text-property text)
    (concat "\n" ts "\n" text "\n")))

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
