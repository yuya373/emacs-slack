;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-message)
(require 'slack-message-editor)

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (id :initarg :id)
   (inviter :initarg :inviter)))

(defclass slack-reply-broadcast-message (slack-user-message)
  ((broadcast-thread-ts :initarg :broadcast_thread_ts :initform nil)))

(cl-defmethod slack-message-sender-id ((m slack-user-message))
  (oref m user))

(cl-defmethod slack-thread-message-p ((_this slack-reply-broadcast-message))
  t)

(defvar slack-user-message-keymap
  (let ((keymap (make-sparse-keymap)))
    keymap))

(cl-defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(cl-defmethod slack-message-header ((m slack-user-message) team)
  (with-slots (ts deleted-at) m
    (let* ((name (slack-message-sender-name m team))
           (status (slack-user-status (slack-message-sender-id m) team))
           (time (slack-message-time-to-string ts))
           (edited-at (slack-message-time-to-string (slack-message-edited-at m)))
           (deleted-at (slack-message-time-to-string deleted-at))
           (header (or (and status
                            (not (slack-string-blankp status))
                            (format "%s %s" name status))
                       (format "%s" name))))
      (if deleted-at
          (format "%s deleted_at: %s" header deleted-at)
        (if edited-at
            (format "%s edited_at: %s" header edited-at)
          header)))))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
