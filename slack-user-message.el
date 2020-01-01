;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-message)

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (id :initarg :id)
   (inviter :initarg :inviter)))

(defclass slack-reply-broadcast-message (slack-user-message) ())

(cl-defmethod slack-message-sender-id ((m slack-user-message))
  (oref m user))

(cl-defmethod slack-thread-message-p ((_this slack-reply-broadcast-message))
  t)

(defvar slack-user-message-keymap
  (let ((keymap (make-sparse-keymap)))
    keymap))

(cl-defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(cl-defmethod slack-message-user-status ((this slack-user-message) team)
  (slack-user-status (slack-message-sender-id this)
                     team))

(cl-defmethod slack-user-find ((this slack-user-message) team)
  (let ((user-id (slack-message-sender-id this)))
    (slack-user--find user-id team)))

(cl-defmethod slack-message-profile-image ((m slack-user-message) team)
  (slack-user-image (slack-user-find m team) team))

(cl-defmethod slack-message-display-thread-sign-p ((this slack-reply-broadcast-message) team)
  nil)

(cl-defmethod slack-message-body ((m slack-reply-broadcast-message) team)
  (let ((s (cl-call-next-method)))
    (unless (slack-string-blankp s)
      (format "%s%s"
              (if (eq major-mode 'slack-thread-message-buffer-mode)
                  ""
                "Replied to a thread: \n")
              s))))

(cl-defmethod slack-message-visible-p ((this slack-reply-broadcast-message) team)
  t)

(provide 'slack-user-message)
;;; slack-user-message.el ends here
