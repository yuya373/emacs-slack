;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)
(require 'slack-message-formatter)
(require 'slack-message-reaction)
(require 'slack-message-editor)

(defvar slack-user-message-keymap
  (let ((keymap (make-sparse-keymap)))
    keymap))

(defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(defmethod slack-message-header ((m slack-user-message) team)
  (with-slots (ts edited-at deleted-at) m
    (let* ((name (slack-message-sender-name m team))
           (time (slack-message-time-to-string ts))
           (edited-at (slack-message-time-to-string edited-at))
           (deleted-at (slack-message-time-to-string deleted-at))
           (header (format "%s" name)))
      (if deleted-at
          (format "%s deleted_at: %s" header deleted-at)
        (if edited-at
            (format "%s edited_at: %s" header edited-at)
          header)))))

(defmethod slack-message-propertize ((m slack-user-message) text)
  (put-text-property 0 (length text) 'keymap slack-user-message-keymap text)
  text)

(provide 'slack-user-message)
;;; slack-user-message.el ends here
