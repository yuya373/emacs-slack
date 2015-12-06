;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)
(require 'slack-message-formatter)

(defvar slack-user-message-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "e" #'slack-message-edit)
    keymap))

(defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(defun slack-user-message-header (m)
  (let* ((name (slack-user-name (oref m user)))
         (time (slack-message-time-to-string (oref m ts)))
         (edited-at (slack-message-time-to-string (oref m edited-at)))
         (header (format "%s \t%s" name time)))
    (if edited-at
        (format "%s  edited_at: %s" header edited-at)
      header)))

(defmethod slack-message-propertize ((m slack-user-message) text)
  (with-slots (ts) m
    (propertize text
                'ts ts
                'keymap slack-user-message-keymap)))

(defmethod slack-message-to-string ((m slack-user-message))
  (let* ((text (slack-message-unescape-string (oref m text)))
         (header (slack-user-message-header m)))
    (slack-message-put-header-property header)
    (slack-message-put-text-property text)
    (slack-message-propertize m
                              (concat header "\n" text "\n"))))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
