;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)
(require 'slack-message-formatter)
(require 'slack-message-reaction)
(require 'slack-message-editor)

(defvar slack-user-message-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "e" #'slack-message-edit)
    (define-key keymap "r" #'slack-message-add-reaction)
    (define-key keymap "R" #'slack-message-remove-reaction)
    keymap))

(defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(defun slack-message-append-edited-at (m text)
  (with-slots (edited-at) m
    (if edited-at
        (format "%s  edited_at: %s" text (slack-message-time-to-string edited-at))
      text)))

(defun slack-user-message-header (m)
  (let* ((name (slack-user-name (oref m user)))
         (time (slack-message-time-to-string (oref m ts)))
         (edited-at (slack-message-time-to-string (oref m edited-at)))
         (header (format "%s \t%s" name time)))
    (slack-message-append-edited-at m header)))

(defmethod slack-message-propertize ((m slack-user-message) text)
  (with-slots (ts) m
    (propertize text
                'ts ts
                'keymap slack-user-message-keymap)))

(defmethod slack-message-to-string ((m slack-user-message))
  (with-slots (text reactions) m
    (let* ((text-escaped (slack-message-unescape-string text))
           (header (slack-user-message-header m))
           (reactions-str (slack-message-reactions-to-string reactions)))
      (slack-message-put-header-property header)
      (slack-message-put-text-property text-escaped)
      (slack-message-put-reactions-property reactions-str)
      (slack-message-propertize m
                                (concat header "\n"
                                        text-escaped "\n" "\n"
                                        reactions-str "\n")))))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
