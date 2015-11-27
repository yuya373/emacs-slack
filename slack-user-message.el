;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)
(require 'slack-message-formatter)

(defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(defun slack-user-message-header (m)
  (let* ((name (slack-user-name (oref m user)))
         (time (slack-message-time-to-string (oref m ts)))
         (edited-at (slack-message-time-to-string (oref m edited-at))))
    (concat name "\t" time (if edited-at
                               (concat "\t" "edited_at: " edited-at)))))

(defmethod slack-message-to-string ((m slack-user-message))
  (let* ((text (slack-message-unescape-string (oref m text)))
         (header (slack-user-message-header m)))
    (slack-message-put-header-property header)
    (slack-message-put-text-property text)
    (concat header "\n" text "\n")))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
