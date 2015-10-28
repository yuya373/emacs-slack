;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (edited :initarg :edited)
   (id :initarg :id)
   (inviter :initarg :inviter)))

(defmethod slack-message-sender-equalp ((m slack-user-message) sender-id)
  (string= (oref m user) sender-id))

(defmethod slack-message-to-string ((m slack-user-message))
  (let* ((name (slack-user-name (oref m user)))
         (text (oref m text))
         (time (slack-message-time-to-string m))
         (header (concat name "\t" time)))
    (slack-message-put-header-property header)
    (slack-message-put-text-property text)
    (concat header "\n" text "\n")))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
