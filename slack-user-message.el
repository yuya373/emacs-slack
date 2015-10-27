;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'eieio)

(defclass slack-user-message (slack-message)
  ((user :initarg :user :type string)
   (edited :initarg :edited)
   (inviter :initarg :inviter)))

(defmethod slack-message-equal ((msg slack-user-message) m)
  (and (slot-boundp  m 'user)
       (string= (oref m user) (oref m user))
       (string= (oref m ts) (oref m ts))))

(defmethod slack-message-to-string ((m slack-user-message))
  (let* ((user (slack-user-find (oref m user)))
         (name (gethash "name" user))
         (text (oref m text))
         (time (slack-message-time-to-string m))
         (header (concat name "\t" time)))
    (slack-message-put-header-property header)
    (slack-message-put-text-property text)
    (concat "\n" header "\n" text "\n")))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
