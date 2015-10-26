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
       (string= (slot-value m 'user) (slot-value m 'user))
       (string= (slot-value m 'ts) (slot-value m 'ts))))

(defmethod slack-message-to-string ((m slack-user-message))
  (let* ((user (slack-user-find (oref m 'user)))
         (name (gethash "name" user))
         (text (oref m 'text))
         (time (slack-message-time-to-string m))
         (header (concat name "\t" time)))
    (slack-message-put-header-property header)
    (concat header "\n" text "\n")))

(provide 'slack-user-message)
;;; slack-user-message.el ends here
