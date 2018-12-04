;;; slack-reply.el ---handle reply from slack        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-util)
(require 'slack-message)
(require 'slack-team)
(require 'slack-message-buffer)

(defclass slack-reply (slack-message)
  ((user :initarg :user :initform nil)
   (reply-to :initarg :reply_to :type integer)
   (id :initarg :id :type integer)))

(cl-defmethod slack-message-handle-reply ((m slack-reply) team)
  (with-slots (reply-to) m
    (let ((sent-msg (slack-message-find-sent m team)))
      (if sent-msg
          (progn
            (oset sent-msg ts (slack-ts m))
            (slack-message-update sent-msg team)
            (slack-if-let*
                ((room (slack-room-find (oref sent-msg channel) team))
                 (buffer (slack-buffer-find 'slack-message-buffer
                                            room team))
                 (ts (slack-ts sent-msg))
                 (last-read (slack-buffer-last-read buffer)))
                (progn
                  (when (string< last-read ts)
                    (slack-buffer-update-mark-request buffer ts))
                  (when (string= last-read ts)
                    (slack-buffer-update-marker-overlay buffer)))))))))

(cl-defmethod slack-message-find-sent ((m slack-reply) team)
  (with-slots (reply-to) m
    (with-slots (sent-message) team
      (let ((found (gethash reply-to sent-message)))
        (remhash reply-to sent-message)
        found))))

(cl-defmethod slack-message-sender-equalp ((m slack-reply) sender-id)
  (string= (oref m user) sender-id))


(provide 'slack-reply)
;;; slack-reply.el ends here
