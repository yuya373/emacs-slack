;;; slack-reply-event.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-event)
(require 'slack-message-buffer)

(defclass slack-reply-event (slack-event slack-message-event-processable) ())

(defun slack-create-reply-event (payload)
  (slack-reply-event :type "" :payload payload))

(cl-defmethod slack-event-find-message ((this slack-reply-event) team)
  (slack-if-let* ((payload (oref this payload))
                  (reply-to (plist-get payload :reply_to))
                  (table (oref team sent-message))
                  (sent-message (gethash reply-to table)))
      (progn
        (remhash reply-to table)
        (oset sent-message ts (plist-get payload :ts))
        sent-message)))

(cl-defmethod slack-event-update-buffer ((_this slack-reply-event) message team)
  (slack-message-update-buffer message team)
  (slack-if-let* ((room (slack-room-find message team))
                  (buffer (slack-buffer-find 'slack-message-buffer team room))
                  (ts (slack-ts message))
                  (last-read (slack-buffer-last-read buffer)))
      (progn
        (when (string< last-read ts)
          (slack-buffer-update-mark-request buffer ts))
        (when (string= last-read ts)
          (slack-buffer-update-marker-overlay buffer)))))

(provide 'slack-reply-event)
;;; slack-reply-event.el ends here
