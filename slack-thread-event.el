;;; slack-thread-event.el ---                        -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-event)

(defclass slack-thread-event (slack-event slack-message-event-processable) ())

(cl-defmethod slack-event-find-message ((this slack-thread-event) team)
  (let* ((payload (oref this payload))
         (subscription (plist-get payload :subscription))
         (channel (plist-get subscription :channel))
         (thread-ts (plist-get subscription :thread_ts)))
    (slack-if-let* ((room (slack-room-find channel team)))
        (slack-room-find-message room thread-ts))))

(defclass slack-thread-marked-event (slack-thread-event) ())

(defun slack-create-thread-marked-event (payload)
  (make-instance 'slack-thread-marked-event
                 :payload payload))

(cl-defmethod slack-event-save-message ((this slack-thread-marked-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (let* ((payload (oref this payload))
             (subscription (plist-get payload :subscription))
             (last-read (plist-get subscription :last_read)))
        (oset message last-read last-read)
        (slack-room-push-message room message team))))

(defclass slack-thread-subscribed-event (slack-thread-event) ())

(defun slack-create-thread-subscribed-event (payload)
  (make-instance 'slack-thread-subscribed-event
                 :payload payload))

(cl-defmethod slack-event-save-message ((this slack-thread-subscribed-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (let* ((payload (oref this payload))
             (subscription (plist-get payload :subscription))
             (last-read (plist-get subscription :last_read)))
        (oset message subscribed t)
        (oset message last-read last-read)
        (slack-room-push-message room message team))))

(defclass slack-thread-unsubscribed-event (slack-thread-event) ())

(defun slack-create-thread-unsubscribed-event (payload)
  (make-instance 'slack-thread-unsubscribed-event
                 :payload payload))

(cl-defmethod slack-event-save-message ((this slack-thread-unsubscribed-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (let* ((payload (oref this payload))
             (subscription (plist-get payload :subscription))
             (last-read (plist-get subscription :last_read)))
        (oset message last-read last-read)
        (oset message subscribed nil)
        (slack-room-push-message room message team))))

(provide 'slack-thread-event)
;;; slack-thread-event.el ends here
