;;; slack-message-buffer.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(require 'slack-buffer)

(defclass slack-message-buffer (slack-buffer)
  ((oldest-ts :initform nil :type (or null string))
   (last-read :initform nil :type (or null string))))


(defmethod slack-buffer-update-mark ((this slack-message-buffer) message)
  (with-slots (room team) this
    (slack-room-update-mark room team message)))


(defmethod slack-buffer-send-message ((this slack-message-buffer) message)
  (with-slots (room team) this
    (slack-message-send-internal message (oref room id) team)))


(defmethod slack-buffer-buffer ((this slack-message-buffer))
  (let ((buffer (call-next-method)))
    (with-current-buffer buffer
      (slack-buffer-insert-latest-messages this))
    buffer))

(defmethod slack-buffer-display-unread-threads ((this slack-message-buffer))
  (with-slots (room team) this
    (let* ((threads (mapcar #'(lambda (m) (oref m thread))
                            (cl-remove-if
                             #'(lambda (m)
                                 (or (not (slack-message-thread-parentp m))
                                     (not (< 0 (oref (oref m thread) unread-count)))))
                             (oref room messages))))
           (alist (mapcar #'(lambda (thread)
                              (cons (slack-thread-title thread team) thread))
                          (cl-sort threads
                                   #'string>
                                   :key #'(lambda (thread) (oref thread thread-ts)))))
           (selected (slack-select-from-list (alist "Select Thread: "))))
      (slack-thread-show-messages selected room team))))

(defmethod slack-buffer-start-thread ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let* ((message (slack-room-find-message room ts))
           (buf (slack-create-thread-message-buffer room team ts)))
      (if (object-of-class-p message 'slack-reply-broadcast-message)
          (error "Can't start thread from broadcasted message"))
      (slack-buffer-display buf))))

(defmethod slack-buffer-init-buffer ((this slack-message-buffer))
  (let ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-mode)
      (setq slack-current-buffer this)
      (add-hook 'kill-buffer-hook 'slack-message-buffer-on-killed nil t)
      (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
      (goto-char (point-min))
      (let ((lui-time-stamp-position nil))
        (lui-insert (slack-room-previous-link (oref this room)) t)))
    (oset (oref this room) buffer this)
    buf))

(defun slack-message-buffer-on-killed ()
  (if-let* ((buf (and (boundp 'slack-current-buffer)
                      slack-current-buffer)))
      (with-slots (room) buf
        (and room (oset room buffer nil)))))

(cl-defmethod slack-buffer-update ((this slack-message-buffer) message &key replace)
  (with-slots (room team buffer) this
    (slack-buffer-update-last-read this message)
    (if (slack-buffer-in-current-frame buffer)
        (slack-room-update-mark room team message)
      (slack-room-inc-unread-count room))
    (if replace (slack-buffer-replace this message)
      (with-current-buffer buffer (slack-buffer-insert message team)))))

(defmethod slack-buffer-display-message-compose-buffer ((this slack-message-buffer))
  (with-slots (room team) this
    (let ((buf (slack-message-compose-buffer :room room
                                             :team team)))
      (slack-buffer-display buf))))

(defmethod slack-buffer-message-delete ((this slack-message-buffer) ts)
  (with-slots (buffer) this
    (lui-delete #'(lambda () (equal (get-text-property (point) 'ts)
                                    ts)))))

(defmethod slack-buffer-update-last-read ((this slack-message-buffer) message)
  (with-slots (last-read) this
    (if (or (null last-read)
            (string< last-read (oref message ts)))
        (setq last-read (oref message ts)))))

(defmethod slack-buffer-insert-latest-messages ((this slack-message-buffer))
  (with-slots (room team last-read) this
    (let* ((messages (slack-room-sorted-messages room))
           (latest-message (car (last messages))))
      (cl-loop for m in messages
               do (if (or (null last-read)
                          (string< last-read (oref m ts)))
                      (slack-buffer-insert m team t)))
      (when latest-message
        (slack-buffer-update-last-read this latest-message)
        (slack-buffer-update-mark this latest-message)))))

(defmethod slack-buffer-display-thread ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((thread (slack-room-find-thread room ts)))
      (if thread (slack-thread-show-messages thread room team)
        (slack-thread-start)))))

(defmethod slack-buffer-display-edit-message-buffer ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-edit-message-buffer room team ts)))
      (slack-buffer-display buf))))

(defun slack-create-message-buffer (room team)
  (slack-message-buffer :room room :team team))

(defmethod slack-buffer-share-message ((this slack-message-buffer) ts)
  (with-slots (room team) this
    (let ((buf (slack-create-message-share-buffer room team ts)))
      (slack-buffer-display buf))))

(provide 'slack-message-buffer)
;;; slack-message-buffer.el ends here
