;;; slack-thread-message-buffer.el ---               -*- lexical-binding: t; -*-

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
(require 'slack-room-buffer)

(defclass slack-thread-message-buffer (slack-room-buffer)
  ((thread-ts :initarg :thread-ts :type string)))

(defmethod slack-buffer-name ((this slack-thread-message-buffer))
  (format "%s Thread: %s" (call-next-method) (oref this thread-ts)))

(defmethod slack-buffer-init-buffer ((this slack-thread-message-buffer))
  (let* ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-thread-mode)
      (goto-char lui-input-marker)
      (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
      (add-hook 'kill-buffer-hook 'slack-thread-message-buffer-on-killed)
      (with-slots (room thread-ts team) this
        (if-let* ((message (slack-room-find-message room thread-ts)))
            (progn
              (slack-buffer-insert message team t)
              (let ((lui-time-stamp-position nil))
                (lui-insert (format "%s\n" (make-string lui-fill-column ?=)) t))))))
    (oset this buffer buf)
    buf))

(defun slack-thread-message-buffer-on-killed ()
  (if-let* ((buf slack-current-buffer))
      (with-slots (room thread-ts) buf
        (with-slots (thread-message-buffers) room
          (setq thread-message-buffers
                (cl-remove-if #'(lambda (e) (string= e thread-ts))
                              thread-message-buffers
                              :key #'(lambda (e) (oref e thread-ts))))))))

(defun slack-thread-message-buffer-find (thread-ts room)
  (cl-find-if #'(lambda (e) (string= e thread-ts))
              (oref room thread-message-buffers)
              :key #'(lambda (e) (oref e thread-ts))))

(defun slack-create-thread-message-buffer (room team thread-ts)
  (if-let* ((buf (slack-thread-message-buffer-find thread-ts room)))
      buf
    (let ((buf (slack-thread-message-buffer :room room
                                            :team team
                                            :thread-ts thread-ts)))
      (push buf thread-message-buffers)
      buf)))

(defmethod slack-buffer-display-message-compose-buffer
  ((this slack-thread-message-buffer))
  (with-slots (room team thread-ts) this
    (let ((buf (slack-thread-message-compose-buffer :room room
                                                    :team team
                                                    :ts thread-ts)))
      (slack-buffer-display buf))))

(defclass slack-thread-message-compose-buffer (slack-thread-message-buffer) ())

(defmethod slack-buffer-send-message ((this slack-thread-message-buffer) message)
  (with-slots (room team thread-ts) this
    (slack-thread-send-message room team message thread-ts)))

(defmethod slack-buffer-send-message
  ((this slack-thread-message-compose-buffer) message)
  (let ((buffer (slack-buffer-buffer this)))
    (with-slots (room team thread-ts) this
      (slack-thread-send-message room team message thread-ts))))



(provide 'slack-thread-message-buffer)
;;; slack-thread-message-buffer.el ends here
