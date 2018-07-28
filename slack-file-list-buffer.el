;;; slack-file-list-buffer.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-room-buffer)

(define-derived-mode slack-file-list-buffer-mode slack-buffer-mode "Slack File List Buffer")

(defclass slack-file-list-buffer (slack-message-buffer) ())

(defmethod slack-buffer-name ((_this slack-file-list-buffer))
  (format "%s" (call-next-method)))

(defmethod slack-buffer-major-mode ((this slack-file-list-buffer))
  'slack-file-list-buffer-mode)

(defmethod slack-create-message-buffer ((room slack-file-room) team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer
                                             room
                                             team)))
      buffer
    (slack-file-list-buffer :room room :team team)))

(cl-defmethod slack-buffer-update ((this slack-file-list-buffer) message &key replace)
  (with-slots (room team) this
    (let ((buffer (get-buffer (slack-buffer-name this))))
      (if replace (slack-buffer-replace this message)
        (with-current-buffer buffer
          (slack-buffer-insert this message))))))

(defmethod slack-buffer-insert ((this slack-file-list-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-time (slack-message-time-stamp message))
        (ts (slack-ts message))
        (team (oref this team)))
    (lui-insert-with-text-properties
     (slack-message-to-string message ts team)
     'not-tracked-p not-tracked-p
     'ts ts
     'slack-last-ts lui-time-stamp-last)
    (lui-insert "" t)
    ))

(defmethod slack-buffer-replace ((this slack-file-list-buffer)
                                 message)
  (with-slots (team) this
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (slack-message-to-string message
                                            (slack-ts message)
                                            team)
                   (lambda ()
                     (equal (get-text-property (point) 'ts)
                            (slack-ts message)))))))

(provide 'slack-file-list-buffer)
;;; slack-file-list-buffer.el ends here
