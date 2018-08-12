;;; slack-message-share-buffer.el ---                -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-room-buffer)

(define-derived-mode slack-message-share-buffer-mode
  slack-message-compose-buffer-mode
  "Slack Share Message")

(defclass slack-message-share-buffer (slack-message-compose-buffer)
  ((ts :initarg :ts :type string)
   (room :initarg :room :type slack-room)))

(defun slack-create-message-share-buffer (room team ts)
  (slack-if-let* ((buf (slack-buffer-find 'slack-message-share-buffer
                                    room ts team)))
      buf
    (slack-message-share-buffer :room room :team team :ts ts)))

(defmethod slack-buffer-find :static ((class slack-message-share-buffer) room ts team)
  (slack-buffer-find-4 class room ts team))

(defmethod slack-buffer-name :static ((class slack-message-share-buffer) room ts team)
  (format "*Slack - %s : %s  Share Message - %s"
          (oref team name)
          (slack-room-name room team)
          ts))

(defmethod slack-buffer-name ((this slack-message-share-buffer))
  (with-slots (room ts team) this
    (slack-buffer-name 'slack-message-share-buffer
                       room ts team)))

(defmethod slack-buffer-init-buffer ((this slack-message-share-buffer))
  (let* ((buf (call-next-method)))
    (with-current-buffer buf
      (slack-message-share-buffer-mode)
      (slack-buffer-set-current-buffer this))
    (with-slots (room ts team) this
      (slack-buffer-push-new-4 'slack-message-share-buffer
                               room ts team))
    buf))

(defmethod slack-buffer-send-message ((this slack-message-share-buffer) message)
  (with-slots (room team ts) this
    (slack-message-share--send team room ts message)
    (call-next-method)))

(provide 'slack-message-share-buffer)
;;; slack-message-share-buffer.el ends here
