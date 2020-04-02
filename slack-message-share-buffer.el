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
(require 'slack-buffer)
(require 'slack-message-compose-buffer)
(require 'slack-message-edit-buffer)
(require 'slack-message-editor)

(define-derived-mode slack-message-share-buffer-mode
  slack-edit-message-mode
  "Slack Share Message")

(defclass slack-message-share-buffer (slack-buffer)
  ((room-id :initarg :room-id :type string)
   (ts :initarg :ts :type string)))

(cl-defmethod slack-buffer-room ((this slack-message-share-buffer))
  (with-slots (room-id) this
    (slack-room-find room-id (slack-buffer-team this))))

(defun slack-create-message-share-buffer (room team ts)
  (slack-if-let* ((buf (slack-buffer-find 'slack-message-share-buffer team room ts)))
      buf
    (slack-message-share-buffer :room-id (oref room id) :team-id (oref team id) :ts ts)))

(cl-defmethod slack-buffer-name ((this slack-message-share-buffer))
  (let ((ts (oref this ts))
        (team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (format "*Slack - %s : %s  Share Message - %s"
            (slack-team-name team)
            (slack-room-name room team)
            ts)))

(cl-defmethod slack-buffer-key ((_class (subclass slack-message-share-buffer)) room ts)
  (concat (oref room id)
          ":"
          ts))

(cl-defmethod slack-buffer-key ((this slack-message-share-buffer))
  (let ((room (slack-buffer-room this))
        (ts (oref this ts)))
    (slack-buffer-key 'slack-message-share-buffer room ts)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-message-share-buffer)))
  'slack-message-share-buffer)

(cl-defmethod slack-buffer-init-buffer ((this slack-message-share-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-share-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))

(cl-defmethod slack-buffer-send-message ((this slack-message-share-buffer) message)
  (with-slots (ts) this
    (slack-message-share--send (slack-buffer-team this)
                               (slack-buffer-room this)
                               ts
                               message)
    (slack-buffer-kill-buffer-window this)))

(provide 'slack-message-share-buffer)
;;; slack-message-share-buffer.el ends here
