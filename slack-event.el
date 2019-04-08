;;; slack-event.el ---                               -*- lexical-binding: t; -*-

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
(require 'slack-room)

(defun slack-event-thread-message-p (payload)
  (let ((v (plist-get payload :thread_ts)))
    (and (not (null v))
         (stringp v)
         (not (slack-string-blankp v)))))

(defclass slack-event ()
  ((type :initarg :type :type string)
   (payload :initarg :payload)))

(cl-defgeneric slack-event-find-message (event team))
(cl-defgeneric slack-event-update-buffer (event message team))
(cl-defgeneric slack-event-notify (event message team))
(cl-defgeneric slack-event-update-ui (event message team))
(cl-defgeneric slack-event-save-message (event message team))
(cl-defgeneric slack-event-update (event team))

(cl-defmethod slack-event-find-message ((_this slack-event) _team))

(cl-defmethod slack-event-save-message ((_this slack-event) message team)
  (let ((room (slack-room-find message team)))
    (slack-room-push-message room message team)))

(cl-defmethod slack-event-update-buffer ((_this slack-event) _message _team))
(cl-defmethod slack-event-notify ((_this slack-event) _message _team))

(cl-defmethod slack-event-update-ui ((this slack-event) message team)
  (slack-event-update-buffer this message team)
  (slack-event-notify this message team))

(cl-defmethod slack-event-update ((this slack-event) team)
  (let ((message (slack-event-find-message this team)))
    (when message
      (slack-event-save-message this message team)
      (slack-event-update-ui this message team))))

(provide 'slack-event)
;;; slack-event.el ends here
