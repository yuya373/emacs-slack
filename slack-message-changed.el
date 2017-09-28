;;; slack-message-changed.el --- impl for message changed  -*- lexical-binding: t; -*-

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
(require 'slack-room)
(require 'slack-message)
(require 'slack-message-update)
(require 'slack-thread)

(defun slack-message-changed (payload team)
  (let* ((room (slack-room-find (plist-get payload :channel) team))
         (mpayload (plist-get payload :message))
         (base (and room (slack-room-find-message room (plist-get mpayload :ts)))))
    (when base
      (slack-message-changed--copy base mpayload)
      (slack-message-update base team t))))

(defmethod slack-message-changed--copy ((this slack-message) payload)
  (oset this text (plist-get payload :text))
  (slack-message-set-attachments this payload)
  (oset this edited-at (plist-get (plist-get payload :edited) :ts)))

(defmethod slack-message-changed--copy ((this slack-file-comment-message) payload)
  (let* ((file-id (plist-get (plist-get payload :file) :id))
         (new-comment (slack-file-comment-create
                       (plist-get payload :comment)
                       file-id)))
    (with-slots (comment text) this
      (oset comment comment (oref new-comment comment)))
    (call-next-method)))

(provide 'slack-message-changed)
;;; slack-message-changed.el ends here
