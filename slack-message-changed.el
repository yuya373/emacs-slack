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
      (plist-put mpayload :edited-at (plist-get (plist-get mpayload :edited) :ts))
      (let* ((new-message (slack-message-create mpayload team :room room))
             (notifyp (slack-message-changed--copy base new-message)))
        (slack-message-update base team t (not notifyp))))))

(defmethod slack-message-changed--copy ((this slack-message) other)
  (let ((changed nil))
    (with-slots (text attachments edited-at) this
      (unless (string= text (oref other text))
        (setq text (oref other text))
        (setq changed t))
      (setq attachments (oref other attachments))
      (setq edited-at (oref other edited-at)))
    changed))

(provide 'slack-message-changed)
;;; slack-message-changed.el ends here
