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

(defclass _slack-message-changed ()
  ((room :initarg :room :initform nil)
   (team :initarg :team)
   (edited-message :initarg :edited-message)
   (base-message :initarg :base-message :initform nil)))

(defclass _slack-thread-message-changed (_slack-message-changed) ())

(defun slack-message-changed (payload team)
  (let* ((room (slack-room-find (plist-get payload :channel) team))
         (mpayload (plist-get payload :message))
         (edited (and room (slack-message-create (slack-decode mpayload)
                                                 team
                                                 :room room)))
         (base (and room (slack-room-find-message room (plist-get mpayload :ts))))
         (class (and base (or (and (slack-message-thread-messagep base) '_slack-thread-message-changed)
                              '_slack-message-changed)))
         (changed (and class (make-instance class
                                            :room room
                                            :team team
                                            :edited-message edited
                                            :base-message base))))
    (when changed
      (slack-message-changed--copy changed)
      (slack-message-changed--update changed))))

(defmethod slack-message-changed--copy ((this _slack-message-changed))
  (with-slots ((base base-message) (edited edited-message)) this
    (oset base text (oref edited text))
    (oset base edited-at (oref edited edited-at))))

(defmethod slack-message-changed--update ((this _slack-message-changed))
  (with-slots ((base base-message) team) this
    (slack-message-update base team t)))

(provide 'slack-message-changed)
;;; slack-message-changed.el ends here
