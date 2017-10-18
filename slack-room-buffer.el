;;; slack-room-buffer.el ---                         -*- lexical-binding: t; -*-

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

(defclass slack-room-buffer (slack-buffer)
  ((room :initarg :room :type slack-room)))

(defmethod slack-buffer-name :static ((class slack-room-buffer) room team)
  (if-let* ((room-name (slack-room-name room)))
      (format  "*Slack* : %s"
               (or (and slack-display-team-name
                        (format "%s - %s"
                                (oref team name)
                                room-name))
                   room-name))))

(defmethod slack-buffer-find :static ((class slack-room-buffer) room team)
  (if-let* ((buf (cl-find-if
                  #'(lambda (buf)
                      (string= (buffer-name buf)
                               (slack-buffer-name class room team)))
                  (slot-value team class))))
      (with-current-buffer buf slack-current-buffer)))

(defmethod slack-buffer-name ((this slack-room-buffer))
  (with-slots (room team) this
      (slack-buffer-name (eieio-object-class-name this) room team)))


(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
