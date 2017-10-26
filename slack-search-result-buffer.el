;;; slack-search-result-buffer.el ---                -*- lexical-binding: t; -*-

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

(defclass slack-search-result-buffer (slack-buffer)
  ((search-result :initarg :search-result :type slack-search-result)))

(defmethod slack-buffer-name :static ((class slack-search-result-buffer) search-result team)
  (with-slots (query) search-result
    (format "*Slack - %s : Search Result - %s"
            (oref team name)
            query)))

(defmethod slack-buffer-name ((this slack-search-result-buffer))
  (with-slots (search-result team) this
    (slack-buffer-name (eieio-object-class-name this) search-result team)))

(defun slack-create-search-result-buffer (search-result team)
  (if-let* ((buffer (slack-buffer-find 'slack-search-result-buffer
                                       search-result
                                       team)))
      buffer
    (make-instance 'slack-search-result-buffer
                   :team team
                   :search-result search-result)))

(defmethod slack-buffer-insert ((this slack-search-result-buffer) message)
  (with-slots (team) this
    (with-slots (channel ts username text) message
      (let* ((time (slack-ts-to-time ts))
             (room (slack-room-find (oref channel id) team))
             (header (format "%s%s"
                             (if (slack-channel-p room)
                                 "#" "@")
                             (slack-room-name room)))
             (message-header (propertize (format "%s" username)
                                         'face 'slack-message-output-header))
             (message-body (slack-message-unescape-string text
                                                          team)))
        l
        (let ((lui-time-stamp-time time)
              (lui-time-stamp-format "%Y-%m-%d"))
          (lui-insert header t))
        (let ((lui-time-stamp-time time)
              (lui-time-stamp-format "%H:%M:%S"))
          (lui-insert (slack-format-message message-header
                                            message-body)
                      t))))))

(defmethod slack-buffer-init-buffer ((this slack-search-result-buffer))
  (let ((buffer (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buffer
      (slack-info-mode)
      (with-slots (search-result) this
        (cl-loop for m in (oref search-result matches)
                 do (slack-buffer-insert this m))))
    (with-slots (search-result team) this
      (slack-buffer-push-new-3 'slack-search-result-buffer
                               search-result
                               team))
    buffer))


(provide 'slack-search-result-buffer)
;;; slack-search-result-buffer.el ends here
