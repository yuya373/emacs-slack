;;; slack-slash-commands.el ---                      -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defvar slack-slash-commands-available
  '("active" "away" "dnd" "leave" "join"))

(defun slack-slash-commands-parse (text)
  (if (string-prefix-p "/" text)
      (let* ((maybe-command (car (split-string (substring text 1) " ")))
             (command (cl-find maybe-command slack-slash-commands-available
                               :test #'string=)))
        (when command
          (cons command (cdr (split-string text " ")))))))

(defun slack-slack-commands-execute (command channel-id team)
  (let ((command (car command))
        (args (cdr command)))
    (cond
     ((string= command "active")
      (slack-request-set-active team))
     ((string= command "away")
      (slack-request-set-presence team))
     ((string= command "dnd")
      (slack-request-dnd-set-snooze team (car args)))
     ((string= command "leave")
      (slack-slash-commands-leave channel-id (car args) team))
     ((string= command "join")
      (slack-slash-commands-join (car args) team))
     )))

(defun slack-slash-commands-leave (channel-id channel-name team)
  (let ((channel (or (and channel-name (slack-room-find-by-name channel-name team))
                     (slack-room-find channel-id team))))
    (if channel
        (progn
          (unless (eq 'slack-channel (eieio-object-class channel))
            (error "%s is not a Channel" (slack-room-name channel)))
          (slack-channel-request-leave channel team))
      (error "Channel not found: %s" channel-name))))

(defun slack-slash-commands-join (channel-name team)
  (let ((channel (and channel-name (slack-room-find-by-name channel-name team))))
    (if channel
        (progn
          (unless (eq 'slack-channel (eieio-object-class channel))
            (error "%s is not a Channel" (slack-room-name channel)))
          (slack-channel-request-join channel team))
      (error "Channel not found: %s" channel-name))))

(provide 'slack-slash-commands)
;;; slack-slash-commands.el ends here
