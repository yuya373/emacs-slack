;;; slack-log.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018

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
(require 'slack-util)
(require 'slack-team)
(require 'slack-team-ws)

(defvar slack-buffer-function)

(defconst slack-log-levels
  '(;; debugging
    (trace . 40) (debug . 30)
    ;; information
    (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defcustom slack-log-level 'info
  "Used in `slack-message-logger'.
One of 'info, 'debug"
  :type '(choice (const trace)
                 (const debug)
                 (const info)
                 (const warn)
                 (const error))
  :group 'slack)

(defcustom slack-log-time-format
  "[%Y-%m-%d %H:%M:%S]"
  "Time format for log."
  :type 'string
  :group 'slack)


(defun slack-log-level-to-int (level)
  (let ((cell (cl-assoc level slack-log-levels)))
    (if cell (cdr cell)
      20)))


(defun slack-message-logger (message level team)
  "Display MESSAGE with LEVEL using `message'."
  (let ((user-level (slack-log-level-to-int slack-log-level))
        (current-level (slack-log-level-to-int level)))
    (when (<= current-level user-level)
      (message (format "%s [%s] [%s] %s"
                       (format-time-string slack-log-time-format)
                       level
                       (slack-team-name team)
                       message)))))

(cl-defun slack-log (msg team &key
                         (logger #'slack-message-logger)
                         (level 'debug))
  "LEVEL is one of 'trace, 'debug, 'info, 'warn, 'error"
  (let ((log (format "%s [%s] %s - %s"
                     (format-time-string slack-log-time-format)
                     level
                     msg
                     (slack-team-name team)))
        (buf (get-buffer-create (slack-log-buffer-name team))))
    (when (functionp logger)
      (funcall logger msg level team))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (save-excursion
        (goto-char (point-max))
        (insert log)
        (insert "\n"))
      (setq buffer-read-only t))))

(defun slack-log-buffer-name (team)
  (format "*Slack Log - %s*" (slack-team-name team)))

(defun slack-log-open-buffer ()
  (interactive)
  (let ((team (slack-team-select t t)))
    (funcall slack-buffer-function (get-buffer-create (slack-log-buffer-name team)))))

(defun slack-event-log-buffer-name (team)
  (format "*Slack Event Log - %s*" (slack-team-name team)))

(defun slack-log-websocket-payload (payload team)
  (let* ((bufname (slack-event-log-buffer-name team))
         (buf (get-buffer-create bufname)))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (save-excursion
          (goto-char (point-max))
          (insert (format "[%s] %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S")
                          payload)))
        (setq buffer-read-only t)))))

(defun slack-log-open-websocket-buffer ()
  (interactive)
  (if websocket-debug
      (progn
        (let* ((team (slack-team-select t t))
               (websocket (and (slot-boundp team 'ws)
                               (oref (oref team ws) conn))))
          (if websocket
              (funcall slack-buffer-function
                       (websocket-get-debug-buffer-create websocket))
            (error "Websocket is not connected"))))
    (error "`websocket-debug` is not t")))

(defun slack-log-open-event-buffer ()
  (interactive)
  (let* ((team (slack-team-select t t))
         (bufname (slack-event-log-buffer-name team))
         (buf (get-buffer bufname)))
    (if buf
        (funcall slack-buffer-function buf)
      (error "No Event Log Buffer"))))

(provide 'slack-log)
;;; slack-log.el ends here
