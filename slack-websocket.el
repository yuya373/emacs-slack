;;; slack-websocket.el --- slack websocket interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'websocket)
(require 'slack-request)
(require 'slack-message)
(require 'slack-reply)

(defvar slack-ws nil)
(defvar slack-ws-url nil)

(defun slack-ws-open ()
  (unless slack-ws
    (setq slack-ws (websocket-open
                   slack-ws-url
                   :on-message #'slack-ws-on-message))))

(defun slack-ws-close ()
  (interactive)
  (if slack-ws
      (progn
        (websocket-close slack-ws)
        (setq slack-ws nil))))

(defun slack-ws-send (payload)
  (websocket-send-text slack-ws payload))

(defun slack-ws-recursive-decode (payload)
  (cl-labels ((decode (e) (if (stringp e)
                              (decode-coding-string e 'utf-8-unix)
                            e))
              (recur (payload acc)
                      (if (and payload (< 0 (length payload)))
                          (let ((h (car payload)))
                            (if (and (not (stringp h)) (or (arrayp h) (listp h)))
                                (recur (cdr payload) (cons (recur (append h nil) ()) acc))
                              (recur (cdr payload) (cons (decode h) acc))))
                        (reverse acc))))
    (recur payload ())))


(defun slack-ws-on-message (_websocket frame)
  ;; (message "%s" (websocket-frame-payload frame))
  (when (websocket-frame-completep frame)
    (let* ((payload (slack-request-parse-payload
                     (websocket-frame-payload frame)))
           (decoded-payload (slack-ws-recursive-decode payload))
           (type (plist-get decoded-payload :type)))
      (cond
       ((string= type "hello")
        (message "Slack Websocket Is Ready!"))
       ((string= type "message")
        (slack-ws-handle-message decoded-payload))
       ((plist-get decoded-payload :reply_to)
        (slack-ws-handle-reply decoded-payload))))))

(defun slack-ws-handle-message (payload)
  (let ((m (slack-message-create payload)))
    (if m
        (slack-message-update m))))

(defun slack-ws-handle-reply (payload)
  (let ((ok (plist-get payload :ok))
        (e (plist-get payload :error)))
    (if ok
        (slack-message-handle-reply
         (slack-message-create payload))
      (error "Code: %s msg: %s"
             (plist-get :code e)
             (plist-get :msg e)))))

(provide 'slack-websocket)
;;; slack-websocket.el ends here
