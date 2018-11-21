;;; slack-team-ws.el ---                             -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'cl))
(require 'eieio)

(defclass slack-team-ws ()
  ((url :initarg :url)
   (conn :initarg :conn :initform nil)
   (ping-timer :initform nil)
   (check-ping-timeout-timer :initform nil)
   (check-ping-timeout-sec :initarg :check-ping-timeout-sec :initform 20)
   (connected :initform nil)
   (reconnect-auto :initarg :reconnect-auto :initform t)
   (reconnect-timer :initform nil)
   (reconnect-after-sec :initform 10)
   (reconnect-count :initform 0)
   (reconnect-count-max :initform 360)
   (last-pong :initform nil)
   (waiting-send :initform nil)
   (ping-check-timers :initform (make-hash-table :test 'equal))
   (reconnect-url :initform "" :type string)
   (connect-timeout-timer :initform nil)
   (connect-timeout-sec :type number :initform 20) ;; websocket url is valid for 30 seconds.
   (inhibit-reconnection :initform nil)
   (nowait :initarg :websocket-nowait :initform nil)
   ))

(cl-defmethod slack-ws-cancel-connect-timeout-timer ((ws slack-team-ws))
  (when (timerp (oref ws connect-timeout-timer))
    (cancel-timer (oref ws connect-timeout-timer))
    (oset ws connect-timeout-timer nil)))

(cl-defmethod slack-ws-set-connect-timeout-timer ((ws slack-team-ws) fn &rest fn-args)
  (slack-ws-cancel-connect-timeout-timer ws)
  (oset ws connect-timeout-timer
        (apply #'run-at-time (oref ws connect-timeout-sec)
               nil
               fn fn-args)))

(cl-defmethod slack-ws-cancel-ping-timer ((ws slack-team-ws))
  (with-slots (ping-timer) ws
    (if (timerp ping-timer)
        (cancel-timer ping-timer))
    (setq ping-timer nil)))

(cl-defmethod slack-ws-set-ping-timer ((ws slack-team-ws) fn &rest fn-args)
  (slack-ws-cancel-ping-timer ws)
  (oset ws ping-timer (apply #'run-at-time 10 nil fn fn-args)))

(cl-defmethod slack-ws-cancel-ping-check-timers ((ws slack-team-ws))
  (maphash #'(lambda (_key value)
               (if (timerp value)
                   (cancel-timer value)))
           (oref ws ping-check-timers))
  (oset ws ping-check-timers (make-hash-table :test 'equal)))

(cl-defmethod slack-ws-set-ping-check-timer ((ws slack-team-ws) time fn &rest fn-args)
  (puthash time (apply #'run-at-time
                       (oref ws check-ping-timeout-sec)
                       nil fn fn-args)
           (oref ws ping-check-timers)))

(cl-defmethod slack-ws-cancel-reconnect-timer ((ws slack-team-ws))
  (with-slots (reconnect-timer) ws
    (if (timerp reconnect-timer)
        (cancel-timer reconnect-timer))
    (setq reconnect-timer nil)))

(cl-defmethod slack-ws-set-reconnect-timer ((ws slack-team-ws) fn &rest fn-args)
  (slack-ws-cancel-reconnect-timer ws)
  (oset ws reconnect-timer
        (apply #'run-at-time (oref ws reconnect-after-sec)
               nil
               fn fn-args)))

(cl-defmethod slack-ws-reconnect-count-exceed-p ((ws slack-team-ws))
  (< (oref ws reconnect-count-max)
     (oref ws reconnect-count)))

(cl-defmethod slack-ws-inc-reconnect-count ((ws slack-team-ws))
  (incf (oref ws reconnect-count)))

(cl-defmethod slack-ws-use-reconnect-url-p ((ws slack-team-ws))
  (< 0 (length (oref ws reconnect-url))))

(provide 'slack-team-ws)
;;; slack-team-ws.el ends here
