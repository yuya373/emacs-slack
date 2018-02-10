;;; slack-request-worker.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  南優也

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
(require 'slack-util)
(require 'slack-request)

(defcustom slack-request-worker-max-request-limit 30
  "Max request count perform simultaneously."
  :group 'slack)

(defvar slack-request-worker-instance nil)

(defclass slack-request-worker ()
  ((queue :initform '() :type list)
   (timer :initform nil)
   (current-request-count :initform 0 :type integer)
   ))

(defun slack-request-worker-create ()
  "Create `slack-request-worker' instance."
  (make-instance 'slack-request-worker))

(defmethod slack-request-worker-push ((this slack-request-worker) req)
  (let ((l '()))
    (cl-pushnew req (oref this queue)
                :test #'slack-equalp)))

(defmethod slack-request-worker-create-timer ((_this slack-request-worker))
  (run-at-time t 1 #'slack-request-worker-execute))

(defun slack-request-worker-execute ()
  "Pop request from queue until `slack-request-worker-max-request-limit', and execute."
  (let ((do '())
        (skip '())
        (current (time-to-seconds))
        (limit (- slack-request-worker-max-request-limit
                  (oref slack-request-worker-instance
                        current-request-count))))

    (cl-loop for req in (reverse (oref slack-request-worker-instance queue))
             do (if (and (< (oref req execute-at) current)
                         (< (length do) limit))
                    (push req do)
                  (push req skip)))

    ;; (message "[WORKER] QUEUE: %s, LIMIT: %s, CURRENT: %s, DO: %s, SKIP: %s"
    ;;          (length (oref slack-request-worker-instance queue))
    ;;          slack-request-worker-max-request-limit
    ;;          (oref slack-request-worker-instance current-request-count)
    ;;          (length do)
    ;;          (length skip))

    (oset slack-request-worker-instance queue skip)

    (cl-labels
        ((decl-request-count
          ()
          (cl-decf (oref slack-request-worker-instance
                         current-request-count))))
      (cl-loop for req in do
               do (progn
                    (cl-incf (oref slack-request-worker-instance
                                   current-request-count))
                    (slack-request req
                                   :on-success #'decl-request-count
                                   :on-error #'decl-request-count
                                   ))))))

(defmethod slack-request-worker-push ((req slack-request-request))
  (unless slack-request-worker-instance
    (setq slack-request-worker-instance
          (slack-request-worker-create)))
  (slack-request-worker-push slack-request-worker-instance req)
  (unless (oref slack-request-worker-instance timer)
    (oset slack-request-worker-instance timer
          (slack-request-worker-create-timer slack-request-worker-instance))))


(provide 'slack-request-worker)
;;; slack-request-worker.el ends here
