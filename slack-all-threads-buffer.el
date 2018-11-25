;;; slack-all-threads-buffer.el ---                  -*- lexical-binding: t; -*-

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
(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-message)
(require 'slack-thread)
(require 'slack-message-buffer)


(define-derived-mode slack-all-threads-buffer-mode
  slack-buffer-mode
  "Slack All Treads")

(defface slack-all-thread-buffer-thread-header-face
  '((t (:weight bold :height 1.2)))
  "Face used to All Threads buffer's each threads header."
  :group 'slack)

(defclass slack-all-threads-buffer (slack-buffer)
  ((current-ts :initarg :current-ts :type string)
   (has-more :initarg :has-more :type boolean)
   (total-unread-replies :initarg :total-unread-replies :type integer)
   (new-threads-count :initarg :new-threads-count :type integer)
   (threads :initarg :threads :type list '())))


(cl-defmethod slack-buffer-find ((_class (subclass slack-all-threads-buffer)) team)
  (slack-if-let* ((buf (car (oref team slack-all-threads-buffer))))
      (with-current-buffer buf slack-current-buffer)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-all-threads-buffer)) team)
  (format "*Slack - %s : All Threads"
          (slack-team-name team)))

(cl-defmethod slack-buffer-name ((this slack-all-threads-buffer))
  (slack-buffer-name 'slack-all-threads-buffer (oref this team)))

(defun slack-create-all-threads-buffer (team total-unread-replies new-threads-count threads has-more)
  (slack-if-let* ((buf (slack-buffer-find 'slack-all-threads-buffer
                                          team)))
      buf
    (slack-all-threads-buffer :team team
                              :total-unread-replies total-unread-replies
                              :new-threads-count new-threads-count
                              :threads threads
                              :has-more has-more)))

(cl-defmethod slack-buffer-init-buffer ((this slack-all-threads-buffer))
  (let* ((buf (get-buffer-create (slack-buffer-name this))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))
      (slack-all-threads-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char lui-input-marker)
      (let ((lui-time-stamp-position nil))
        (lui-insert (propertize "All Threads\n"
                                'face '(:weight bold))
                    t))
      (with-slots (threads current-ts) this
        (cl-loop for thread in threads
                 do (slack-buffer-insert-thread this thread)))
      (when (slack-buffer-has-next-page-p this)
        (slack-buffer-insert-load-more this))
      (goto-char (point-min)))
    (unless (oref (oref this team) slack-all-threads-buffer)
      (oset (oref this team) slack-all-threads-buffer
            (list buf)))
    (with-slots (team) this
      (slack-subscriptions-thread-clear-all team))
    buf))

(cl-defmethod slack-buffer-insert-thread ((this slack-all-threads-buffer) thread)
  (oset this current-ts (oref thread last-read))
  (let* ((root (oref thread root))
         (replies (oref thread messages))
         ;; (reply-count (oref thread reply-count))
         ;; (more-replies-count (- reply-count (length replies)))
         )
    (let* ((lui-time-stamp-position nil)
           (team (oref this team))
           (room (slack-room-find (oref root channel) team))
           (prefix (or (and (slack-im-p room) "@") "#")))
      (lui-insert (format "%s\n"
                          (make-string lui-fill-column ?=))
                  t)
      (lui-insert (propertize (format "%s%s"
                                      prefix
                                      (slack-room-name room team))
                              'face
                              'slack-all-thread-buffer-thread-header-face)
                  t))
    (slack-buffer-insert this root t)
    ;; (let ((lui-time-stamp-position nil))
    ;;   (when (< 1 more-replies-count)
    ;;     (lui-insert (format "%s more replies"
    ;;                         more-replies-count)))
    ;;   (when (= 1 more-replies-count)
    ;;     (lui-insert "1 more reply")))
    (cl-loop for reply in replies
             do (slack-buffer-insert this reply t))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-all-threads-buffer))
  (oref this has-more))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-all-threads-buffer))
  (slack-if-let*
      ((beg (next-single-property-change (point-min)
                                         'loading-message))
       (end (next-single-property-change beg
                                         'loading-message)))
      (delete-region beg end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-all-threads-buffer)))

(cl-defmethod slack-buffer-insert-history ((this slack-all-threads-buffer))
  (with-slots (team threads current-ts) this
    (cl-loop for thread in (cl-remove-if #'(lambda (e)
                                             (not (string< (oref e last-read)
                                                           current-ts)))
                                         threads)
             do (slack-buffer-insert-thread this thread))))

(cl-defmethod slack-buffer-insert--history ((this slack-all-threads-buffer))
  (slack-buffer-insert-history this)
  (when (slack-buffer-has-next-page-p this)
    (slack-buffer-insert-load-more this)))

(cl-defmethod slack-buffer-request-history ((this slack-all-threads-buffer) after-success)
  (let ((cur-point (point)))
    (with-slots (team current-ts) this
      (cl-labels
          ((success (total-unread-replies new-threads-count threads has-more)
                    (oset this total-unread-replies total-unread-replies)
                    (oset this new-threads-count new-threads-count)
                    (oset this threads (append (oref this threads) threads))
                    (oset this has-more has-more)
                    (funcall after-success)
                    (when (and (< (point-min) cur-point)
                               (< cur-point (point-max)))
                      (goto-char cur-point))))
        (slack-subscriptions-thread-get-view team current-ts #'success)))))


(cl-defmethod slack-buffer-display-thread ((this slack-all-threads-buffer) ts)
  (with-slots (team threads) this
    (slack-if-let*
        ((thread (cl-find-if #'(lambda (e) (string= ts (slack-ts (oref e root))))
                             threads))
         (root (oref thread root))
         (room-id (oref root channel))
         (room (slack-room-find room-id team)))
        (cl-labels
            ((display-thread (message)
                             (slack-thread-show-messages (oref message thread)
                                                         room
                                                         team)))
          (slack-if-let* ((message (slack-room-find-message room (slack-ts root))))
              (display-thread message)
            (cl-labels
                ((success (_has-more)
                          (let ((message (slack-room-find-message room (slack-ts root))))
                            (display-thread message))))
              (slack-room-history-request room team
                                          :oldest (slack-ts root)
                                          :inclusive "true"
                                          :count 1
                                          :after-success #'success)))))))

(defun slack-all-threads ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels
        ((open-buffer (total-unread-replies new-threads-count threads has-more)
                      (slack-buffer-display
                       (slack-create-all-threads-buffer team
                                                        total-unread-replies
                                                        new-threads-count
                                                        threads
                                                        has-more)))
         (success (&rest args)
                  (slack-if-let* ((buf (slack-buffer-find 'slack-all-threads-buffer
                                                          team)))
                      (progn
                        (kill-buffer (slack-buffer-buffer buf))
                        (apply #'open-buffer args))
                    (apply #'open-buffer args))))
      (slack-subscriptions-thread-get-view team nil #'success))))

(provide 'slack-all-threads-buffer)
;;; slack-all-threads-buffer.el ends here
