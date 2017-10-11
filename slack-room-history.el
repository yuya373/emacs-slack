;;; slack-room-history.el --- impl for room's prev messages  -*- lexical-binding: t; -*-

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
(require 'slack-search)
(require 'slack-buffer)
(require 'slack-team)

(defvar slack-current-room-id)
(defvar slack-current-team-id)
(defconst slack-channel-history-url "https://slack.com/api/channels.history")
(defconst slack-group-history-url "https://slack.com/api/groups.history")
(defconst slack-im-history-url "https://slack.com/api/im.history")
(defconst slack-file-history-url "https://slack.com/api/files.list")

(defclass _slack-room-history ()
  ((room :initarg :room)
   (team :initarg :team)
   (oldest :initarg :oldest)
   (current-ts :initarg :current-ts :initform nil)))

(defclass _slack-search-history (_slack-room-history)
  ((channel-id :initarg :channel-id)))

(defclass _slack-file-search-history (_slack-room-history) ())

(defmethod slack-room-prev-link-info ((room slack-room))
  (with-slots (oldest) room
    (if oldest
        (oref oldest ts))))

(defmethod slack-room-propertize-load-more ((room slack-room) base-text)
  (propertize base-text
              'oldest (slack-room-prev-link-info room)
              'class '_slack-room-history))

(defmethod slack-room-propertize-load-more ((room slack-search-result) base-text)
  (with-slots (oldest) room
    (with-slots (info) oldest
      (propertize base-text
                  'oldest (slack-room-prev-link-info room)
                  'channel-id (oref info channel-id)
                  'class '_slack-search-history))))

(defmethod slack-room-propertize-load-more ((room slack-file-search-result) base-text)
  (propertize base-text
              'oldest (slack-room-prev-link-info room)
              'class '_slack-file-search-history))

(defmethod slack-room-insert-previous-link ((room slack-room) buf)
  (when (slack-room-prev-link-info room)
    (with-current-buffer buf
      (slack-buffer-widen
       (let* ((inhibit-read-only t)
              (base (propertize "(load more message)"
                                'face '(:underline t)
                                'keymap (let ((map (make-sparse-keymap)))
                                          (define-key map (kbd "RET")
                                            #'slack-room-history-load)
                                          map)))
              (text (slack-room-propertize-load-more room base)))
         (goto-char (point-min))
         (insert (format "%s\n\n" text))
         (set-marker lui-output-marker (point)))))))

(defmethod slack-room-history--collect-meta ((this _slack-room-history))
  this)

(defmethod slack-room-history--collect-meta ((this _slack-search-history))
  (oset this channel-id (get-text-property 0 'channel-id (thing-at-point 'line))))

(defmethod slack-room-history--insert ((this _slack-room-history))
  (with-slots (team room oldest current-ts) this
    (slack-room-with-buffer room team
      (slack-buffer-widen
       (let ((inhibit-read-only t)
             (loading-message-end (slack-buffer-ts-eq (point-min) (point-max) oldest))
             (messages (slack-room-history--collect-message this))
             (cur-point (point)))
         (goto-char (point-min))
         (when loading-message-end
           (delete-region (point-min) loading-message-end))

         (if (and messages (< 0 (length messages)))
             (slack-room-insert-previous-link room buf)
           (set-marker lui-output-marker (point-min))
           (lui-insert "(no more messages)\n"))

         (cl-loop for m in messages
                  do (slack-buffer-insert m team t))
         (unless current-ts
           (goto-char cur-point))))
      (lui-recover-output-marker)
      (when current-ts
        (slack-buffer-goto current-ts)))))

(defmethod slack-room-history--request ((this _slack-room-history) after-success)
  (with-slots (team room oldest) this
    (slack-room-history-request room team :oldest oldest :after-success after-success)))

(defmethod slack-room-history--collect-message ((this _slack-room-history))
  (with-slots (team room oldest) this
    (cl-remove-if #'(lambda (m)
                      (or (string< oldest (oref m ts))
                          (string= oldest (oref m ts))))
                  (slack-room-sort-messages (copy-sequence (oref room messages))))))

(defmethod slack-room-history--collect-message ((this _slack-search-history))
  (with-slots (team room oldest channel-id) this
    (let* ((messages (reverse (oref room messages)))
           (nth (slack-search-get-index room messages oldest channel-id)))
      (if nth
          (nreverse (nthcdr (1+ nth) messages))))))

(defmethod slack-room-history--collect-message ((this _slack-file-search-history))
  (with-slots (team room oldest) this
    (let* ((messages (reverse (oref room messages)))
           (nth (slack-search-get-index room messages oldest)))
      (if nth
          (nreverse (nthcdr (1+ nth) messages))))))

(defun slack-room-history-load ()
  (interactive)
  (let* ((cur-point (point))
         (class (get-text-property 0 'class (thing-at-point 'line)))
         (team (slack-team-find slack-current-team-id))
         (prev-messages (make-instance class
                                       :room (slack-room-find slack-current-room-id team)
                                       :team team
                                       :oldest (get-text-property 0 'oldest (thing-at-point 'line))
                                       :current-ts (let ((change (next-single-property-change cur-point 'ts)))
                                                     (when change
                                                       (get-text-property change 'ts))))))
    (slack-room-history--collect-meta prev-messages)
    (slack-room-history--request prev-messages
                                 #'(lambda ()
                                     (slack-room-history--insert prev-messages)))))

(defmethod slack-room-history-url ((_room slack-channel))
  slack-channel-history-url)

(defmethod slack-room-history-url ((_room slack-group))
  slack-group-history-url)

(defmethod slack-room-history-url ((_room slack-im))
  slack-im-history-url)

(cl-defmethod slack-room-history-request ((room slack-room) team &key oldest after-success async)
  (cl-labels
      ((on-request-update
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-room-request-update")
         (let* ((datum (plist-get data :messages))
                (messages
                 (cl-loop for data in datum
                          collect (slack-message-create data team :room room))))
           (if oldest (slack-room-set-prev-messages room messages)
             (slack-room-set-messages room messages)
             (slack-room-reset-last-read room))
           (if (and after-success (functionp after-success))
               (funcall after-success))))))
    (slack-request
     (slack-request-create
      (slack-room-history-url room)
      team
      :params (list (cons "channel" (oref room id))
                    (if oldest (cons "latest" oldest)))
      :success #'on-request-update))))

(provide 'slack-room-history)
;;; slack-room-history.el ends here
