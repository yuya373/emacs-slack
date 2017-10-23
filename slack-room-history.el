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

(defmethod slack-room-previous-link ((room slack-room))
  (when (slack-room-prev-link-info room)
    (slack-room-propertize-load-more
     room
     (propertize "(load more message)"
                 'face '(:underline t)
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "RET")
                             #'slack-room-history-load)
                           map)))))

(defun slack-room-history-load ()
  (interactive)
  (if-let* ((buf slack-current-buffer))
      (slack-buffer-load-history buf)))

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
