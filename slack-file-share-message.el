;;; slack-file-share-message.el ---                  -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-team)
(require 'slack-message)

(defclass slack-file-share-message (slack-file-message)
  ((upload :initarg :upload)
   (user :initarg :user :initform nil)))


(defmethod slack-message-image-to-string ((m slack-file-share-message))
  (slack-image-string (slack-file-thumb-image-spec (oref m file))))

(defmethod slack-team-display-image-inlinep ((m slack-file-share-message) team)
  (slack-team-display-image-inlinep (oref m file) team))

(defmethod slack-message-to-string ((m slack-file-share-message) team)
  (cl-labels
      ((redisplay () (slack-message-redisplay m (slack-room-find (oref m channel) team))))
    (let* ((header (slack-message-header-to-string m team))
           (attachment-body (slack-message-attachment-body m team))
           (body (slack-file-summary (oref m file) (slack-ts m) team))
           (thumb (slack-message-image-to-string m))
           (reactions (slack-message-reaction-to-string m team))
           (thread (slack-thread-to-string m team))
           (initial-comment (slack-if-let* ((initial-comment
                                             (oref (oref m file) initial-comment)))
                                (propertize
                                 (format "\n“ %s%s"
                                         (slack-message-body initial-comment team)
                                         (let ((str (slack-message-reaction-to-string
                                                     initial-comment
                                                     team)))
                                           (if (slack-string-blankp str)
                                               ""
                                             (format "\n%s" str))))
                                 'file-comment-id (oref initial-comment id))
                              "")))
      (slack-format-message header body attachment-body
                            thumb reactions initial-comment thread))))

(defun slack-get-file-comment-id ()
  (get-text-property 0 'file-comment-id (thing-at-point 'line)))

(defmethod slack-message-get-param-for-reaction ((m slack-file-share-message))
  (slack-if-let* ((file-comment-id (slack-get-file-comment-id)))
      (cons "file_comment" file-comment-id)
    (cons "file" (oref (oref m file) id))))

(defmethod slack-message-star-added ((m slack-file-share-message))
  (slack-message-star-added (oref m file)))

(defmethod slack-message-star-removed ((m slack-file-share-message))
  (slack-message-star-removed (oref m file)))

(defmethod slack-message-star-api-params ((m slack-file-share-message))
  (slack-message-star-api-params (oref m file)))

(defmethod slack-reaction-find ((this slack-file-share-message) reaction)
  (slack-reaction-find (oref this file) reaction))

(defmethod slack-reaction-delete ((this slack-file-share-message) reaction)
  (slack-reaction-delete (oref this file) reaction))

(defmethod slack-reaction-push ((this slack-file-share-message) reaction)
  (slack-reaction-push (oref this file) reaction))

(defmethod slack-message-append-reaction ((m slack-file-share-message) reaction
                                          &optional type)
  (if (string= type "file_comment")
      (slack-if-let* ((old-reaction (slack-reaction-find (oref (oref m file) initial-comment)
                                                         reaction)))
          (slack-reaction-join old-reaction reaction)
        (slack-reaction-push (oref (oref m file) initial-comment) reaction))
    (slack-if-let* ((old-reaction (slack-reaction-find m reaction)))
        (slack-reaction-join old-reaction reaction)
      (slack-reaction-push m reaction))))

(defmethod slack-message-pop-reaction ((m slack-file-share-message) reaction
                                       &optional type)
  (if (string= type "file_comment")
      (slack-message--pop-reaction (oref (oref m file) initial-comment)
                                   reaction)
    (slack-message--pop-reaction m reaction)))

(defmethod slack-message-changed--copy ((this slack-file-share-message) other)
  (let ((changed (call-next-method)))
    (let ((new-file (oref other file)))
      (oset this file new-file))
    changed))

(provide 'slack-file-share-message)
;;; slack-file-share-message.el ends here
