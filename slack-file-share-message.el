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
(require 'slack-team)
(require 'slack-message)

(defclass slack-file-share-message (slack-file-message)
  ((upload :initarg :upload)
   (user :initarg :user :initform nil)))


(defmethod slack-message-image-to-string ((m slack-file-share-message) team)
  (if (slack-team-display-image-inlinep m team)
      (slack-message-render-image m team)
    (slack-message-view-image-to-string m team)))

(cl-defmethod slack-image-create ((m slack-file-share-message) &key success error token)
  (slack-image-create (oref m file) :success success :error error :token token))

(defmethod slack-team-display-image-inlinep ((m slack-file-share-message) team)
  (slack-team-display-image-inlinep (oref m file) team))

(defmethod slack-message-has-imagep ((m slack-file-share-message))
  (slack-message-has-imagep (oref m file)))

(defmethod slack-open-image ((m slack-file-share-message) team)
  (slack-open-image (oref m file) team))

(defmethod slack-message-to-string ((m slack-file-share-message) team)
  (cl-labels
      ((redisplay () (slack-message-redisplay m (slack-room-find (oref m channel) team))))
    (let* ((header (slack-message-header-to-string m team))
           (attachment-body (slack-message-attachment-body m team))
           (body (slack-file-summary (oref m file)))
           (thumb (slack-message-image-to-string m team))
           (reactions (slack-message-reaction-to-string m))
           (thread (slack-thread-to-string m team))
           (initial-comment (if-let* ((initial-comment
                                       (oref (oref m file) initial-comment)))
                                (format "\n“ %s\n%s"
                                        (slack-message-body initial-comment team)
                                        (slack-message-reaction-to-string
                                         initial-comment))
                              "")))
      (slack-format-message header body attachment-body
                            thumb reactions initial-comment thread))))

(defmethod slack-message-get-param-for-reaction ((m slack-file-share-message))
  (cons "file" (oref (oref m file) id)))

(defmethod slack-message-star-added ((m slack-file-share-message))
  (oset (oref m file) is-starred t))

(defmethod slack-message-star-removed ((m slack-file-share-message))
  (oset (oref m file) is-starred nil))

(defmethod slack-message-star-api-params ((m slack-file-share-message))
  (cons "file" (oref (oref m file) id)))

(defmethod slack-message-changed--copy ((this slack-file-share-message) other)
  (let ((changed (call-next-method)))
    (with-slots (file) this
      (with-slots (comments initial-comment) file
        (let ((new-comments (oref (oref other file) comments)))
          (unless (or (eq (length comments)
                          (length new-comments))
                      (cl-find-if #'(lambda (c)
                                      (let ((old (cl-find-if #'(lambda (o)
                                                                 (slack-equalp o c))
                                                             comments)))
                                        (if old
                                            (not (string= (oref old comment)
                                                          (oref c comment)))
                                          t)))
                                  new-comments))
            (setq comments new-comments)
            (setq changed t)))
        (let ((new-initial-comment (oref (oref other file) initial-comment)))
          (when (or (and (not initial-comment) new-initial-comment)
                    (and (and initial-comment new-initial-comment)
                         (not (string= (oref initial-comment comment)
                                       (oref new-initial-comment comment)))))
            (setq initial-comment new-initial-comment)
            (setq changed t)))))
    changed))

(provide 'slack-file-share-message)
;;; slack-file-share-message.el ends here
