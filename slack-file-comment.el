;;; slack-file-comment.el ---                        -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-util)
(require 'slack-message)
(require 'slack-file)

(define-derived-mode slack-edit-file-comment-mode
  fundamental-mode
  "Slack Edit File Comment"
  ""
  (slack-buffer-enable-emojify))

(defvar slack-edit-file-comment-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'slack-file-comment-edit-commit)
    keymap))

(defclass slack-file-comment (slack-user-message)
  ((id :initarg :id)
   (file-id :initarg :file_id :type string)
   (created :initarg :created)
   (timestamp :initarg :timestamp)
   (user :initarg :user)
   (is-intro :initarg :is_intro)
   (comment :initarg :comment)
   (channel :initarg :channel)
   (reactions :initarg :reactions type list)
   (is-starred :initarg :is_starred :type boolean :initform nil)))

(defclass slack-file-comment-message (slack-file-message)
  ((comment :initarg :comment :initform nil)))

(defclass slack-file-mention-message (slack-file-message)
  ((user :initarg :user :initform nil)))

(defmethod slack-reaction-delete ((this slack-file-comment) reaction)
  (with-slots (reactions) this
    (setq reactions
          (slack-reaction--delete reactions reaction))))

(defmethod slack-reaction-push ((this slack-file-comment) reaction)
  (push reaction (oref this reactions)))

(defmethod slack-reaction-find ((this slack-file-comment) reaction)
  (slack-reaction--find (oref this reactions) reaction))

(defmethod slack-message-reactions ((this slack-file-comment))
  (oref this reactions))

(defmethod slack-message-reaction-to-string ((m slack-file-comment) team)
  (let ((reactions (oref m reactions)))
    (when reactions
      (slack-format-reactions reactions team))))

(defmethod slack-equalp ((c slack-file-comment) other)
  (string= (oref c id) (oref other id)))

(defmethod slack-merge ((old slack-file-comment) new)
  (slack-merge-list (oref old reactions) (oref new reactions))
  (oset old comment (oref new comment)))

(defmethod slack-message-body ((comment slack-file-comment) team)
  (slack-message-unescape-string (oref comment comment) team))

(defmethod slack-message-to-string ((comment slack-file-comment) team)
  (let ((header (slack-message-header-to-string comment team))
        (body (slack-message-body-to-string comment team))
        (reactions (slack-message-reaction-to-string comment team)))
    (propertize (slack-format-message header body reactions)
                'file-comment-id (oref comment id))))

(defmethod slack-file-id ((file-comment slack-file-comment))
  (oref file-comment file-id))

(defmethod slack-message-star-added ((this slack-file-comment))
  (oset this is-starred t))

(defmethod slack-message-star-removed ((this slack-file-comment))
  (oset this is-starred nil))

(defmethod slack-message-star-api-params ((this slack-file-comment))
  (cons "file_comment" (oref this id)))

(defun slack-file-comment-edit-commit ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
            (message (buffer-substring-no-properties (point-min) (point-max))))
      (slack-buffer-send-message buf message)))

(defun slack-file-comment-process-star-api (url team)
  (slack-if-let* ((file-id slack-current-file-id)
            (file-comment-id (slack-get-file-comment-id)))
      (slack-with-file file-id team
        (slack-with-file-comment file-comment-id file
          (slack-message-star-api-request url
                                          (list (slack-message-star-api-params file-comment))
                                          team)))))

(defmethod slack-message-set-file-comment ((m slack-file-comment-message) payload)
  (let* ((file-id (plist-get (plist-get payload :file) :id))
         (comment (plist-get payload :comment))
         (file-comment (slack-file-comment-create comment file-id)))
    (oset m comment file-comment)
    m))

(defmethod slack-message-sender-name ((m slack-file-comment-message) team)
  (slack-user-name (oref (oref m comment) user) team))

(defmethod slack-message-sender-id ((m slack-file-comment-message))
  (oref (oref m comment) user))

(defmethod slack-message-star-added ((m slack-file-comment-message))
  (slack-message-star-added (oref m comment)))

(defmethod slack-message-star-removed ((m slack-file-comment-message))
  (slack-message-star-removed (oref m comment)))

(defmethod slack-message-star-api-params ((m slack-file-comment-message))
  (slack-message-star-api-params (oref m comment)))

(defmethod slack-reaction-delete ((this slack-file-comment-message) reaction)
  (slack-reaction-delete (oref this comment) reaction))

(defmethod slack-reaction-push ((this slack-file-comment-message) reaction)
  (slack-reaction-push (oref this comment) reaction))

(defmethod slack-reaction-find ((this slack-file-comment-message) reaction)
  (slack-reaction-find (oref this comment) reaction))

(defmethod slack-message-reactions ((this slack-file-comment-message))
  (slack-message-reactions (oref this comment)))

(defmethod slack-message-get-param-for-reaction ((m slack-file-comment-message))
  (cons "file_comment" (oref (oref m comment) id)))

(defmethod slack-file-id ((m slack-file-comment-message))
  (slack-file-id (oref m comment)))

(defmethod slack-message-starred-p ((m slack-file-comment-message))
  (oref (oref m comment) is-starred))

(defmethod slack-message-to-string ((this slack-file-comment-message) team)
  (with-slots (permalink name id page) (oref this file)
    (with-slots (comment) (oref this comment)
      (let* ((face '(:underline t))
             (text (format "commented on %s <%s|open in browser>\n%s"
                           (slack-file-link-info (oref (oref this file) id) name)
                           permalink
                           (format "“ %s" (slack-message-unescape-string comment
                                                                          team))))
             (header (slack-message-header-to-string this team))
             (reactions (slack-message-reaction-to-string this team)))
        (slack-format-message header text reactions)))))

(defmethod slack-message-get-user-id ((m slack-file-comment-message))
  (oref (oref m comment) user))

(defmethod slack-message-edit-type ((_m slack-file-comment-message))
  'edit-file-comment)

(defmethod slack-message-get-text ((m slack-file-comment-message))
  (oref (oref m comment) comment))

(defmethod slack-message-changed--copy ((this slack-file-comment-message) other)
  (when (slack-file-comment-message-p other)
    (let ((changed (call-next-method)))
      (with-slots ((old-comment comment) text) this
        (let ((new-comment (oref other comment)))
          (oset old-comment reactions (oref new-comment reactions))
          (unless (string= (oref old-comment comment) (oref new-comment comment))
            (oset old-comment comment (oref new-comment comment))
            (setq changed t))))
      changed)))

(defmethod slack-ts ((this slack-file-comment))
  (number-to-string (oref this timestamp)))

(defmethod slack-message-update ((this slack-file-comment) file team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             file
                                             team)))
      (progn
        (oset buffer file file)
        (slack-buffer-update buffer (oref this id)))))

(provide 'slack-file-comment)
;;; slack-file-comment.el ends here
