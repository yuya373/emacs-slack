;;; slack-stars.el ---                               -*- lexical-binding: t; -*-

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

(defconst slack-stars-list-url "https://slack.com/api/stars.list")

(defclass slack-star ()
  ((paging :initarg :paging :type slack-star-paging)
   (items :initarg :items :type (or null list) :initform nil)))

(defclass slack-star-paging ()
  ((per-page :initarg :per-page :type number)
   (spill :initarg :spill :type number) ;; ??
   (total :initarg :total :type number)
   (page :initarg :page :type number)
   (pages :initarg :pages :type number)))

(defclass slack-star-message ()
  ((channel :initarg :channel :type string)
   (message :initarg :message :type slack-message)))

(defclass slack-star-file ()
  ((file :initarg :file :type slack-file)))

(defclass slack-star-file-comment (slack-star-file)
  ((file-comment :initarg :file-comment :type slack-file-comment)))

(defclass slack-star-channel () ;; Ch ??
  ((channel :initarg :channel :type string))) ;; ID

(defclass slack-star-group () ;; Gh ??
  ((group :initarg :group :type string))) ;; ID

(defclass slack-star-im () ;; Dh ??
  ((channel :initarg :channel :type string))) ;; ID

(defmethod slack-merge ((old slack-star) new)
  (with-slots (paging items) old
    (setq paging (oref new paging))
    (setq items (append (oref new items) items))))

(defmethod slack-to-string ((this slack-star-message) team)
  (slack-message-to-string (oref this message) team))

(defmethod slack-to-string ((this slack-star-file-comment) team)
  (slack-message-to-string (oref this file-comment) team))

(defmethod slack-to-string ((this slack-star-file) team)
  (slack-message-to-string (oref this file) team))

(defun slack-create-star-paging (payload)
  ;; (:per_page 20 :spill 0 :page 1 :total 61 :pages 4)
  (make-instance 'slack-star-paging
                 :per-page (plist-get payload :per_page)
                 :spill (plist-get payload :spill)
                 :page (plist-get payload :page)
                 :total (plist-get payload :total)
                 :pages (plist-get payload :pages)
                 ))

(defun slack-create-star-items (payload team)
  (mapcar #'(lambda (e) (slack-create-star-item e team))
          payload))

(defun slack-create-star-item (payload team)
  (let ((type (plist-get payload :type)))
    (cond
     ((string= type "message")
      (make-instance 'slack-star-message
                     :channel (plist-get payload :channel)
                     :message (slack-message-create (plist-get payload :message)
                                                    team)))
     ((string= type "file")
      (make-instance 'slack-star-file
                     :file (slack-file-create (plist-get payload :file))))
     ((string= type "file_comment")
      (make-instance 'slack-star-file-comment
                     :file (slack-file-create (plist-get payload :file))
                     :file-comment (slack-file-comment-create
                                    (plist-get payload :comment)
                                    (plist-get (plist-get payload :file) :id))))
     ((string= type "channel")
      (make-instance 'slack-star-channel
                     :channel (plist-get payload :channel)))
     ((string= type "im")
      (make-instance 'slack-star-im
                     :channel (plist-get payload :channel)))
     ((string= type "group")
      (make-instance 'slack-star-group
                     :group (plist-get payload :group))))))

;; (:per_page 20 :spill 0 :page 1 :total 61 :pages 4)
(defun slack-create-star-paging (payload)
  (make-instance 'slack-star-paging
                 :per-page (plist-get payload :per_page)
                 :spill (plist-get payload :spill)
                 :page (plist-get payload :page)
                 :total (plist-get payload :total)
                 :pages (plist-get payload :pages)))

(defun slack-create-star (payload team)
  (let ((items (slack-create-star-items (plist-get payload :items)
                                        team))
        (paging (slack-create-star-paging (plist-get payload :paging))))
    (make-instance 'slack-star
                   :items (reverse items)
                   :paging paging)))

(defun slack-stars-list-request (team &optional page after-success)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-stars-list-request")
                    (let ((star (slack-create-star data team)))
                      (if (oref team star)
                          (if page
                              (slack-merge (oref team star) star)
                            (oset team star star))
                        (oset team star star)))
                    (if after-success
                        (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-stars-list-url
      team
      :type "POST"
      :data (list (cons "exclude" "Ch,Gh,Dh")
                  (cons "count" "20")
                  (cons "page" (number-to-string (or page 1))))
      :success #'on-success))))


(defun slack-stars-list ()
  (interactive)
  (let* ((team (slack-team-select))
         (buf (slack-buffer-find 'slack-stars-buffer team)))
    (if buf (slack-buffer-display buf)
      (slack-stars-list-request
       team nil
       #'(lambda () (slack-buffer-display (slack-create-stars-buffer team)))))))

(provide 'slack-star)
;;; slack-stars.el ends here
