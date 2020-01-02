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
(require 'slack-util)
(require 'slack-request)
(require 'slack-team)
(require 'slack-file)
(require 'slack-buffer)

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

(defclass slack-star-item ()
  ((date-create :initarg :date-create :type string)))

(defclass slack-star-message (slack-star-item)
  ((channel :initarg :channel :type string)
   (message :initarg :message :type slack-message)))

(defclass slack-star-file (slack-star-item)
  ((file :initarg :file :type slack-file)))

(defclass slack-star-channel (slack-star-item) ;; Ch ??
  ((channel :initarg :channel :type string))) ;; ID

(defclass slack-star-group (slack-star-item) ;; Gh ??
  ((group :initarg :group :type string))) ;; ID

(defclass slack-star-im (slack-star-item) ;; Dh ??
  ((channel :initarg :channel :type string))) ;; ID

(cl-defmethod slack-star-item-file ((_this slack-star-item) &rest _args)
  nil)

(cl-defmethod slack-star-item-file ((this slack-star-message) file-id)
  (let ((message (oref this message)))
    (cl-find-if #'(lambda (e) (string= file-id (slack-file-id e)))
                (oref message files))))

(cl-defmethod slack-star-item-file ((this slack-star-file) &rest _args)
  (oref this file))

(cl-defmethod slack-star-item-message ((this slack-star-message))
  (oref this message))

(cl-defmethod slack-star-item-message ((this slack-star-file))
  (oref this file))

(cl-defmethod slack-ts ((this slack-star-item))
  (oref this date-create))

(cl-defmethod slack-next-page ((this slack-star-paging))
  (with-slots (pages page) this
    (unless (< pages (1+ page))
      (1+ page))))

(cl-defmethod slack-star-has-next-page-p ((this slack-star))
  (slack-next-page (oref this paging)))

(cl-defmethod slack-per-page ((this slack-star-paging))
  (oref this per-page))

(cl-defmethod slack-star-per-page ((this slack-star))
  (slack-per-page (oref this paging)))

(cl-defmethod slack-star-items ((this slack-star))
  (oref this items))

(cl-defmethod slack-merge ((old slack-star) new)
  (with-slots (paging items) old
    (setq paging (oref new paging))
    (setq items (append (oref new items) items))))

(cl-defmethod slack-to-string ((this slack-star-message) team)
  (with-slots (message) this
    (slack-message-to-string message team)))

(cl-defmethod slack-to-string ((this slack-star-file) team)
  (with-slots (file) this
    (slack-message-to-string file (slack-ts this) team)))

(defun slack-create-star-paging (payload)
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
  (let* ((type (plist-get payload :type))
         (date-create (format "%s" (plist-get payload :date_create)))
         (file-payload (plist-get payload :file))
         (file (and file-payload
                    (if (or (slack-file-p file-payload)
                            (slack-file-email-p file-payload))
                        file-payload
                      (slack-file-create file-payload)))))
    (cond
     ((string= type "message")
      (make-instance 'slack-star-message
                     :date-create date-create
                     :channel (plist-get payload :channel)
                     :message (slack-message-create (plist-get payload :message)
                                                    team
                                                    (plist-get payload :channel))))
     ((string= type "file")
      (make-instance 'slack-star-file
                     :date-create date-create
                     :file file))
     ((string= type "channel")
      (make-instance 'slack-star-channel
                     :date-create date-create
                     :channel (plist-get payload :channel)))
     ((string= type "im")
      (make-instance 'slack-star-im
                     :date-create date-create
                     :channel (plist-get payload :channel)))
     ((string= type "group")
      (make-instance 'slack-star-group
                     :date-create date-create
                     :group (plist-get payload :group))))))

(defun slack-create-star (payload team)
  (let ((items (slack-create-star-items (plist-get payload :items)
                                        team))
        (paging (slack-create-star-paging (plist-get payload :paging))))
    (make-instance 'slack-star
                   :items (reverse items)
                   :paging paging)))

(defun slack-stars-list-request (team &optional page after-success)
  (cl-labels
      ((callback ()
                 (when (functionp after-success)
                   (funcall after-success)))
       (on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-stars-list-request")
                    (let* ((star (slack-create-star data team))
                           (user-ids (slack-team-missing-user-ids
                                      team (cl-loop for item in (oref star items)
                                                    if (slack-star-message-p item)
                                                    nconc (slack-message-user-ids
                                                           (oref item message))))))
                      (if (oref team star)
                          (if page
                              (slack-merge (oref team star) star)
                            (oset team star star))
                        (oset team star star))
                      (if (< 0 (length user-ids))
                          (slack-users-info-request
                           user-ids team
                           :after-success #'(lambda () (callback)))
                        (callback))))))
    (slack-request
     (slack-request-create
      slack-stars-list-url
      team
      :type "POST"
      :data (list (cons "exclude" "Ch,Gh,Dh")
                  (cons "count" "20")
                  (cons "page" (number-to-string (or page 1))))
      :success #'on-success))))


(cl-defmethod slack-message-star-api-params ((this slack-star-item))
  (list (slack-message-star-api-params (slack-star-item-message this))))

(cl-defmethod slack-message-star-api-params ((this slack-star-message))
  (append (list (cons "channel" (oref this channel)))
          (cl-call-next-method)))

(defun slack-star-api-request (url params team)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data url))))
    (slack-request
     (slack-request-create
      url
      team
      :params params
      :success #'on-success))))

(cl-defmethod slack-star-remove-star ((this slack-star) ts team)
  (slack-if-let* ((item (cl-find-if #'(lambda (e) (string= (oref e date-create) ts))
                                    (oref this items))))
      (slack-star-api-request slack-message-stars-remove-url
                                      (slack-message-star-api-params item)
                                      team)))


(provide 'slack-star)
;;; slack-star.el ends here
