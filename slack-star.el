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

(defmethod slack-star-item-message ((this slack-star-message))
  (oref this message))

(defmethod slack-star-item-message ((this slack-star-file))
  (oref this file))

(defmethod slack-ts ((this slack-star-item))
  (oref this date-create))

(defmethod slack-next-page ((this slack-star-paging))
  (with-slots (pages page) this
    (unless (< pages (1+ page))
      (1+ page))))

(defmethod slack-star-has-next-page-p ((this slack-star))
  (slack-next-page (oref this paging)))

(defmethod slack-per-page ((this slack-star-paging))
  (oref this per-page))

(defmethod slack-star-per-page ((this slack-star))
  (slack-per-page (oref this paging)))

(defmethod slack-star-items ((this slack-star))
  (oref this items))

(defmethod slack-merge ((old slack-star) new)
  (with-slots (paging items) old
    (setq paging (oref new paging))
    (setq items (append (oref new items) items))))

(defmethod slack-to-string ((this slack-star-message) team)
  (with-slots (message) this
    (slack-message-to-string message team)))

(defmethod slack-to-string ((this slack-star-file) team)
  (with-slots (date-create file) this
    (slack-message-to-string file date-create team)))

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
  (let* ((type (plist-get payload :type))
         (date-create (format "%s" (plist-get payload :date_create)))
         (file-payload (plist-get payload :file))
         (file (and file-payload
                    (if (or (slack-file-p file-payload)
                            (slack-file-email-p file-payload))
                        file-payload
                      (slack-file-create file-payload))))
         (file-id (and file (oref file id))))
    (cond
     ((string= type "message")
      (make-instance 'slack-star-message
                     :date-create date-create
                     :channel (plist-get payload :channel)
                     :message (slack-message-create (plist-get payload :message)
                                                    team)))
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

(defmethod slack-message-star-api-params ((this slack-star-item))
  (list (slack-message-star-api-params (slack-star-item-message this))))

(defmethod slack-message-star-api-params ((this slack-star-message))
  (append (list (cons "channel" (oref this channel)))
          (call-next-method)))

(defmethod slack-star-remove-star ((this slack-star) ts team)
  (slack-if-let* ((item (cl-find-if #'(lambda (e) (string= (oref e date-create) ts))
                              (oref this items))))
      (slack-message-star-api-request slack-message-stars-remove-url
                                      (slack-message-star-api-params item)
                                      team)))

(defmethod slack-star-remove ((this slack-star) payload team)
  (let ((date-create (format "%s" (plist-get payload :date_create))))
    (oset this items (cl-remove-if #'(lambda (e) (string= (slack-ts e)
                                                          date-create))
                                   (oref this items)))
    (slack-if-let* ((buffer (slack-buffer-find 'slack-stars-buffer team)))
        (slack-buffer-message-delete buffer date-create))))

(defmethod slack-star-add ((this slack-star) payload team)
  (setq payload (append payload nil))
  (cl-labels
      ((create-star (payload)
                    (slack-create-star-item payload team))
       (append-star-item (item)
                         (oset this items (append (oref this items) (list item))))
       (insert-to-buffer (item)
                         (slack-if-let* ((buffer (slack-buffer-find 'slack-stars-buffer
                                                                    team)))
                             (with-current-buffer (slack-buffer-buffer buffer)
                               (slack-buffer-insert buffer item)))))
    (if (plist-get payload :file)
        (cl-labels
            ((insert-star (payload file)
                          (let ((item (create-star (plist-put payload :file file))))
                            (append-star-item item)
                            (insert-to-buffer item))))
          (let ((file-id (plist-get (plist-get payload :file) :id)))
            (slack-if-let* ((file (slack-file-find file-id team)))
                (insert-star payload file)
              (slack-file-request-info file-id 1 team
                                       #'(lambda (file _team)
                                           (insert-star payload file))))))
      (let ((item (create-star payload)))
        (append-star-item item)
        (insert-to-buffer item)))))

(provide 'slack-star)
;;; slack-stars.el ends here
