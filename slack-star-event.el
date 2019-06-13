;;; slack-star-event.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019

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
(require 'slack-event)
(require 'slack-message-buffer)

(defclass slack-star-event (slack-event slack-message-event-processable) ())

(cl-defmethod slack-event-update-buffer ((_this slack-star-event) message team)
  (slack-message-replace-buffer message team))

(cl-defmethod slack-event-create-star-item ((this slack-star-event) team &optional file)
  (let* ((payload (oref this payload))
         (item (plist-get payload :item)))
    (slack-create-star-item (plist-put item :file file) team)))

(cl-defmethod slack-event-update-star-item ((this slack-star-event) team &optional file)
  (let ((star-item (slack-event-create-star-item this team file)))
    (slack-event-save-star-item this star-item team)
    (slack-event-update-star-buffer this star-item team)))

(defclass slack-star-added-event (slack-star-event) ())

(cl-defmethod slack-event-save-message ((_this slack-star-added-event) message _team)
  (slack-message-star-added message))

(cl-defmethod slack-event-save-star-item ((_this slack-star-added-event) item team)
  (slack-if-let* ((star (oref team star)))
      (push item (oref star items))))

(cl-defmethod slack-event-update-star-buffer ((_this slack-star-added-event) item team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-stars-buffer team)))
      (with-current-buffer (slack-buffer-buffer buffer)
        (slack-buffer-insert buffer item))))

(defclass slack-star-removed-event (slack-star-event) ())

(cl-defmethod slack-event-save-message ((_this slack-star-removed-event) message _team)
  (slack-message-star-removed message))

(cl-defmethod slack-event-save-star-item ((_this slack-star-removed-event) item team)
  (slack-if-let* ((star (oref team star)))
      (oset star items (cl-remove-if #'(lambda (e) (string= (slack-ts e)
                                                            (slack-ts item)))
                                     (oref star items)))))

(cl-defmethod slack-event-update-star-buffer ((_this slack-star-removed-event) item team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-stars-buffer team)))
      (slack-buffer-message-delete buffer (slack-ts item))))

(defclass slack-message-star-event (slack-star-event) ())
(defclass slack-message-star-added-event (slack-message-star-event slack-star-added-event) ())
(defclass slack-message-star-removed-event (slack-message-star-event slack-star-removed-event) ())

(defclass slack-file-star-event (slack-star-event) ())
(defclass slack-file-star-added-event (slack-file-star-event slack-star-added-event) ())
(defclass slack-file-star-removed-event (slack-file-star-event slack-star-removed-event) ())

(defun slack-create-star-event (payload)
  (let* ((type (plist-get payload :type))
         (item (plist-get payload :item))
         (item-type (plist-get item :type))
         (added-p (string= "star_added" type)))
    (cond ((string= "message" item-type)
           (if added-p
               (slack-create-message-star-added-event payload)
             (slack-create-message-star-removed-event payload)))
          ((string= "file" item-type)
           (if added-p
               (slack-create-file-star-added-event payload)
             (slack-create-file-star-removed-event payload))))))

(defun slack-create-message-star-added-event (payload)
  (slack-message-star-added-event :type (plist-get payload :type)
                                  :payload payload))

(defun slack-create-message-star-removed-event (payload)
  (slack-message-star-removed-event :type (plist-get payload :type)
                                    :payload payload))

(defun slack-create-file-star-added-event (payload)
  (slack-file-star-added-event :type (plist-get payload :type)
                               :payload payload))

(defun slack-create-file-star-removed-event (payload)
  (slack-file-star-removed-event :type (plist-get payload :type)
                                 :payload payload))

(cl-defmethod slack-event-find-message ((this slack-message-star-event) team)
  (let* ((payload (oref this payload))
         (item (plist-get payload :item))
         (channel (plist-get item :channel))
         (room (slack-room-find channel team))
         (message (plist-get item :message))
         (ts (plist-get message :ts)))
    (when room
      (slack-room-find-message room ts))))

(cl-defmethod slack-event-update ((this slack-message-star-event) team)
  (cl-call-next-method)
  (slack-event-update-star-item this team))

(cl-defmethod slack-event-find-message ((this slack-file-star-event) team)
  (let* ((payload (oref this payload))
         (item (plist-get payload :item))
         (file (plist-get item :file))
         (id (plist-get file :id)))
    (slack-file-find id team)))

(cl-defmethod slack-event-update ((this slack-file-star-event) team)
  (let ((file (slack-event-find-message this team)))
    (cl-labels
        ((update (&rest _args)
                 (let ((file (slack-event-find-message this team)))
                   (slack-event-save-message this file team)
                   (slack-event-update-ui this file team)
                   (slack-event-update-star-item this team file))))
      (if file (update)
        (let* ((payload (oref this payload))
               (item (plist-get payload :item))
               (file (plist-get item :file))
               (id (plist-get file :id)))
          (slack-file-request-info id 1 team #'update))))))

(provide 'slack-star-event)
;;; slack-star-event.el ends here
