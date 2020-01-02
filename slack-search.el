;;; slack-search.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
(require 'slack-request)
(require 'slack-channel)
(require 'slack-file)
(require 'slack-team)

(defvar slack-completing-read-function)

(defface slack-search-result-message-header-face
  '((t (:weight bold :height 1.1 :underline t)))
  "Face used to search message header."
  :group 'slack)
;; (:inherit (markdown-code-face font-lock-constant-face))
(defface slack-search-result-message-username-face
  '((t (:inherit slack-message-output-header :underline nil)))
  ""
  :group 'slack)

(defclass slack-search-pagination ()
  ((total-count :initarg :total_count :type number)
   (page :initarg :page :type number)
   (per-page :initarg :per_page :type number)
   (page-count :initarg :page_count :type number)
   (first :initarg :first :type number)
   (last :initarg :last :type number)))

(defclass slack-search-result (slack-room)
  ((query :initarg :query :type string)
   (sort :initarg :sort :type string)
   (sort-dir :initarg :sort-dir :type string)
   (total :initarg :total :type number)
   (matches :initarg :matches :initform nil :type (or null list))
   (pagination :initarg :pagination :type slack-search-pagination)))

(defclass slack-file-search-result (slack-search-result) ())

(defclass slack-search-message ()
  ((message :initarg :message :type slack-message)

   (channel :initarg :channel :type slack-search-message-channel)
   (user :initarg :user :type string)
   (username :initarg :username :type string)
   (permalink :initarg :permalink :type string)

   (previous-2 :initarg :previous-2 :type (or null slack-search-message-around-message) :initform nil)
   (previous :initarg :previous :type (or null slack-search-message-around-message) :initform nil)
   (next :initarg :next :type (or null slack-search-message-around-message) :initform nil)
   (next-2 :initarg :next-2 :type (or null slack-search-message-around-message) :initform nil)))

(defclass slack-search-message-channel ()
  ((id :initarg :id :type string)
   (name :initarg :name :type string)))

(defclass slack-search-message-around-message ()
  ((user :initarg :user :type (or null string))
   (username :initarg :username :type string)
   (text :initarg :text :type string)
   (ts :initarg :ts :type string)
   (type :initarg :type :type string)))

(cl-defmethod slack-merge ((this slack-search-result) other)
  (oset this query (oref other query))
  (oset this sort (oref other sort))
  (oset this sort-dir (oref other sort-dir))
  (oset this total (oref other total))
  (oset this matches (append (oref this matches) (oref other matches)))
  (oset this pagination (oref other pagination)))

(cl-defmethod slack-message-to-string ((this slack-search-message) team)
  (with-slots (channel username) this
    (let* ((room (slack-room-find (oref channel id) team))
           (header (propertize (format "%s%s"
                                       (if (slack-channel-p room)
                                           "#" "@")
                                       (slack-room-name room team))
                               'face 'slack-search-result-message-header-face)))
      (propertize (format "%s\n%s"
                          header
                          (slack-message-to-string (oref this message) team))
                  'ts (slack-ts this)))))

(cl-defmethod slack-ts ((this slack-search-message))
  (slack-ts (oref this message)))

(cl-defmethod slack-search-has-next-page-p ((this slack-search-result))
  (slack-search-paging-next-page (oref this pagination)))

(cl-defmethod slack-search-paging-next-page ((this slack-search-pagination))
  (with-slots (page-count page) this
    (min (1+ page) page-count)))

(defun slack-search-create-message-channel (payload)
  (and payload
       (make-instance 'slack-search-message-channel
                      :id (plist-get payload :id)
                      :name (plist-get payload :name))))

(defun slack-search-create-around-message (payload)
  (and payload
       (make-instance 'slack-search-message-around-message
                      :user (plist-get payload :user)
                      :username (plist-get payload :username)
                      :text (plist-get payload :text)
                      :ts (plist-get payload :ts)
                      :type (plist-get payload :type))))

(defun slack-search-create-message (payload team)
  (setq payload (append payload nil))
  (let* ((channel (slack-search-create-message-channel
                   (plist-get payload :channel)))
         (previous-2 (slack-search-create-around-message
                      (plist-get payload :previous_2)))
         (previous (slack-search-create-around-message
                    (plist-get payload :previous)))
         (next (slack-search-create-around-message
                (plist-get payload :next)))
         (next-2 (slack-search-create-around-message
                  (plist-get payload :next_2)))
         (room (slack-room-find (oref channel id) team)))

    (unless (< 0 (length (plist-get payload :user)))
      (plist-put payload :user nil)
      (plist-put payload :subtype "bot_message"))

    (make-instance 'slack-search-message
                   :message (slack-message-create payload team room)
                   :channel channel
                   :previous-2 previous-2
                   :previous previous
                   :next next
                   :next-2 next-2)))

(defun slack-search-create-pagination (payload)
  (and payload
       (make-instance 'slack-search-pagination
                      :total_count (plist-get payload :total_count)
                      :page (plist-get payload :page)
                      :per_page (plist-get payload :per_page)
                      :page_count (plist-get payload :page_count)
                      :first (plist-get payload :first)
                      :last (plist-get payload :last))))

(defun slack-search-create-result (payload sort sort-dir team)
  (let* ((messages (plist-get payload :messages))
         (matches (mapcar #'(lambda (e) (slack-search-create-message e team))
                          (plist-get messages :matches)))
         (pagination (slack-search-create-pagination (plist-get messages :pagination))))
    (make-instance 'slack-search-result
                   :query (plist-get payload :query)
                   :total (plist-get messages :total)
                   :pagination pagination
                   :matches matches
                   :sort sort
                   :sort-dir sort-dir)))

(defun slack-search-create-file-result (payload sort sort-dir)
  (let* ((files (plist-get payload :files))
         (matches (mapcar #'slack-file-create
                          (plist-get files :matches)))
         (pagination (slack-search-create-pagination (plist-get files :pagination))))
    (make-instance 'slack-search-result
                   :query (plist-get payload :query)
                   :total (plist-get files :total)
                   :pagination pagination
                   :matches matches
                   :sort sort
                   :sort-dir sort-dir)))

(defun slack-search-query-params ()
  (let ((team (slack-team-select))
        (query (read-from-minibuffer "Query: "))
        (sort (funcall slack-completing-read-function "Sort: " `("score" "timestamp")
                       nil t))
        (sort-dir (funcall slack-completing-read-function "Direction: " `("desc" "asc")
                           nil t)))
    (list team query sort sort-dir)))

(cl-defmethod slack-search-request-url ((_this slack-search-result))
  "https://slack.com/api/search.messages")

(cl-defmethod slack-search-request-url ((_this slack-file-search-result))
  "https://slack.com/api/search.files")

(cl-defmethod slack-search-request ((this slack-search-result)
                                    after-success team &optional (page 1))
  (cl-labels
      ((callback ()
                 (funcall after-success))
       (on-success
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-search-request")
         (let* ((search-result (if (slack-file-search-result-p this)
                                   (slack-search-create-file-result data
                                                                    (oref this sort)
                                                                    (oref this sort-dir))
                                 (slack-search-create-result data
                                                             (oref this sort)
                                                             (oref this sort-dir)
                                                             team)))
                (user-ids (slack-team-missing-user-ids
                           team (cl-loop for match in (oref search-result matches)
                                         nconc (slack-message-user-ids match)))))
           (slack-merge this search-result)
           (if (< 0 (length user-ids))
               (slack-users-info-request
                user-ids team :after-success #'(lambda () (callback)))
             (callback))))))
    (with-slots (query sort sort-dir) this
      (if (< 0 (length query))
          (slack-request
           (slack-request-create
            (slack-search-request-url this)
            team
            :type "POST"
            :params (list (cons "query" query)
                          (cons "sort" sort)
                          (cons "sort_dir" sort-dir)
                          (cons "page" (number-to-string page)))
            :success #'on-success))))))

(cl-defmethod slack-message-user-ids ((this slack-search-message))
  (with-slots (message) this
    (slack-message-user-ids message)))


(provide 'slack-search)
;;; slack-search.el ends here
