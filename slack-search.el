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

(defface slack-search-result-message-header-face
  '((t (:weight bold :height 1.1 :underline t)))
  "Face used to search message header."
  :group 'slack)
;; (:inherit (markdown-code-face font-lock-constant-face))
(defface slack-search-result-message-username-face
  '((t (:inherit slack-message-output-header :underline nil)))
  ""
  :group 'slack)

(defclass slack-search-paging ()
  ((count :initarg :count :type number)
   (total :initarg :total :type number)
   (page :initarg :page :type number)
   (pages :initarg :pages :type number)))

(defclass slack-search-result (slack-room)
  ((query :initarg :query :type string)
   (sort :initarg :sort :type string)
   (sort-dir :initarg :sort-dir :type string)
   (total :initarg :total :type number)
   (matches :initarg :matches :initform nil :type (or null list))
   (paging :initarg :paging :type slack-search-paging)))

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
  ((user :initarg :user :type string)
   (username :initarg :username :type string)
   (text :initarg :text :type string)
   (ts :initarg :ts :type string)
   (type :initarg :type :type string)))

(defmethod slack-merge ((this slack-search-result) other)
  (oset this query (oref other query))
  (oset this sort (oref other sort))
  (oset this sort-dir (oref other sort-dir))
  (oset this total (oref other total))
  (oset this matches (append (oref this matches) (oref other matches)))
  (oset this paging (oref other paging)))

(defmethod slack-message-to-string ((this slack-search-message) team)
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

(defmethod slack-ts ((this slack-search-message))
  (slack-ts (oref this message)))

(defmethod slack-search-has-next-page-p ((this slack-search-result))
  (slack-search-paging-next-page (oref this paging)))

(defmethod slack-search-paging-next-page ((this slack-search-paging))
  (with-slots (pages page) this
    (unless (< pages (1+ page))
      (1+ page))))

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
                   :message (slack-message-create payload team :room room)
                   :channel channel
                   :previous-2 previous-2
                   :previous previous
                   :next next
                   :next-2 next-2)))

(defun slack-search-create-paging (payload)
  (and payload
       (make-instance 'slack-search-paging
                      :count (plist-get payload :count)
                      :total (plist-get payload :total)
                      :page (plist-get payload :page)
                      :pages (plist-get payload :pages))))

(defun slack-search-create-result (payload sort sort-dir team)
  (let* ((messages (plist-get payload :messages))
         (matches (mapcar #'(lambda (e) (slack-search-create-message e team))
                          (plist-get messages :matches)))
         (paging (slack-search-create-paging
                  (plist-get messages :paging))))
    (make-instance 'slack-search-result
                   :query (plist-get payload :query)
                   :total (plist-get messages :total)
                   :paging paging
                   :matches matches
                   :sort sort
                   :sort-dir sort-dir)))

(defun slack-search-create-file-result (payload sort sort-dir)
  (let* ((files (plist-get payload :files))
         (matches (mapcar #'slack-file-create
                          (plist-get files :matches)))
         (paging (slack-search-create-paging
                  (plist-get files :paging))))
    (make-instance 'slack-search-result
                   :query (plist-get payload :query)
                   :total (plist-get files :total)
                   :paging paging
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

(defun slack-search-from-messages ()
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((instance (make-instance 'slack-search-result
                                   :sort sort
                                   :sort-dir sort-dir
                                   :query query)))
      (cl-labels
          ((after-success ()
                          (let ((buffer (slack-create-search-result-buffer instance team)))
                            (slack-buffer-display buffer))))
        (slack-search-request instance #'after-success team)))))

(defun slack-search-from-files ()
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((instance (make-instance 'slack-file-search-result
                                   :sort sort
                                   :sort-dir sort-dir
                                   :query query)))
      (cl-labels
          ((after-success ()
                          (let ((buffer (slack-create-search-result-buffer instance team)))
                            (slack-buffer-display buffer))))
        (slack-search-request instance #'after-success team)))))

(defmethod slack-search-request-url ((_this slack-search-result))
  "https://slack.com/api/search.messages")

(defmethod slack-search-request-url ((_this slack-file-search-result))
  "https://slack.com/api/search.files")

(cl-defmethod slack-search-request ((this slack-search-result)
                                    after-success team &optional (page 1))
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-search-request")
                    (let ((search-result
                           (if (slack-file-search-result-p this)
                               (slack-search-create-file-result data
                                                                (oref this sort)
                                                                (oref this sort-dir))
                             (slack-search-create-result data
                                                         (oref this sort)
                                                         (oref this sort-dir)
                                                         team))))
                      (slack-merge this search-result)
                      (funcall after-success)))))
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


(provide 'slack-search)
;;; slack-search.el ends here
