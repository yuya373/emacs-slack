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
                                       (slack-room-name room))
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

;; (defun slack-search-alist (team)
;;   (with-slots (search-results) team
;;     (cl-loop for s in search-results
;;              collect (cons (slack-room-buffer-name s) s))))

;; (defun slack-search-select ()
;;   (interactive)
;;   (let* ((team (slack-team-select))
;;          (alist (slack-search-alist team)))
;;     (slack-select-from-list
;;         (alist "Select Search: ")
;;         (funcall slack-buffer-function (slack-buffer-create selected team)))))

;; protocols
;; (defmethod slack-room-sorted-messages ((room slack-search-result))
;;   (copy-sequence (oref room messages)))

;; (defmethod slack-room-update-last-read ((room slack-search-result) msg)
;;   (if (not (slot-exists-p msg 'info))
;;       (progn
;;         (oset room last-read (oref msg ts))
;;         (oset room last-channel-id ""))
;;     (with-slots (ts info) msg
;;       (with-slots (channel-id) info
;;         (when (slack-room-update-last-read-p room ts)
;;           (oset room last-read ts)
;;           (oset room last-channel-id channel-id))))))

;; (defmethod slack-search-get-index ((_search-result slack-file-search-result)
;;                                    messages last-read &optional _last-chanel-id)
;;   (cl-loop for i from 0 upto (1- (length messages))
;;            for m = (nth i messages)
;;            if (string= (oref m ts) last-read)
;;            return i))

;; (defmethod slack-search-get-index ((_search-result slack-search-result)
;;                                    messages last-read &optional last-channel-id)
;;   (cl-loop for i from 0 upto (1- (length messages))
;;            for m = (nth i messages)
;;            if (and (string= (oref m ts) last-read)
;;                    (string= (oref (oref m info) channel-id)
;;                             last-channel-id))
;;            return i))

;; (defmethod slack-room-latest-messages ((room slack-search-result) messages)
;;   (with-slots (type last-read last-channel-id) room
;;     (let* ((r-messages (reverse messages))
;;            (nth (slack-search-get-index room r-messages
;;                                         last-read last-channel-id)))
;;       (if nth
;;           (nreverse
;;            (nthcdr (1+ nth) r-messages))
;;         (copy-sequence messages)))))

;; (defmethod slack-room-prev-messages ((room slack-file-search-result) oldest)
;;   (let* ((messages (reverse (oref room messages)))
;;          (nth (slack-search-get-index room messages oldest)))
;;     (if nth
;;         (nreverse (nthcdr (1+ nth) messages)))))

;; (defmethod slack-room-prev-messages ((room slack-search-result) param)
;;   (let* ((oldest (car param))
;;          (channel-id (cdr param))
;;          (messages (reverse (oref room messages)))
;;          (nth (slack-search-get-index room messages
;;                                       oldest channel-id)))
;;     (if nth
;;         (nreverse (nthcdr (1+ nth) messages)))))

;; (defmethod slack-message-equal ((m slack-search-message) n)
;;   (with-slots ((m-info info) (m-ts ts)) m
;;     (with-slots ((m-channel-id channel-id)) m-info
;;       (with-slots ((n-info info) (n-ts ts)) n
;;         (with-slots ((n-channel-id channel-id)) n-info
;;           (and (string= m-channel-id n-channel-id)
;;                (string= m-ts n-ts)))))))

;; (defmethod slack-room-buffer-name ((room slack-search-result))
;;   (with-slots (query sort sort-dir team-id type) room
;;     (let ((team (slack-team-find team-id)))
;;       (format "%s - %s Query: %s Sort: %s Order: %s"
;;               (oref team name)
;;               (eieio-object-class room)
;;               query sort sort-dir))))

;; (defmethod slack-message-to-string ((message slack-search-message) team)
;;   (with-slots (info text username) message
;;     (with-slots (channel-id permalink) info
;;       (let* ((header (format "%s" username))
;;              (channel (slack-room-find channel-id team))
;;              (body (slack-message-unescape-string
;;                     (format "%s\n\n------------\nChanel: %s\nPermalink: %s"
;;                             text
;;                             (slack-room-name channel)
;;                             permalink)
;;                     team)))
;;         (slack-message-put-header-property header)
;;         (slack-message-put-text-property body)
;;         (format "%s\n%s\n" header body)))))

;; (defmethod slack-room-set-prev-messages ((room slack-search-result) prev)
;;   (slack-room-set-messages room (nreverse
;;                                  (nconc (nreverse prev) (oref room messages)))))

;; (defmethod slack-room-set-messages ((room slack-search-result) messages)
;;   (let ((msgs (nreverse messages)))
;;     (oset room messages msgs)
;;     (oset room latest (car (last msgs)))
;;     (oset room oldest (car msgs))))

;; (cl-defmethod slack-room-history-request ((room slack-search-result) team
;;                                           &key
;;                                           oldest after-success async)
;;   (cl-labels
;;       ((on-history
;;         (&key data &allow-other-keys)
;;         (slack-request-handle-error
;;          (data "slack-room-history")
;;          (let* ((matches (cl-case (eieio-object-class room)
;;                            ('slack-search-result (plist-get data :messages))
;;                            ('slack-file-search-result (plist-get data :files))))
;;                 (messages (cl-loop
;;                            for match in (plist-get matches :matches)
;;                            collect (slack-search-create-message room match))))
;;            (oset room current-page
;;                  (plist-get (plist-get matches :paging) :page))
;;            (if oldest
;;                (slack-room-set-prev-messages room messages)
;;              (slack-room-reset-last-read room)
;;              (slack-room-set-messages room messages))
;;            (if after-success
;;                (funcall after-success))))))
;;     (let* ((current-page (oref room current-page))
;;            (total-page (oref room total-page))
;;            (next-page (if oldest
;;                           (1+ current-page)
;;                         1)))
;;       (with-slots (query sort sort-dir) room
;;         (cl-case (eieio-object-class room)
;;           ('slack-search-result
;;            (slack-search-request-message team
;;                                          query
;;                                          sort
;;                                          sort-dir
;;                                          #'on-history
;;                                          next-page
;;                                          async))
;;           ('slack-file-search-result
;;            (slack-search-request-file team
;;                                       query
;;                                       sort
;;                                       sort-dir
;;                                       #'on-history
;;                                       next-page
;;                                       async)))))))

(provide 'slack-search)
;;; slack-search.el ends here
