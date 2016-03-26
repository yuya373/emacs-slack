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

(defclass slack-search-result (slack-room)
  ((type :initarg :type :type symbol)
   (query :initarg :query :type string)
   (per-page :initarg :per-page :type integer)
   (total-page :initarg :total-page :type integer)
   (current-page :initarg :current-page :type integer)
   (total-messages :initarg :total-messages :type integer)
   (sort :initarg :sort :type string)
   (sort-dir :initarg :sort-dir :type string)
   (last-channel-id :initarg :last-channel-id :type string :initform "")))

(defclass slack-file-search-result (slack-search-result) ())

(defclass slack-search-message ()
  ((user-id :initarg :user-id :type string)
   (username :initarg :username :type string)
   (ts :initarg :ts :type string)
   (text :initarg :text :type string)
   (previous-2 :initarg :previous-2)
   (previous :initarg :previous)
   (next :initarg :next)
   (next-2 :initarg :next-2)
   (info :initarg :info)))

(defclass slack-search-message-info ()
  ((channel-id :initarg :channel-id :type string)
   (channel-name :initarg :channel-name :type string)
   (permalink :initarg :permalink :type string :initform "")
   (result-id :initarg :result-id :type string)))

(defun slack-search-result-id (type query sort sort-dir)
  (format "Q%s%s%s%s" type query sort sort-dir))

(defun slack-search-create-message-info (payload)
  (let ((channel (plist-get payload :channel)))
    (make-instance 'slack-search-message-info
                   :channel-id (plist-get channel :id)
                   :channel-name (plist-get channel :name)
                   :permalink (plist-get payload :permalink))))

(defmethod slack-search-create-message ((room slack-search-result) payload)
  (cl-labels ((create-message
               (params info)
               (let ((previous-2 (if (plist-get params :previous_2)
                                     (create-message (plist-get params :previous_2)
                                                     info)))
                     (previous (if (plist-get params :previous)
                                   (create-message (plist-get params :previous)
                                                   info)))
                     (next (if (plist-get params :next)
                               (create-message (plist-get params :next)
                                               info)))
                     (next-2 (if (plist-get params :next_2)
                                 (create-message (plist-get params :next_2)
                                                 info))))
                 (make-instance 'slack-search-message
                                :info info
                                :user-id (plist-get params :user)
                                :username (plist-get params :username)
                                :text (plist-get params :text)
                                :ts (plist-get params :ts)
                                :previous-2 previous-2
                                :previous previous
                                :next next
                                :next-2 next-2)))
              (create-info
               (params result)
               (let ((channel (plist-get params :channel)))
                 (make-instance 'slack-search-message-info
                                :result-id (oref result id)
                                :channel-id (plist-get channel :id)
                                :channel-name (plist-get channel :name)
                                :permalink (plist-get params :permalink)))))
    (let ((info (create-info payload room)))
      (create-message payload info))))

(defmethod slack-search-create-message ((_room slack-file-search-result) payload)
  (slack-file-create payload))

(defun slack-create-search-result (plist team type)
  (let* ((result (cl-case type
                   ('message (apply #'make-instance 'slack-search-result
                                    (slack-collect-slots 'slack-search-result
                                                         plist)))
                   ('file (apply #'make-instance 'slack-file-search-result
                                 (slack-collect-slots 'slack-file-search-result
                                                      plist)))))
         (result-messages (cl-loop
                           for message in (plist-get plist :messages)
                           collect (slack-search-create-message result message))))
    (slack-room-set-messages result result-messages)
    (with-slots (search-results) team
      (setq search-results
            (cl-remove-if #'(lambda (other)
                              (slack-room-equal-p result other))
                          search-results))
      (push result search-results))
    result))

(defun slack-search-create-result-params (data team type sort sort-dir)
  (let* ((messages (cl-case type
                     ('message (plist-get data :messages))
                     ('file (plist-get data :files))))
         (paging (plist-get messages :paging))
         (query (plist-get data :query))
         (plist (list :type type
                      :team-id (oref team id)
                      :id (slack-search-result-id
                           type query sort sort-dir)
                      :sort sort
                      :sort-dir sort-dir
                      :query query
                      :per-page (plist-get paging :count)
                      :total-page (plist-get paging :pages)
                      :current-page (plist-get paging :page)
                      :total-messages (plist-get paging :total)
                      :messages
                      (append (plist-get messages :matches)
                              nil))))

    plist))

(defun slack-search-query-params ()
  (let ((team (slack-team-select))
        (query (read-from-minibuffer "Query: "))
        (sort (completing-read "Sort: " `("score" "timestamp")
                               nil t))
        (sort-dir (completing-read "Direction: " `("desc" "asc")
                                   nil t)))
    (list team query sort sort-dir)))

(defun slack-search-pushnew (search-result team)
  (cl-pushnew search-result (oref team search-results)
              :test #'slack-room-equal-p))

(defun slack-search-from-messages ()
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((type 'message))
      (cl-labels
          ((on-search
            (&key data &allow-other-keys)
            (slack-request-handle-error
             (data "slack-search-from-messages")
             (let* ((params (slack-search-create-result-params
                             data team type sort sort-dir))
                    (search-result (slack-create-search-result params team 'message)))
               (slack-search-pushnew search-result team)
               (funcall slack-buffer-function
                        (slack-buffer-create search-result
                                             team :type 'info))))))
        (let ((same-search (slack-room-find (slack-search-result-id
                                             type query sort sort-dir)
                                            team)))
          (if same-search
              (progn
                (message "Same Query Already Exist")
                (funcall slack-buffer-function
                         (slack-buffer-create same-search
                                              team
                                              :type 'info)))
            (slack-search-request-message team
                                          query
                                          sort
                                          sort-dir
                                          #'on-search)))))
    ))

(defun slack-search-from-files ()
  (interactive)
  (cl-destructuring-bind (team query sort sort-dir) (slack-search-query-params)
    (let ((type 'file))
      (cl-labels
          ((on-search
            (&key data &allow-other-keys)
            (slack-request-handle-error
             (data "slack-search-from-files")
             (let* ((params (slack-search-create-result-params
                             data team type sort sort-dir))
                    (search-result (slack-create-search-result params team 'file)))
               (slack-search-pushnew search-result team)
               (funcall slack-buffer-function
                        (slack-buffer-create search-result
                                             team :type 'info))))))
        (let ((same-search (slack-room-find (slack-search-result-id type query
                                                                    sort sort-dir)
                                            team)))
          (if same-search
              (progn
                (message "Same Query Already Exist")
                (funcall slack-buffer-function
                         (slack-buffer-create same-search
                                              team
                                              :type 'info)))
            (slack-search-request-file team
                                       query
                                       sort
                                       sort-dir
                                       #'on-search)))))))

(cl-defun slack-search-request-message (team query sort sort-dir success
                                             &optional
                                             (page 1)
                                             (async t))
  (slack-search-request team query sort sort-dir success page async
                        "https://slack.com/api/search.messages"))

(cl-defun slack-search-request-file (team query sort sort-dir success
                                          &optional
                                          (page 1)
                                          (async t))
  (slack-search-request team query sort sort-dir success page async
                        "https://slack.com/api/search.files"))

(defun slack-search-request (team query sort sort-dir success page async url)
  (if (< 0 (length query))
      (slack-request
       url
       team
       :type "POST"
       :params (list (cons "query" query)
                     (cons "sort" sort)
                     (cons "sort_dir" sort-dir)
                     (cons "page" (number-to-string page)))
       :success success
       :sync (not async))))

(defun slack-search-alist (team)
  (with-slots (search-results) team
    (cl-loop for s in search-results
             collect (cons (slack-room-buffer-name s) s))))

(defun slack-search-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-search-alist team)))
    (slack-select-from-list
     (alist "Select Search: ")
     (funcall slack-buffer-function
              (slack-buffer-create selected
                                   team
                                   :type 'info)))))

;; protocols
(defmethod slack-room-update-mark ((_room slack-search-result) _team _msg))
(defmethod slack-room-sorted-messages ((room slack-search-result))
  (copy-sequence (oref room messages)))

(defmethod slack-room-update-last-read ((room slack-search-result) msg)
  (if (not (slot-exists-p msg 'info))
      (progn
        (oset room last-read (oref msg ts))
        (oset room last-channel-id ""))
    (with-slots (ts info) msg
      (with-slots (channel-id) info
        (oset room last-read ts)
        (oset room last-channel-id channel-id)))))

(defmethod slack-search-get-index ((_search-result slack-file-search-result)
                                   messages last-read &optional _last-chanel-id)
  (cl-loop for i from 0 upto (1- (length messages))
           for m = (nth i messages)
           if (string= (oref m ts) last-read)
           return i))

(defmethod slack-search-get-index ((_search-result slack-search-result)
                                   messages last-read &optional last-channel-id)
  (cl-loop for i from 0 upto (1- (length messages))
           for m = (nth i messages)
           if (and (string= (oref m ts) last-read)
                   (string= (oref (oref m info) channel-id)
                            last-channel-id))
           return i))

(defmethod slack-room-latest-messages ((room slack-search-result) messages)
  (with-slots (type last-read last-channel-id) room
    (let* ((r-messages (reverse messages))
           (nth (slack-search-get-index room r-messages
                                        last-read last-channel-id)))
      (if nth
          (nreverse
           (nthcdr (1+ nth) r-messages))
        (copy-sequence messages)))))

(defmethod slack-room-prev-messages ((room slack-file-search-result) oldest)
  (let* ((messages (reverse (oref room messages)))
         (nth (slack-search-get-index room messages oldest)))
    (if nth
        (nreverse (nthcdr (1+ nth) messages)))))

(defmethod slack-room-prev-messages ((room slack-search-result) param)
  (let* ((oldest (car param))
         (channel-id (cdr param))
         (messages (reverse (oref room messages)))
         (nth (slack-search-get-index room messages
                                      oldest channel-id)))
    (if nth
        (nreverse (nthcdr (1+ nth) messages)))))

(defmethod slack-room-render-prev-messages ((room slack-search-result)
                                            team oldest ts)
  (slack-buffer-create
   room team
   :insert-func
   #'(lambda (room team)
       (slack-buffer-widen
        (let* ((inhibit-read-only t)
               (oldest-ts (if (listp oldest) (car oldest) oldest))
               (loading-message-end (slack-buffer-ts-eq (point-min)
                                                        (point-max)
                                                        oldest-ts)))
          (delete-region (point-min) loading-message-end)
          (slack-buffer-insert-prev-messages room team oldest)))
       (slack-buffer-goto ts))
   :type 'info))

(defmethod slack-buffer-insert-prev-messages ((room slack-search-result) team oldest)
  (slack-buffer-widen
   (let ((messages (slack-room-prev-messages room oldest)))
     (if messages
         (progn
           (slack-buffer-insert-previous-link room)
           (cl-loop for m in messages
                    do (slack-buffer-insert m team t)))
       (set-marker lui-output-marker (point-min))
       (lui-insert "(no more messages)\n"))
     (slack-buffer-recover-lui-output-marker))))

(defmethod slack-room-prev-link-info ((room slack-file-search-result))
  (with-slots (oldest) room
    (oref oldest ts)))

(defmethod slack-room-prev-link-info ((room slack-search-result))
  (with-slots (oldest) room
    (with-slots (info ts) oldest
      (cons ts (oref info channel-id)))))

(defmethod slack-message-equal ((m slack-search-message) n)
  (with-slots ((m-info info) (m-ts ts)) m
    (with-slots ((m-channel-id channel-id)) m-info
      (with-slots ((n-info info) (n-ts ts)) n
        (with-slots ((n-channel-id channel-id)) n-info
          (and (string= m-channel-id n-channel-id)
               (string= m-ts n-ts)))))))

(defmethod slack-room-buffer-name ((room slack-search-result))
  (with-slots (query sort sort-dir team-id type) room
    (let ((team (slack-team-find team-id)))
      (format "%s - %s Query: %s Sort: %s Order: %s"
              (oref team name)
              (eieio-object-class room)
              query sort sort-dir))))

(defmethod slack-message-to-string ((message slack-search-message) team)
  (with-slots (info text username) message
    (with-slots (channel-id permalink) info
      (let* ((header (format "%s" username))
             (channel (slack-room-find channel-id team))
             (body (slack-message-unescape-string
                    (format "%s\n\n------------\nChanel: %s\nPermalink: %s"
                            text
                            (slack-room-name channel)
                            permalink)
                    team)))
        (slack-message-put-header-property header)
        (slack-message-put-text-property body)
        (format "%s\n%s\n" header body)))))

(defmethod slack-room-set-prev-messages ((room slack-search-result) prev)
  (slack-room-set-messages room (nreverse
                                 (nconc (nreverse prev) (oref room messages)))))

(defmethod slack-room-set-messages ((room slack-search-result) messages)
  (let ((msgs (nreverse messages)))
    (oset room messages msgs)
    (oset room latest (car (last msgs)))
    (oset room oldest (car msgs))))

(defmethod slack-room-history ((room slack-search-result) team
                               &optional
                               oldest after-success async)
  (cl-labels
      ((on-history
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-room-history")
         (let* ((matches (cl-case (eieio-object-class room)
                           ('slack-search-result (plist-get data :messages))
                           ('slack-file-search-result (plist-get data :files))))
                (messages (cl-loop
                           for match across (plist-get matches :matches)
                           collect (slack-search-create-message room match))))
           (oset room current-page
                 (plist-get (plist-get matches :paging) :page))
           (if oldest
               (slack-room-set-prev-messages room messages)
             (let ((init-msg (make-instance 'slack-search-message
                                            :ts "0" :info
                                            (make-instance 'slack-search-message-info
                                                           :channel-id ""))))
               (slack-room-update-last-read room init-msg))
             (slack-room-set-messages room messages))
           (if after-success
               (funcall after-success))))))
    (let* ((current-page (oref room current-page))
           (total-page (oref room total-page))
           (next-page (if oldest
                          (1+ current-page)
                        1)))
      (with-slots (query sort sort-dir) room
        (cl-case (eieio-object-class room)
          ('slack-search-result
           (slack-search-request-message team
                                         query
                                         sort
                                         sort-dir
                                         #'on-history
                                         next-page
                                         async))
          ('slack-file-search-result
           (slack-search-request-file team
                                      query
                                      sort
                                      sort-dir
                                      #'on-history
                                      next-page
                                      async)))))))

(provide 'slack-search)
;;; slack-search.el ends here
