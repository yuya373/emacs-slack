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

(defun slack-search-create-message (payload search-result)
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
    (cl-case (oref search-result type)
      ('message
       (let ((info (create-info payload search-result)))
         (create-message payload info)))
      ('file
       (slack-file-create payload)))))

(defun slack-create-search-result (plist team)
  (let* ((result (apply #'make-instance 'slack-search-result
                        (slack-collect-slots 'slack-search-result
                                             plist)))
         (result-messages (cl-loop
                           for message in (plist-get plist :messages)
                           collect (slack-search-create-message message result))))
    (oset result messages result-messages)
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
                    (search-result (slack-create-search-result params team)))
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
                    (search-result (slack-create-search-result params team)))
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

(defun slack-search-get-index (search-result messages last-read
                                             &optional last-channel-id)
  (cl-case (oref search-result type)
    ('message
     (cl-loop for i from 0 upto (1- (length messages))
              for m = (nth i messages)
              if (and (string= (oref m ts) last-read)
                      (string= (oref (oref m info) channel-id)
                               last-channel-id))
              return i))
    ('file
     (cl-loop for i from 0 upto (1- (length messages))
              for m = (nth i messages)
              if (string= (oref m ts) last-read)
              return i))))

(defmethod slack-room-latest-messages ((room slack-search-result) messages)
  (with-slots (type last-read last-channel-id) room
    (let* ((r-messages (reverse messages))
           (nth (slack-search-get-index room r-messages
                                        last-read last-channel-id)))
      (if nth
          (nreverse
           (nthcdr (1+ nth) r-messages))
        (copy-sequence messages)))))

(defmethod slack-room-prev-messages ((room slack-search-result) param)
  (let* ((oldest (car param))
         (channel-id (cdr param))
         (messages (oref room messages))
         (nth (slack-search-get-index room messages
                                      oldest channel-id)))
    (if nth
        (nthcdr (1+ nth) messages))))

(defmethod slack-room-render-prev-messages ((room slack-search-result)
                                            team _cur-point oldest ts)
  (slack-buffer-create
   room team
   :insert-func
   #'(lambda (room team)
       (let* ((inhibit-read-only t)
              (loading-message-end (slack-buffer-ts-eq (point-min)
                                                       (point-max)
                                                       oldest))
              (channel-id (get-text-property (point-min) 'channel-id))
              (prev-messages
               (slack-room-prev-messages room `(,oldest . ,channel-id))))
         (delete-region (point-min) loading-message-end)
         (set-marker lui-output-marker (point-min))
         (if prev-messages
             (progn
               (cl-loop for m in prev-messages
                        do (slack-buffer-insert m team t))
               (slack-buffer-insert-previous-link (cl-first prev-messages)))))
       (slack-buffer-recover-lui-output-marker)
       (goto-char (slack-buffer-ts-eq (point-min) (point-max) ts)))
   :type 'info))

(defmethod slack-buffer-insert-previous-link ((oldest slack-search-message))
  (let ((inhibit-read-only t))
    (set-marker lui-output-marker (point-min))
    (insert (concat (propertize "(load more message)"
                                'face '(:underline t)
                                'oldest (oref oldest ts)
                                'channel-id (oref (oref oldest info)
                                                  channel-id)
                                'keymap (let ((map (make-sparse-keymap)))
                                          (define-key map (kbd "RET")
                                            #'slack-room-load-prev-messages)
                                          map))
                    "\n\n"))
    (slack-buffer-recover-lui-output-marker)))

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
      (format "%s - Search %s Query: %s Sort: %s Order: %s"
              (oref team name)
              type query sort sort-dir))))

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

(defmethod slack-room-history ((room slack-search-result) team
                               &optional
                               _oldest after-success _async)
  (let* ((next-page (1+ (oref room current-page))))
    (cl-labels ((on-history
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-room-history")
                  (let* ((messages (cl-case (oref room type)
                                     ('message
                                      (plist-get data :messages))
                                     ('file
                                      (plist-get data :files))))
                         (prev (cl-loop
                                for match across (plist-get messages :matches)
                                collect (slack-search-create-message match room))))
                    (oset room current-page next-page)
                    (oset room messages
                          (nconc (oref room messages)
                                 (nreverse prev)))
                    (if after-success
                        (funcall after-success))))))
      (if (< (oref room total-page) next-page)
          (message "Already Last Page")
        (with-slots (type query sort sort-dir) room
          (cl-case type
            ('message
             (slack-search-request-message team
                                           query
                                           sort
                                           sort-dir
                                           #'on-history
                                           next-page))
            ('file
             (slack-search-request-file team
                                        query
                                        sort
                                        sort-dir
                                        #'on-history
                                        next-page))))))))

(provide 'slack-search)
;;; slack-search.el ends here
