;;; slack-file.el ---  handle files                  -*- lexical-binding: t; -*-

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
(require 'slack-util)
(require 'slack-room)
(require 'slack-request)

(defconst slack-file-history-url "https://slack.com/api/files.list")
(defconst slack-file-list-url "https://slack.com/api/files.list")
(defconst slack-file-upload-url "https://slack.com/api/files.upload")
(defconst slack-file-delete-url "https://slack.com/api/files.delete")
(defconst slack-file-comment-delete-url "https://slack.com/api/files.comments.delete")

(defclass slack-file (slack-message)
  ((id :initarg :id)
   (created :initarg :created)
   (name :initarg :name)
   (size :initarg :size)
   (public :initarg :public)
   (filetype :initarg :filetype)
   (user :initarg :user)
   (preview :initarg :preview)
   (initial-comment :initarg :initial_comment :initform nil)
   (permalink :initarg :permalink)
   (channels :initarg :channels :type list)
   (groups :initarg :groups :type list)
   (ims :initarg :ims :type list)
   (username :initarg :username)
   (comments-count :initarg :comments_count)
   (comments :initarg :comments :initform nil)
   (page :initarg :page :initform 1)
   (pages :initarg :pages :initform nil)
   (thumb-64 :initarg :thumb_64 :initform nil)
   (thumb-80 :initarg :thumb_80 :initform nil)
   (thumb-360 :initarg :thumb_360 :initform nil)
   (thumb-360-w :initarg :thumb_360_w :initform nil)
   (thumb-360-h :initarg :thumb_360_h :initform nil)
   (thumb-160 :initarg :thumb_160 :initform nil)
   (thumb-pdf :initarg :thumb_pdf :initform nil)
   (thumb-pdf-w :initarg :thumb_pdf_w :initform nil)
   (thumb-pdf-h :initarg :thumb_pdf_h :initform nil)
   (original-w :initarg :original_w :initform nil)
   (original-h :initarg :original_h :initform nil)
   (is-starred :initarg :is_starred :initform nil)
   (mimetype :initarg :mimetype :type string :initform "")
   (title :initarg :title :type (or null string) :initform nil)
   (pretty-type :initarg :pretty_type :type (or null string) :initform nil)
   (is-public :initarg :is_public :initform nil)
   (url :initarg :url :initform "" :type string)
   (url-download :initarg :url_download :initform "" :type string)
   (url-private :initarg :url_private :initform "" :type string)
   (url-private-download :initarg :url_private_download :initform "" :type string)
   ))

(defclass slack-file-email (slack-file)
  ((from :initarg :from :type (or null list) :initform nil)
   (to :initarg :to :type (or null list) :initform nil)
   ;; TODO verify type
   (cc :initarg :cc :type (or null list) :initform nil)
   (subject :initarg :subject :type string)
   (plain-text :initarg :plain_text :type string)
   (preview-plain-text :initarg :preview_plain_text :type string)
   (is-expanded :initform nil :type boolean)))

(defclass slack-file-email-from ()
  ((address :initarg :address :type string)
   (name :initarg :name :type string)
   (original :initarg :original :type string)))

(defclass slack-file-email-to (slack-file-email-from) ())
(defclass slack-file-email-cc (slack-file-email-from) ())

(defmethod slack-merge ((old slack-reaction) new)
  (with-slots (count users) old
    (setq count (oref new count))
    (setq users (cl-remove-duplicates (append users (oref new users))
                                      :test #'string=))))

(defmethod slack-merge ((old string) _new) old)
(defmethod slack-equalp ((old string) new) (string= old new))

(defmethod slack-merge ((old slack-file) new)
  (cl-labels
      ((slack-merge-string-list
        (new old)
        (cl-remove-duplicates (append new old) :test #'string=)))

    (slack-merge-list (oref old reactions) (oref new reactions))
    (slack-merge-list (oref old comments) (oref new comments))
    (slack-merge-list (oref old channels) (oref new channels))
    (slack-merge-list (oref old groups) (oref new groups))
    (slack-merge-list (oref old ims) (oref new ims))
    (with-slots (comments-count initial-comment) old
      (setq comments-count (oref new comments-count))
      (setq initial-comment (oref new initial-comment)))))

(defmethod slack-file-set-channel ((f slack-file) id)
  (cond
   ((string-prefix-p "G" id)
    (oset f groups (list id)))
   ((string-prefix-p "C" id)
    (oset f channels (list id)))
   ((string-prefix-p "D" id)
    (oset f ims (list id)))))

(defclass slack-file-room (slack-room) ())

(defun slack-file-find (id team)
  (let ((files (oref (slack-file-room-obj team) messages)))
    (cl-find-if #'(lambda (file) (string= (oref file id) id))
                files)))

(defun slack-file-room-obj (team)
  (with-slots (file-room) team
    (if file-room
        file-room
      (setq file-room (slack-file-room "file-room"
                                       :name "Files"
                                       :id "F"
                                       :team-id (oref team id)
                                       :created (format-time-string "%s")
                                       :latest nil
                                       :unread_count 0
                                       :unread_count_display 0
                                       :messages '())))))

(defun slack-file-comment-create (payload file-id)
  (let* ((reactions (mapcar #'slack-reaction-create
                            (append (plist-get payload :reactions) nil)))
         (comment (apply #'make-instance
                         'slack-file-comment
                         (slack-collect-slots 'slack-file-comment
                                              (plist-put payload
                                                         :reactions
                                                         reactions)))))
    (oset comment file-id file-id)
    (oset comment reactions reactions)
    comment))

(defun slack-file-create-email-from (payload &optional type)
  (and payload
       (make-instance (or (and (eq type 'to) 'slack-file-email-to)
                          (and (eq type 'cc) 'slack-file-email-cc)
                          'slack-file-email-from)
                      :original (plist-get payload :original)
                      :name (plist-get payload :name)
                      :address (plist-get payload :address))))

(defun slack-file-create (payload)
  (setq payload (append payload nil))
  (plist-put payload :channels (append (plist-get payload :channels) nil))
  (plist-put payload :groups (append (plist-get payload :groups) nil))
  (plist-put payload :ims (append (plist-get payload :ims) nil))
  (plist-put payload :reactions (append (plist-get payload :reactions) nil))
  (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
  (plist-put payload :ts (number-to-string (plist-get payload :timestamp)))
  (plist-put payload :channel "F")
  (let* ((file (if (string= "email" (plist-get payload :filetype))
                   (progn
                     (plist-put payload :from
                                (mapcar #'slack-file-create-email-from
                                        (plist-get payload :from)))
                     (plist-put payload :to
                                (mapcar #'(lambda (e)
                                            (slack-file-create-email-from e 'to))
                                        (plist-get payload :to)))
                     (plist-put payload :cc
                                (mapcar #'(lambda (e)
                                            (slack-file-create-email-from e 'cc))
                                        (plist-get payload :cc)))
                     (apply #'slack-file-email "file-email"
                            (slack-collect-slots 'slack-file-email
                                                 payload)))

                 (apply #'slack-file "file"
                        (slack-collect-slots 'slack-file payload))))
         (initial-comment (if (plist-get payload :initial_comment)
                              (slack-file-comment-create (plist-get payload :initial_comment)
                                                         (oref file id))
                            nil))
         (comments (mapcar #'(lambda (e) (slack-file-comment-create e (oref file id)))
                           (plist-get payload :comments))))
    (oset file reactions
          (mapcar #'slack-reaction-create (plist-get payload :reactions)))
    (oset file comments comments)
    (oset file initial-comment initial-comment)
    file))

(defmethod slack-message-equal ((f slack-file) other)
  (string= (oref f id) (oref other id)))

(defmethod slack-equalp ((old slack-file) new)
  (string= (oref old id) (oref new id)))

(defmethod slack-file-pushnew ((f slack-file) team)
  (let ((room (slack-file-room-obj team)))
    (slack-merge-list (oref room messages) (list f))
    (oset room messages (slack-room-sort-messages (oref room messages)))
    (slack-room-update-latest room (car (last (oref room messages))))))

(defmethod slack-message-body ((file slack-file) team)
  (with-slots (initial-comment) file
    (let ((body (plist-get initial-comment :comment)))
      (slack-message-unescape-string body team))))

(defun slack-file-more-comments-string (file)
  (let ((count (oref file comments-count))
        (page (oref file page))
        (comments (oref file comments)))
    (propertize (format "%s more comments" (- count 1))
                'face '(:underline t)
                'page page
                'file (oref file id)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET")
                            #'slack-file-update)
                          map))))

(defconst slack-file-info-url "https://slack.com/api/files.info")

(defun slack-file-update ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (with-slots (file team) buf
        (slack-if-let* ((page (oref file page)))
            (slack-file-request-info
             file page team
             #'(lambda (file team)
                 (slack-redisplay file team)))))))

(defun slack-file-request-info (file-id page team &optional after-success)
  (cl-labels
      ((on-file-info (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-file-info"))
                     (let* ((paging (plist-get data :paging))
                            (comments (plist-get data :comments))
                            (file (slack-file-create (plist-put (plist-get data :file)
                                                                :comments comments))))
                       (slack-file-pushnew file team)
                       (if after-success
                           (funcall after-success file team)))))
    (slack-request
     (slack-request-create
      slack-file-info-url
      team
      :params (list (cons "file" file-id)
                    (cons "page" (number-to-string page)))
      :success #'on-file-info))))

(defmethod slack-file-comments-count-to-string ((this slack-file))
  (format "%s comment%s"
          (oref this comments-count)
          (if (< 1 (oref this comments-count))
              "s" "")))

(defmethod slack-file-gdoc-p ((this slack-file))
  (string= (oref this filetype) "gdoc"))

(defmethod slack-file-gdoc-to-string ((this slack-file))
  (with-slots (pretty-type name title url-private permalink) this
    (let ((title (propertize (format "<%s|%s>" permalink (or title name))
                             'face '(:weight bold)))
          (description (format "<%s|%s>" url-private pretty-type)))
      (slack-format-message title description))))

(defmethod slack-message-body-to-string ((file slack-file) team)
  (cond
   ((slack-file-gdoc-p file) (slack-file-gdoc-to-string file))
   (t (with-slots (name title size filetype permalink) file
        (slack-message-put-text-property
         (format "name: %s\nsize: %s\ntype: %s\n%s\n"
                 (or title name) size filetype permalink))))))

(defmethod slack-file-comments-to-string ((file slack-file) team)
  (with-slots (comments) file
    (and (< 0 (length comments))
         (format "\n%s"
                 (mapconcat #'(lambda (e)
                                (slack-message-to-string e team))
                            comments "\n")))))

(defmethod slack-file-initial-comment-to-string ((file slack-file) team)
  (with-slots (initial-comment) file
    (and initial-comment
         (format "comment:\n%s"
                 (slack-message-to-string initial-comment team )))))

;; (defmethod slack-file-comments-count-to-string ((file slack-file))
;;   (with-slots (comments-count comments) file
;;     (if (> comments-count (length comments))
;;         (slack-file-more-comments-string file)
;;       "")))

(defmethod slack-team-display-image-inlinep ((_file slack-file) team)
  (slack-team-display-file-image-inlinep team))

(defmethod slack-message-image-to-string ((file slack-file))
  (slack-image-string (slack-file-thumb-image-spec file)))

(defmethod slack-file-image-p ((this slack-file))
  (string= (car (split-string (oref this mimetype) "/"))
           "image"))

(defmethod slack-message-large-image-to-string ((file slack-file))
  (slack-image-string (slack-file-image-spec file)))

(defmethod slack-message-to-string ((this slack-file-email) team)
  (with-slots (preview-plain-text from subject) this
    (slack-format-message (slack-message-header-to-string this team)
                          (format "Subject: %s" subject)
                          (format "From: %s" (mapconcat #'identity
                                                        (mapcar #'(lambda (e) (oref e original))
                                                                from)
                                                        ", "))
                          (propertize preview-plain-text
                                      'slack-defer-face #'slack-put-preview-overlay)
                          (slack-file-comments-count-to-string this)
                          (slack-message-reaction-to-string this team)
                          (slack-file-link-info (oref this id) "\n(more info)"))))

(defmethod slack-message-to-string ((file slack-file) team)
  (with-slots (title name pretty-type mimetype) file
    (slack-format-message (slack-message-header-to-string file team)
                          (format "%s%s"
                                  (or title name)
                                  (or (and pretty-type
                                           (format ": %s" pretty-type))
                                      (format ": %s" mimetype)))
                          (slack-file-comments-count-to-string file)
                          (slack-message-image-to-string file)
                          (slack-message-reaction-to-string file team)
                          (slack-file-link-info (oref file id) "\n(more info)"))))

(defun slack-file-list ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-file-room-obj team)))
    (slack-room-display room team)))

(cl-defmethod slack-room-history-request ((room slack-file-room) team &key oldest after-success async)
  (cl-labels
      ((on-file-list
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-file-list")
         (let ((files (cl-loop for e in (plist-get data :files)
                               collect (slack-file-create e))))
           (if oldest
               (slack-room-set-prev-messages room files)
             (slack-room-set-messages room files)))
         (if after-success
             (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-file-list-url
      team
      :params (list (if oldest
                        (cons "ts_to" oldest)))
      :success #'on-file-list))))

(defun slack-file-upload ()
  (interactive)
  (cl-labels
      ((on-file-upload (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-file-upload")))
       (select-channels (channels acc)
                        (let ((selected (apply slack-completing-read-function
                                               (if acc
                                                   (list "Select another channel (or leave empty): "
                                                         (cons "" channels) nil t)
                                                 (list "Select channel: " channels nil t)))))
                          (if (< 0 (length selected))
                              (select-channels (cl-remove-if (lambda (x) (equal selected (car-safe x))) channels)
                                               (cons selected acc))
                            acc)))
       (channel-id (selected channels)
                   (oref (cdr (cl-assoc selected channels :test #'string=))
                         id)))
    (let* ((team (slack-team-select))
           (channels (slack-room-names
                      (append (oref team ims)
                              (oref team channels)
                              (oref team groups))))
           (target-channels (select-channels channels '()))
           (channel-ids (mapconcat #'(lambda (selected)
                                       (channel-id selected channels))
                                   (cl-delete-if #'null target-channels)
                                   ","))
           (buf (find-file-noselect
                 (car (find-file-read-args
                       "Select File: "
                       (confirm-nonexistent-file-or-buffer)))
                 t t))
           (filename (read-from-minibuffer "Filename: "
                                           (file-name-nondirectory
                                            (buffer-file-name buf))))
           (filetype (read-from-minibuffer "Filetype: "
                                           (file-name-extension
                                            (buffer-file-name buf))))
           (initial-comment (read-from-minibuffer "Message: ")))
      (slack-request
       (slack-request-create
        slack-file-upload-url
        team
        :type "POST"
        :params (list (cons "filename" filename)
                      (cons "channels" channel-ids)
                      (cons "filetype" filetype)
                      (if initial-comment
                          (cons "initial_comment" initial-comment)))
        :files (list (cons "file" buf))
        :headers (list (cons "Content-Type" "multipart/form-data"))
        :success #'on-file-upload)))))

(defun slack-file-delete ()
  (interactive)
  (cl-labels
      ((on-file-delete (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-file-delete"))))
    (let* ((team (slack-team-select))
           (files (oref (slack-file-room-obj team) messages))
           (your-files (cl-remove-if #'(lambda (f)
                                         (not (string= (oref f user)
                                                       (oref team self-id))))
                                     files))
           (candidates (mapcar #'(lambda (f)
                                   (cons (concat
                                          (slack-message-time-to-string (oref f ts))
                                          " "
                                          (oref f (or title name)))
                                         f))
                               your-files))
           (selected (funcall slack-completing-read-function "Select File: " candidates))
           (deleting-file (cdr (cl-assoc selected candidates :test #'string=))))
      (slack-request
       (slack-request-create
        slack-file-delete-url
        team
        :params (list (cons "file" (oref deleting-file id)))
        :success #'on-file-delete)))))

(defconst slack-file-comment-add-url "https://slack.com/api/files.comments.add")

(defmethod slack-file-id ((m slack-file-message))
  (slack-file-id (oref m file)))

(defmethod slack-file-id ((file slack-file))
  (oref file id))

(defmethod slack-file-channel ((m slack-file-message))
  (oref m channel))

(defmethod slack-file-channel ((_file slack-file))
  nil)

(defmethod slack-file-comment-id ((m slack-file-message))
  (slack-file-comment-id (oref m comment)))

(defmethod slack-file-comment-id ((file-comment))
  (oref file-comment id))

(defun slack-file-comment-add-request (id comment team
                                          &optional channel after-success)
  (cl-labels
      ((on-file-comment-add (&key data &allow-other-keys)
                            (slack-request-handle-error
                             (data "slack-file-comment-add")
                             (when after-success
                               (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-file-comment-add-url
      team
      :params (list (cons "file" id)
                    (cons "comment" (slack-message-prepare-links
                                     (slack-escape-message comment)
                                     team))
                    (if channel
                        (cons "channel" channel)))
      :success #'on-file-comment-add))))

(defun slack-file-comment-delete-request (file-id file-comment-id team)
  (cl-labels
      ((on-file-comment-delete (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-file-comment-delete"))))
    (slack-request
     (slack-request-create
      slack-file-comment-delete-url
      team
      :params (list (cons "id" file-comment-id)
                    (cons "file" file-id))
      :success #'on-file-comment-delete))))

(defconst slack-file-comment-edit-url "https://slack.com/api/files.comments.edit")

(defun slack-file-comment--edit (room-id team-id ts comment)
  (let* ((team (slack-team-find team-id))
         (room (slack-room-find room-id team))
         (message (slack-room-find-message room ts))
         (file-id (slack-file-id message))
         (comment-id (slack-file-comment-id message)))
    (slack-file-comment-edit-request file-id
                                     comment-id
                                     comment
                                     team)))

(defun slack-file-comment-edit-request (file-id file-comment-id comment team)
  (cl-labels
      ((on-file-comment-edit (&key data &allow-other-keys)
                             (slack-request-handle-error
                              (data "slack-file-comment-edit-request"))))
    (slack-request
     (slack-request-create
      slack-file-comment-edit-url
      team
      :params (list (cons "id" file-comment-id)
                    (cons "file" file-id)
                    (cons "comment" (slack-message-prepare-links
                                     (slack-escape-message comment)
                                     team)))
      :success #'on-file-comment-edit))))


(defmethod slack-file-thumb-image-spec ((file slack-file))
  (with-slots (thumb-360 thumb-360-w thumb-360-h thumb-160 thumb-80 thumb-64 thumb-pdf thumb-pdf-w thumb-pdf-h) file
    (or (and thumb-360 (list thumb-360 thumb-360-w thumb-360-h))
        (and thumb-160 (list thumb-160 nil nil))
        (and thumb-80 (list thumb-80 nil nil))
        (and thumb-64 (list thumb-64 nil nil))
        (and thumb-pdf (list thumb-pdf thumb-pdf-w thumb-pdf-h))
        (list nil nil nil))))

(defmethod slack-file-image-spec ((this slack-file))
  (with-slots (is-public url-download url-private-download) this
    (list url-private-download
          nil
          nil
          nil
          (floor (* 0.9 (frame-pixel-width))))))

(defmethod slack-file-channel-ids ((file slack-file))
  (append (oref file channels)
          (oref file ims)
          (oref file groups)))

(defun slack-file-link-info (file-id text)
  (propertize text
              'file file-id
              'face '(:underline t :weight bold)
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET")
                          #'slack-file-display)
                        map)))

(defmethod slack-file-summary ((file slack-file) _ts team)
  (with-slots (initial-comment pretty-type mimetype permalink name title) file
    (format "uploaded%s this %s: %s <%s|open in browser>"
            (if initial-comment
                " and commented on"
              "")
            (or pretty-type mimetype)
            (slack-file-link-info (oref file id)
                                  (slack-message-unescape-string (or title name)
                                                                 team))
            permalink)))

(defmethod slack-file-summary ((this slack-file-email) ts team)
  (with-slots (preview-plain-text plain-text is-expanded) this
    (let* ((has-more (< (length preview-plain-text)
                        (length plain-text)))
           (body (slack-message-unescape-string
                  (or (and is-expanded plain-text)
                      (or (and has-more (format "%s…" preview-plain-text))
                          preview-plain-text))
                  team)))
      (format "%s\n\n%s\n\n%s"
              (call-next-method)
              (propertize body
                          'slack-defer-face #'slack-put-preview-overlay)
              (propertize (or (and is-expanded "Collapse ↑")
                              "+ Click to expand inline")
                          'face '(:underline t)
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map (kbd "RET")
                                      #'(lambda ()
                                          (interactive)
                                          (slack-buffer-toggle-email-expand
                                           slack-current-buffer
                                           ts)))
                                    map))))))

(defmethod slack-file-update-comment ((file slack-file) comment team
                                      &optional edited-at)
  (when (oref comment is-intro)
    (oset file initial-comment comment))
  (slack-merge-list (oref file comments) (list comment))
  ;; (cl-loop for channel in (slack-file-channel-ids file)
  ;;          do (let* ((room (slack-room-find channel team))
  ;;                    (message (if (oref comment is-intro)
  ;;                                 (slack-room-find-file-share-message
  ;;                                  room (oref file id))
  ;;                               (slack-room-find-file-comment-message
  ;;                                room (oref comment id)))))
  ;;               (when message
  ;;                 (oset message file file)
  ;;                 (oset message edited-at edited-at)
  ;;                 (if (oref comment is-intro)
  ;;                     (oset message text (slack-file-summary file))
  ;;                   (oset message comment comment))
  ;;                 (slack-message-update message team t))))
  )

(defmethod slack-reaction-find ((this slack-file) reaction)
  (slack-reaction--find (oref this reactions) reaction))

(defmethod slack-reaction-delete ((this slack-file) reaction)
  (oset this reactions
        (slack-reaction--delete (oref this reactions) reaction)))

(defmethod slack-reaction-push ((this slack-file) reaction)
  (push reaction (oref this reactions)))

(defmethod slack-message-reactions ((this slack-file))
  (oref this reactions))

(defmethod slack-reaction-find ((this slack-file) reaction)
  (slack-reaction--find (oref this reactions) reaction))

(defun slack-reorder-comments (comments)
  (cl-remove-duplicates (cl-sort comments
                                 #'<
                                 :key #'(lambda (e) (oref e timestamp)))
                        :test #'string= :key #'(lambda (e) (oref e id))))

(defmacro slack-with-file (id team &rest body)
  (declare (indent 2) (debug t))
  `(cl-loop for file in (oref (slack-file-room-obj ,team) messages)
            do (when (string= (oref file id) ,id)
                 ,@body)))

(defmethod slack-add-comment ((this slack-file) comment)
  (with-slots (comments comments-count) this
    (setq comments (slack-reorder-comments (cons comment comments)))
    (cl-incf comments-count)))

(defmethod slack-delete-comment ((this slack-file) comment-id)
  (with-slots (comments comments-count) this
    (setq comments (slack-reorder-comments (cl-remove-if #'(lambda (e) (string= comment-id (oref e id)))
                                                         comments)))
    (cl-decf comments-count)))

(defmethod slack-room-buffer-name ((this slack-file))
  (with-slots (name) this
    (format "*Slack File - %s*" name)))

(define-derived-mode slack-file-info-mode lui-mode "Slack File Info"
  ""
  (setq-local default-directory slack-default-directory)
  (lui-set-prompt (format "Add Comment %s" lui-prompt-string))
  (setq lui-input-function 'slack-file-comment--add))

(defun slack-file-comment--add (message)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-send-message buf message)))

(defun slack-file-display ()
  (interactive)
  (let ((id (get-text-property (- (point) (line-beginning-position)) 'file (thing-at-point 'line))))
    (slack-if-let* ((buf slack-current-buffer))
        (slack-buffer-display-file buf id))))

(defmethod slack-message-star-added ((this slack-file))
  (oset this is-starred t))

(defmethod slack-message-star-removed ((this slack-file))
  (oset this is-starred nil))

(defmethod slack-message-star-api-params ((this slack-file))
  (cons "file" (oref this id)))

(defun slack-file-process-star-api (url team file-id)
  (slack-with-file file-id team
    (slack-message-star-api-request url
                                    (list (slack-message-star-api-params file))
                                    team)))

(defmethod slack-file-comments-loaded-p ((this slack-file))
  (with-slots (comments comments-count) this
    (<= comments-count (length comments))))

(defmethod slack-message-body-to-string ((this slack-file-email) _team)
  (let ((from (format "From: %s" (mapconcat #'(lambda (e) (oref e original))
                                            (oref this from)
                                            ", ")))
        (to (format "To: %s" (mapconcat #'(lambda (e) (oref e original))
                                        (oref this to)
                                        ", ")))
        (cc (format "CC: %s" (mapconcat #'(lambda (e) (oref e original))
                                        (oref this cc)
                                        ", ")))
        (subject (format "Subject: %s" (oref this subject)))
        (body (propertize (format "\n%s" (oref this plain-text))
                          'slack-defer-face #'slack-put-email-body-overlay))
        (date (format "Date: %s" (slack-message-time-to-string (oref this created)))))
    (mapconcat #'identity
               (list from to cc subject date body)
               "\n")))

(defun slack-redisplay (file team)
  ;; (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer file team)))
  ;;     (slack-buffer--replace buffer nil))
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer
                                             (slack-file-room-obj team)
                                             team)))
      (slack-buffer-replace buffer file)))

(defmethod slack-find-file-comment ((this slack-file) file-comment-id)
  (cl-find-if #'(lambda (e) (string= (oref e id) file-comment-id))
              (cl-remove-if #'null
                            (append (oref this comments)
                                    (list (oref this initial-comment))))))


(defmethod slack-message-update ((this slack-file) team &rest _args)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             this
                                             team)))
      (progn
        (oset buffer file this)
        (slack-buffer-update buffer))))

(defmethod slack-file-insert-comment ((this slack-file) file-comment-id team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             this
                                             team)))
      (progn
        (oset buffer file this)
        (slack-buffer-insert-file-comment buffer file-comment-id))))

(defmethod slack-file-delete-comment ((this slack-file) file-comment-id team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             this
                                             team)))
      (progn
        (oset buffer file this)
        (slack-buffer-update buffer file-comment-id))))

(provide 'slack-file)
;;; slack-file.el ends here

;; [2017-11-11 20:58:38] (:type file_comment_added :comment (:id Fc7ZJJ8N86 :created 1510401518 :timestamp 1510401518 :user U1013370U :is_intro :json-false :comment jaja) :file_id F7YJYDFV1 :user_id U1013370U :file (:id F7YJYDFV1) :event_ts 1510401518.000012 :ts 1510401518.000012)
