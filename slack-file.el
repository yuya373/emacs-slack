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
(require 'slack-room)

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
   (original-w :initarg :original_w :initform nil)
   (original-h :initarg :original_h :initform nil)
   (is-starred :initarg :is_starred :initform nil)
   (mimetype :initarg :mimetype :type string :initform "")
   (title :initarg :title :type (or null string) :initform nil)
   ))

(defun slack-merge-list (old-list new-list)
  (cl-loop for n in new-list
           do (let ((o (cl-find-if #'(lambda (e) (slack-equalp n e))
                                   old-list)))
                (if o (slack-merge o n)
                  (push n old-list)))))

(defmethod slack-merge ((old slack-reaction) new)
  (with-slots (count users) old
    (setq count (oref new count))
    (setq users (cl-remove-duplicates (append users (oref new users))
                                      :test #'string=))))

(defmethod slack-merge ((old slack-file) new)
  (cl-labels
      ((slack-merge-string-list
        (new old)
        (cl-remove-duplicates (append new old) :test #'string=)))
    (with-slots (channels groups ims reactions comments comments-count initial-comment) old
      (setq channels (slack-merge-string-list channels (oref new channels)))
      (setq groups (slack-merge-string-list groups (oref new groups)))
      (setq ims (slack-merge-string-list ims (oref new ims)))
      (slack-merge-list reactions (oref new reactions))
      (slack-merge-list comments (oref new comments))
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
                                       :last_read "0"
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

(defun slack-file-create (payload)
  (plist-put payload :channels (append (plist-get payload :channels) nil))
  (plist-put payload :groups (append (plist-get payload :groups) nil))
  (plist-put payload :ims (append (plist-get payload :ims) nil))
  (plist-put payload :reactions (append (plist-get payload :reactions) nil))
  (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
  (plist-put payload :ts (number-to-string (plist-get payload :timestamp)))
  (plist-put payload :channel "F")
  (let* ((file (apply #'slack-file "file" (slack-collect-slots 'slack-file payload)))
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

(defmethod slack-file-pushnew ((f slack-file) team)
  (let ((room (slack-file-room-obj team)))
    (with-slots (messages) room
      (let ((old (slack-file-find (oref f id) team)))
        (if old
            (slack-merge old f)
          (push f messages)
          (setq messages (slack-room-sorted-messages room))
          (oset room oldest (car messages))
          (oset room latest (car (last messages))))))))

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
  (let* ((line (thing-at-point 'line))
         (page (get-text-property 0 'page line))
         (file (get-text-property 0 'file line))
         (team (slack-team-find slack-current-team-id)))
    (if page
        (slack-file-request-info file page team #'(lambda (file team) (slack-redisplay file team))))))

(defun slack-file-request-info (file-id page team &optional after-success)
  (cl-labels
      ((on-file-info (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-file-info"))
                     (let ((paging (plist-get data :paging))
                           (comments (plist-get data :comments))
                           (file (slack-file-create (plist-get data :file))))
                       (oset file comments (mapcar
                                            #'(lambda (c) (slack-file-comment-create c (oref file id)))
                                            comments))
                       (slack-file-pushnew file team)
                       (slack-message-update file team t)
                       (if after-success
                           (funcall after-success file team)))))
    (slack-request
     (slack-request-create
      slack-file-info-url
      team
      :params (list (cons "file" file-id)
                    (cons "page" (number-to-string page)))
      :success #'on-file-info))))

(defmethod slack-message-body-to-string ((file slack-file) team)
  (with-slots (name title size filetype permalink) file
    (slack-message-put-text-property
     (format "name: %s\nsize: %s\ntype: %s\n%s\n%s\n"
             (or title name) size filetype permalink
             (format "%s comment%s"
                     (oref file comments-count)
                     (if (< 1 (oref file comments-count))
                         "s" ""))))))

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

(defmethod slack-file-comments-count-to-string ((file slack-file))
  (with-slots (comments-count comments) file
    (if (> comments-count (length comments))
        (slack-file-more-comments-string file)
      "")))

(defmethod slack-team-display-image-inlinep ((_file slack-file) team)
  (slack-team-display-file-image-inlinep team))

(defmethod slack-message-image-to-string ((file slack-file) team)
  (if (slack-team-display-image-inlinep file team)
      (slack-message-render-image file team)
    (slack-message-view-image-to-string file team)))

(defmethod slack-message-to-string ((file slack-file) team)
  (slack-format-message (slack-message-header-to-string file team)
                        (slack-message-body-to-string file team)
                        (slack-message-image-to-string file team)
                        (slack-message-reaction-to-string file)
                        (slack-file-link-info (oref file id) "\n(more info)")))

(defmethod slack-to-string ((file slack-file) team)
  (let* ((header (slack-message-header-to-string file team))
         (body (slack-message-body-to-string file team))
         (thumb (slack-message-image-to-string file team))
         (reactions (slack-message-reaction-to-string file))
         ;; (initial-comment
         ;;  (if-let* ((ic (oref file initial-comment)))
         ;;      (slack-message-to-string ic team)
         ;;    ""))
         (comments (slack-file-comments-to-string file team))
         (comments-count
          (slack-file-comments-count-to-string file)))
    (propertize
     (slack-format-message (propertize (slack-format-message header
                                                             body
                                                             thumb
                                                             reactions)
                                       'file-id (oref file id))
                           ;; initial-comment
                           comments
                           comments-count)
     'ts (oref file ts))))

(defmethod slack-room-update-mark ((_room slack-file-room) _team _msg))

(defun slack-file-create-buffer (team)
  (funcall slack-buffer-function
           (slack-room-with-buffer (slack-file-room-obj team) team
             (slack-room-insert-messages (slack-file-room-obj team) buf team))))

(defun slack-file-list ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-file-room-obj team)))
    (with-slots (buffer) room
      (if buffer
          (slack-file-create-buffer team)
        (slack-room-history-request room team
                                    :after-success
                                    #'(lambda () (slack-file-create-buffer team)))))))

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
             (slack-room-reset-last-read room)
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

(cl-defmacro slack-get-message-metadata (&body body)
  `(let* ((ts (slack-get-ts))
          (team (slack-team-find slack-current-team-id))
          (room (slack-room-find slack-current-room-id
                                 team)))
     ,@body))

(defun slack-file-comment-add ()
  (interactive)
  (slack-get-message-metadata
   (if (and ts room)
       (let ((message (slack-room-find-message room ts)))
         (if (or (object-of-class-p message 'slack-file)
                 (object-of-class-p message 'slack-file-message))
             (let ((id (slack-file-id message))
                   (channel (slack-file-channel message))
                   (comment (read-from-minibuffer "Write Comment: ")))
               (slack-file-comment-add-request id
                                               comment
                                               team
                                               channel))
           (error "Message is not a File")))
     (error "Message can't find"))))

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
                    (cons "comment" comment)
                    (if channel
                        (cons "channel" channel)))
      :success #'on-file-comment-add))))

(defun -slack-file-comment-delete ()
  (interactive)
  (slack-get-message-metadata
   (if (and ts room)
       (let ((message (slack-room-find-message room ts)))
         (if (or (object-of-class-p message 'slack-file-comment)
                 (object-of-class-p message 'slack-file-comment-message))
             (let ((id (slack-file-comment-id message))
                   (file-id (slack-file-id message)))
               (slack-file-comment-delete-request file-id id team)))))))

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
                    (cons "comment" comment))
      :success #'on-file-comment-edit))))

(defmethod slack-room-setup-buffer ((room slack-file-room) buf)
  (with-current-buffer buf
    (slack-info-mode)
    (slack-room-insert-previous-link room buf)
    (add-hook 'kill-buffer-hook 'slack-room-kill-buffer nil t)))


(defmethod slack-file-image-spec ((file slack-file))
  (with-slots (thumb-360 thumb-360-w thumb-360-h thumb-160 thumb-80 thumb-64) file
    (or (and thumb-360 (list thumb-360 thumb-360-w thumb-360-h))
        (and thumb-160 (list thumb-160 nil nil))
        (and thumb-80 (list thumb-80 nil nil))
        (and thumb-64 (list thumb-64 nil nil))
        (list nil nil nil))))

(defmethod slack-message-has-imagep ((file slack-file))
  (cl-destructuring-bind (url _width _height) (slack-file-image-spec file)
    url))

(cl-defmethod slack-image-create ((file slack-file) &key success error token)
  (cl-destructuring-bind (url width height) (slack-file-image-spec file)
    (when url
      (let ((path (slack-image-path url)))
        (when path
          (cl-labels
              ((create-image () (slack-image--create path :width width :height height))
               (on-success () (funcall success (create-image)))
               (on-error () (funcall error (create-image))))
            (if (file-exists-p path)
                (create-image)
              (progn
                (slack-url-copy-file url path
                                     :success #'on-success
                                     :error #'on-error
                                     :token token)
                nil))))))))

(defmethod slack-open-image ((file slack-file) team)
  (cl-labels
      ((render (image)
               (funcall slack-buffer-function
                        (slack-render-image image team))))
    (render (slack-image-create file
                                :success #'render
                                :error #'render
                                :token (oref team token)))))

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

(defmethod slack-file-summary ((file slack-file))
  (with-slots (initial-comment mimetype permalink name title) file
    (format "uploaded%s this %s: %s <%s|open in browser>"
            (if initial-comment
                " and commented on"
              "")
            mimetype
            (slack-file-link-info (oref file id) (or title name))
            permalink)))

(defmethod slack-file-update-comment ((file slack-file) comment team
                                      &optional edited-at)
  (when (oref comment is-intro)
    (oset file initial-comment comment))
  (slack-merge-list (oref file comments) (list comment))
  (cl-loop for channel in (slack-file-channel-ids file)
           do (let* ((room (slack-room-find channel team))
                     (message (if (oref comment is-intro)
                                  (slack-room-find-file-share-message
                                   room (oref file id))
                                (slack-room-find-file-comment-message
                                 room (oref comment id)))))
                (when message
                  (oset message file file)
                  (oset message edited-at edited-at)
                  (if (oref comment is-intro)
                      (oset message text (slack-file-summary file))
                    (oset message comment comment))
                  (slack-message-update message team t)))))

(defmethod slack-reaction-find ((this slack-file) reaction)
  (slack-reaction--find (oref this reactions) reaction))

(defmethod slack-reaction-delete ((this slack-file) reaction)
  (with-slots (reactions) this
    (setq reactions
          (slack-reaction--delete reactions reaction))))

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

(defmethod slack-buffer-set-current-room-id ((this slack-file))
  (set (make-local-variable 'slack-current-file-id) (oref this id)))

(defmethod slack-room-setup-buffer ((_this slack-file) buf)
  (with-current-buffer buf
    (slack-file-info-mode)))

(defmethod slack-room-set-buffer ((_this slack-file) _buf))

(define-derived-mode slack-file-info-mode lui-mode "Slack File Info"
  ""
  (setq-local default-directory slack-default-directory)
  (lui-set-prompt (format "Add Comment %s" lui-prompt-string))
  (setq lui-input-function 'slack-file-comment--add))

(defun slack-file-comment--add (message)
  (let ((file-id slack-current-file-id)
        (team (slack-team-find slack-current-team-id)))
    (slack-file-comment-add-request file-id
                                    message
                                    team)))

(defun slack-file-display ()
  (interactive)
  (let* ((line (thing-at-point 'line))
         (id (get-text-property (- (point) (line-beginning-position)) 'file line))
         (team (slack-team-find slack-current-team-id)))
    (cl-labels
        ((open (file team)
               (if-let* ((buf (slack-buffer-create file team)))
                   (progn
                     (slack-file--display file buf team)
                     (funcall slack-buffer-function buf)))))
      (let ((file (slack-file-find id team)))
        (if file
            (with-slots (comments comments-count) file
              (if (<= comments-count (length comments))
                  (open file team)
                (slack-file-request-info id 1 team #'open)))
          (slack-file-request-info id 1 team #'open))))))

(defun slack-file--display (file buf team)
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (delete-region (point-min) lui-output-marker))
    (lui-insert (slack-to-string file team))))

(defun slack-redisplay (file team)
  (if-let* ((buffer (get-buffer (slack-room-buffer-name file))))
      (with-current-buffer buffer
        (let ((cur-point (point))
              (max (marker-position lui-output-marker)))
          (slack-file--display file buffer team)
          (if (and (<= (point-min) cur-point)
                   (< cur-point max))
              (goto-char cur-point))))))

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

(provide 'slack-file)
;;; slack-file.el ends here
