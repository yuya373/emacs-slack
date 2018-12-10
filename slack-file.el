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

(require 'slack-util)
(require 'slack-room)
(require 'slack-request)
(require 'slack-buffer)
(require 'slack-image)

(defvar slack-file-link-keymap)
(defvar slack-default-directory)
(defvar slack-completing-read-function)
(defvar slack-current-buffer)
(defconst slack-file-history-url "https://slack.com/api/files.list")
(defconst slack-file-list-url "https://slack.com/api/files.list")
(defconst slack-file-upload-url "https://slack.com/api/files.upload")
(defconst slack-file-delete-url "https://slack.com/api/files.delete")
;; [file type | Slack](https://api.slack.com/types/file#file_types)
;; let s = "";

;; Array.from(document.querySelectorAll("table#file_types tr")).forEach((e) => {
;;   const tds = Array.from(e.querySelectorAll("td"));
;;   if (tds.length === 2) {
;;     const name = tds[0].textContent;
;;     const desc = tds[1].textContent;
;;     s += `\n ("${name}" . "${desc}")`;
;;   }
;; })
(defconst slack-file-types '(("auto" . "Auto Detect Type")
                             ("text" . "Plain Text")
                             ("ai" . "Illustrator File")
                             ("apk" . "APK")
                             ("applescript" . "AppleScript")
                             ("binary" . "Binary")
                             ("bmp" . "Bitmap")
                             ("boxnote" . "BoxNote")
                             ("c" . "C")
                             ("csharp" . "C#")
                             ("cpp" . "C++")
                             ("css" . "CSS")
                             ("csv" . "CSV")
                             ("clojure" . "Clojure")
                             ("coffeescript" . "CoffeeScript")
                             ("cfm" . "ColdFusion")
                             ("d" . "D")
                             ("dart" . "Dart")
                             ("diff" . "Diff")
                             ("doc" . "Word Document")
                             ("docx" . "Word document")
                             ("dockerfile" . "Docker")
                             ("dotx" . "Word template")
                             ("email" . "Email")
                             ("eps" . "EPS")
                             ("epub" . "EPUB")
                             ("erlang" . "Erlang")
                             ("fla" . "Flash FLA")
                             ("flv" . "Flash video")
                             ("fsharp" . "F#")
                             ("fortran" . "Fortran")
                             ("gdoc" . "GDocs Document")
                             ("gdraw" . "GDocs Drawing")
                             ("gif" . "GIF")
                             ("go" . "Go")
                             ("gpres" . "GDocs Presentation")
                             ("groovy" . "Groovy")
                             ("gsheet" . "GDocs Spreadsheet")
                             ("gzip" . "GZip")
                             ("html" . "HTML")
                             ("handlebars" . "Handlebars")
                             ("haskell" . "Haskell")
                             ("haxe" . "Haxe")
                             ("indd" . "InDesign Document")
                             ("java" . "Java")
                             ("javascript" . "JavaScript/JSON")
                             ("jpg" . "JPEG")
                             ("keynote" . "Keynote Document")
                             ("kotlin" . "Kotlin")
                             ("latex" . "LaTeX/sTeX")
                             ("lisp" . "Lisp")
                             ("lua" . "Lua")
                             ("m4a" . "MPEG 4 audio")
                             ("markdown" . "Markdown (raw)")
                             ("matlab" . "MATLAB")
                             ("mhtml" . "MHTML")
                             ("mkv" . "Matroska video")
                             ("mov" . "QuickTime video")
                             ("mp3" . "mp4")
                             ("mp4" . "MPEG 4 video")
                             ("mpg" . "MPEG video")
                             ("mumps" . "MUMPS")
                             ("numbers" . "Numbers Document")
                             ("nzb" . "NZB")
                             ("objc" . "Objective-C")
                             ("ocaml" . "OCaml")
                             ("odg" . "OpenDocument Drawing")
                             ("odi" . "OpenDocument Image")
                             ("odp" . "OpenDocument Presentation")
                             ("odd" . "OpenDocument Spreadsheet")
                             ("odt" . "OpenDocument Text")
                             ("ogg" . "Ogg Vorbis")
                             ("ogv" . "Ogg video")
                             ("pages" . "Pages Document")
                             ("pascal" . "Pascal")
                             ("pdf" . "PDF")
                             ("perl" . "Perl")
                             ("php" . "PHP")
                             ("pig" . "Pig")
                             ("png" . "PNG")
                             ("post" . "Slack Post")
                             ("powershell" . "PowerShell")
                             ("ppt" . "PowerPoint presentation")
                             ("pptx" . "PowerPoint presentation")
                             ("psd" . "Photoshop Document")
                             ("puppet" . "Puppet")
                             ("python" . "Python")
                             ("qtz" . "Quartz Composer Composition")
                             ("r" . "R")
                             ("rtf" . "Rich Text File")
                             ("ruby" . "Ruby")
                             ("rust" . "Rust")
                             ("sql" . "SQL")
                             ("sass" . "Sass")
                             ("scala" . "Scala")
                             ("scheme" . "Scheme")
                             ("sketch" . "Sketch File")
                             ("shell" . "Shell")
                             ("smalltalk" . "Smalltalk")
                             ("svg" . "SVG")
                             ("swf" . "Flash SWF")
                             ("swift" . "Swift")
                             ("tar" . "Tarball")
                             ("tiff" . "TIFF")
                             ("tsv" . "TSV")
                             ("vb" . "VB.NET")
                             ("vbscript" . "VBScript")
                             ("vcard" . "vCard")
                             ("velocity" . "Velocity")
                             ("verilog" . "Verilog")
                             ("wav" . "Waveform audio")
                             ("webm" . "WebM")
                             ("wmv" . "Windows Media Video")
                             ("xls" . "Excel spreadsheet")
                             ("xlsx" . "Excel spreadsheet")
                             ("xlsb" . "Excel Spreadsheet (Binary, Macro Enabled)")
                             ("xlsm" . "Excel Spreadsheet (Macro Enabled)")
                             ("xltx" . "Excel template")
                             ("xml" . "XML")
                             ("yaml" . "YAML")
                             ("zip" . "Zip")))

(defclass slack-file ()
  ((id :initarg :id)
   (created :initarg :created)
   (name :initarg :name)
   (size :initarg :size)
   (public :initarg :public)
   (filetype :initarg :filetype)
   (user :initarg :user)
   (preview :initarg :preview)
   (permalink :initarg :permalink)
   (channels :initarg :channels :type list)
   (groups :initarg :groups :type list)
   (ims :initarg :ims :type list)
   (username :initarg :username)
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
   (timestamp :initarg :timestamp :type number)
   (comments :initarg :comments :type list :initform '())
   (mode :initarg :mode :type (or null string) :initform nil)
   ))

(defclass slack-file-email (slack-file)
  ((from :initarg :from :type (or null list) :initform nil)
   (to :initarg :to :type (or null list) :initform nil)
   ;; TODO verify type
   (cc :initarg :cc :type (or null list) :initform nil)
   (subject :initarg :subject :type (or null string))
   (plain-text :initarg :plain_text :type string)
   (preview-plain-text :initarg :preview_plain_text :type string)
   (is-expanded :initform nil :type boolean)))

(defclass slack-file-email-from ()
  ((address :initarg :address :type string)
   (name :initarg :name :type string)
   (original :initarg :original :type string)))

(defclass slack-file-email-to (slack-file-email-from) ())
(defclass slack-file-email-cc (slack-file-email-from) ())

(defclass slack-file-comment ()
  ((id :initarg :id :type string)
   (created :initarg :created :type number)
   (timestamp :initarg :timestamp :type number)
   (user :initarg :user :type string)
   (is-intro :initarg :is_intro)
   (comment :initarg :comment :type string)))

(cl-defmethod slack-merge ((old string) _new) old)
(cl-defmethod slack-equalp ((old string) new) (string= old new))

(cl-defmethod slack-merge ((old slack-file) new)
  (cl-labels
      ((slack-merge-string-list
        (new old)
        (cl-remove-duplicates (append new old) :test #'string=)))

    (slack-merge-list (oref old channels) (oref new channels))
    (slack-merge-list (oref old groups) (oref new groups))
    (slack-merge-list (oref old ims) (oref new ims))
    ))

(defun slack-file-find (id team)
  (let ((files (oref team files)))
    (cl-find-if #'(lambda (file) (string= (oref file id) id))
                files)))

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
  (plist-put payload :pinned_to (append (plist-get payload :pinned_to) nil))
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
         )
    file))

(cl-defmethod slack-message-equal ((f slack-file) other)
  (string= (oref f id) (oref other id)))

(cl-defmethod slack-equalp ((old slack-file) new)
  (string= (oref old id) (oref new id)))

(cl-defmethod slack-file-pushnew ((f slack-file) team)
  (slack-merge-list (oref team files) (list f)))

(defconst slack-file-info-url "https://slack.com/api/files.info")

(defun slack-file-comment-create (payload)
  (apply 'make-instance 'slack-file-comment
         (slack-collect-slots 'slack-file-comment payload)))

(defun slack-file-request-info (file-id page team &optional after-success)
  (cl-labels
      ((on-file-info (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-file-info")
                      (let* ((file (slack-file-create (plist-get data :file)))
                             (comments (mapcar #'slack-file-comment-create
                                               (plist-get data :comments))))
                        (oset file comments comments)
                        (slack-file-pushnew file team)
                        (if after-success
                            (funcall after-success file team))))))
    (slack-request
     (slack-request-create
      slack-file-info-url
      team
      :params (list (cons "file" file-id)
                    (cons "page" (number-to-string page)))
      :success #'on-file-info))))

(cl-defmethod slack-file-gdoc-p ((this slack-file))
  (string= (oref this filetype) "gdoc"))

(cl-defmethod slack-team-display-image-inlinep ((_file slack-file) team)
  (slack-team-display-file-image-inlinep team))

(cl-defmethod slack-message-image-to-string ((file slack-file))
  (slack-image-string (slack-file-thumb-image-spec file)))

(cl-defmethod slack-file-image-p ((this slack-file))
  (string= (car (split-string (oref this mimetype) "/"))
           "image"))

(cl-defmethod slack-message-large-image-to-string ((file slack-file))
  (slack-image-string (slack-file-image-spec file)))

(defun slack-file-select-sharing-channels (current-room-name team)
  (let* ((channels (slack-room-names
                    (append (oref team ims)
                            (oref team channels)
                            (oref team groups))
                    team))
         (target-channels
          (slack-select-multiple #'(lambda (loop-count)
                                     (if (< 0 loop-count)
                                         "Select another channel (or leave empty): "
                                       "Select channel: "))
                                 channels
                                 #'(lambda (loop-count)
                                     (when (< loop-count 1)
                                       current-room-name)))))
    (mapcar #'(lambda (channel) (oref channel id))
            target-channels)))

(defun slack-file-select-upload-file-as-buffer ()
  (find-file-noselect
   (car (find-file-read-args
         "Select File: "
         (confirm-nonexistent-file-or-buffer)))
   t t))

(defun slack-file-select-filetype (&optional initial-input)
  (let* ((candidate (mapcar #'(lambda (e)
                                (cons (format "%s: %s" (car e) (cdr e))
                                      (car e)))
                            slack-file-types))
         (selected (completing-read "Select Filetype: "
                                    candidate nil t
                                    initial-input)))
    (cdr (cl-assoc selected candidate :test #'string=))))

(defun slack-file-upload-snippet (&optional beg end)
  (interactive "r")
  (slack-if-let*
      ((team (slack-team-select t))
       (channels (mapconcat #'identity
                            (slack-file-select-sharing-channels "" team)
                            ","))
       (title (read-from-minibuffer "Title: "))
       (initial-comment (read-from-minibuffer "Message: "))
       (filetype (slack-file-select-filetype))
       (content (buffer-substring-no-properties beg end)))
      (cl-labels
          ((on-success (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-file-upload-snippet"))))
        (slack-request
         (slack-request-create
          slack-file-upload-url
          team
          :type "POST"
          :headers (list (cons "Content-Type" "multipart/form-data"))

          :params (list (and filetype (cons "filetype" filetype))
                        (and initial-comment (cons "initial_comment" initial-comment))
                        (and title (cons "title" title))
                        (cons "channels" channels)
                        (cons "content" content))
          :success #'on-success)))))

(defun slack-file-upload ()
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (oref buffer team))
       (buf (slack-file-select-upload-file-as-buffer))
       (filename (read-from-minibuffer "Filename: "
                                       (file-name-nondirectory
                                        (buffer-file-name buf))))
       (filetype (slack-file-select-filetype (file-name-extension
                                              (buffer-file-name buf))))
       (initial-comment (read-from-minibuffer "Message: ")))
      (cl-labels
          ((on-file-upload (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-file-upload"))))

        (slack-request
         (slack-request-create
          slack-file-upload-url
          team
          :type "POST"
          :params (append (slack-file-upload-params buffer)
                          (list
                           (cons "filename" filename)
                           (cons "filetype" filetype)
                           (if initial-comment
                               (cons "initial_comment" initial-comment))))
          :files (list (cons "file" buf))
          :headers (list (cons "Content-Type" "multipart/form-data"))
          :success #'on-file-upload)))
    (error "Call from message buffer or thread buffer")))

;; TODO implement this
;; (defun slack-file-delete ()
;;   (interactive)
;;   (cl-labels
;;       ((on-file-delete (&key data &allow-other-keys)
;;                        (slack-request-handle-error
;;                         (data "slack-file-delete"))))
;;     (let* ((team (slack-team-select))
;;            (files (oref (slack-file-room-obj team) messages))
;;            (your-files (cl-remove-if #'(lambda (f)
;;                                          (not (string= (oref f user)
;;                                                        (oref team self-id))))
;;                                      files))
;;            (candidates (mapcar #'(lambda (f)
;;                                    (cons (concat
;;                                           (slack-message-time-to-string (slack-ts f))
;;                                           " "
;;                                           (or (oref f title)
;;                                               (oref f name)))
;;                                          f))
;;                                your-files))
;;            (selected (funcall slack-completing-read-function "Select File: " candidates))
;;            (deleting-file (cdr (cl-assoc selected candidates :test #'string=))))
;;       (slack-request
;;        (slack-request-create
;;         slack-file-delete-url
;;         team
;;         :params (list (cons "file" (oref deleting-file id)))
;;         :success #'on-file-delete)))))

(cl-defmethod slack-file-id ((file slack-file))
  (oref file id))

(cl-defmethod slack-file-channel ((_file slack-file))
  nil)

(cl-defmethod slack-file-thumb-image-spec ((file slack-file))
  (with-slots (thumb-360 thumb-360-w thumb-360-h thumb-160 thumb-80 thumb-64 thumb-pdf thumb-pdf-w thumb-pdf-h) file
    (or (and thumb-360 (list thumb-360 thumb-360-w thumb-360-h))
        (and thumb-160 (list thumb-160 nil nil))
        (and thumb-80 (list thumb-80 nil nil))
        (and thumb-64 (list thumb-64 nil nil))
        (and thumb-pdf (list thumb-pdf thumb-pdf-w thumb-pdf-h))
        (list nil nil nil))))

(cl-defmethod slack-file-image-spec ((this slack-file))
  (with-slots (is-public url-download url-private-download) this
    (list url-private-download
          nil
          nil
          nil
          (floor (* 0.9 (frame-pixel-width))))))

(cl-defmethod slack-file-channel-ids ((file slack-file))
  (append (oref file channels)
          (oref file ims)
          (oref file groups)))

(defun slack-file-link-info (file-id text)
  (propertize text
              'file file-id
              'face '(:underline t :weight bold)
              'keymap slack-file-link-keymap))

(defmacro slack-with-file (id team &rest body)
  (declare (indent 2) (debug t))
  `(cl-loop for file in (oref ,team files)
            do (when (string= (oref file id) ,id)
                 ,@body)))

(cl-defmethod slack-room-buffer-name ((this slack-file) _team)
  (with-slots (name) this
    (format "*Slack File - %s*" name)))

(define-derived-mode slack-file-info-mode lui-mode "Slack File Info"
  ""
  (setq-local default-directory slack-default-directory)
  )

(cl-defmethod slack-message-star-added ((this slack-file))
  (oset this is-starred t))

(cl-defmethod slack-message-star-removed ((this slack-file))
  (oset this is-starred nil))

(cl-defmethod slack-message-star-api-params ((this slack-file))
  (cons "file" (oref this id)))

(defun slack-file-process-star-api (url team file-id)
  (slack-with-file file-id team
    (slack-message-star-api-request url
                                    (list (slack-message-star-api-params file))
                                    team)))

(defun slack-redisplay (file team)
  ;; (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer file team)))
  ;;     (slack-buffer--replace buffer nil))
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer
                                             team)))
      (slack-buffer-replace buffer file)))

(cl-defmethod slack-message-update ((this slack-file) team &rest _args)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer
                                             this
                                             team)))
      (progn
        (oset buffer file this)
        (slack-buffer-update buffer))))

(cl-defmethod slack-ts ((this slack-file))
  (number-to-string (oref this created)))

(cl-defmethod slack-thread-message-p ((_this slack-file))
  nil)

(cl-defun slack-file-list-request (team &key
                                        (page "1")
                                        (count "100")
                                        (after-success nil))
  (cl-labels
      ((success (&key data &allow-other-keys)
                (let ((files (mapcar #'slack-file-create
                                     (plist-get data :files)))
                      (paging (plist-get data :paging)))
                  (if (string= page "1")
                      (oset team files files)
                    (oset team files (append (oref team files) files)))
                  (oset team files (cl-sort (oref team files)
                                            #'<
                                            :key #'(lambda (e) (oref e created))))
                  (when (functionp after-success)
                    (funcall after-success
                             (plist-get paging :page)
                             (plist-get paging :pages))))))
    (slack-request
     (slack-request-create
      slack-file-list-url
      team
      :params (list (cons "page" page)
                    (cons "count" count))
      :success #'success))))

(provide 'slack-file)
;;; slack-file.el ends here
