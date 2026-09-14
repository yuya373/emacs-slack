;;; slack-block.el ---                               -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'subr-x))
(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-request)
(require 'slack-image)
(require 'slack-usergroup)
(require 'slack-mrkdwn)
(require 'slack-room)
(require 'slack-vip)

(defcustom slack-block-highlight-source nil
  "If non-nil, highlight source blocks in messages.
You need to install `language-detection' for this to work."
  :type 'boolean
  :group 'slack)

(defface slack-block-highlight-source-overlay-face
  '((((class grayscale) (background light))
     :foreground "DimGray" :weight bold)
    (((class grayscale) (background dark))
     :foreground "LightGray" :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "Firebrick")
    (((class color) (min-colors 88) (background dark))
     :foreground "chocolate1")
    (((class color) (min-colors 16) (background light))
     :foreground "red")
    (((class color) (min-colors 16) (background dark))
     :foreground "red1")
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "yellow")
    (t :weight bold))
  "If non-nil, highlight source blocks in messages.
You need to install `language-detection' for this to work.")

(defvar slack-completing-read-function)
(defvar slack-channel-button-keymap)
(defvar slack-user-mention-keymap)
(defvar slack-current-buffer)

;; Layout Blocks
;; [Reference: Message layout blocks | Slack](https://api.slack.com/reference/messaging/blocks)
(defclass slack-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (payload :initarg :payload :initform nil)))

(cl-defmethod slack-block-find-action ((_this slack-layout-block) _action-id)
  nil)

(cl-defmethod slack-block-to-string ((this slack-layout-block) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

(defun slack-create-layout-block (payload)
  (let ((type (plist-get payload :type)))
    (cond
     ((string= "header" type)
      (slack-create-layout-header-block payload))
     ((string= "section" type)
      (slack-create-section-layout-block payload))
     ((string= "divider" type)
      (slack-create-divider-layout-block payload))
     ((string= "image" type)
      (slack-create-image-layout-block payload))
     ((string= "actions" type)
      (slack-create-actions-layout-block payload))
     ((string= "context" type)
      (slack-create-context-layout-block payload))
     ((string= "rich_text" type)
      (slack-create-rich-text-block payload))
     ((string= "call" type)
      (slack-create-call-layout-block payload))
     ((string= "plan" type)
      (slack-create-plan-layout-block payload))
     ((string= "table" type)
      (slack-create-table-layout-block payload))
     ((string= "timeline" type)
      (slack-create-timeline-layout-block payload))
     ((string= "video" type)
      (slack-create-video-layout-block payload))
     ((string= "input" type)
      (slack-create-input-layout-block payload))
     (t (make-instance 'slack-layout-block
                       :type type
                       :payload payload))
     ;; ;; TODO https://api.slack.com/reference/block-kit/blocks#file
     ;; ((string= "file" type)
     ;;  (message "TODO: %S" payload)
     ;;  nil
     ;;  )
     )))

;; Rich Text Blocks
;; [Changes to message objects on the way to support WYSIWYG | Slack](https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less)
(defclass slack-layout-header-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type string)
   (text :initarg :text :type slack-text-message-composition-object)
   ))

(cl-defmethod slack-block-to-string ((this slack-layout-header-block) &optional option)
  (propertize (slack-block-to-string (oref this text)) 'face '(:weight bold :height 1.2)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-layout-header-block) &optional option)
  (format "# %s" (slack-block-to-string (oref this text))))

(defun slack-create-layout-header-block (payload)
  (make-instance 'slack-layout-header-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))))

(defclass slack-rich-text-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (elements :initarg :elements :type list) ;; list of slack-rich-text-section
   ))

(cl-defmethod slack-block-to-string ((this slack-rich-text-block) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-string element option))
             (oref this elements)
             ""))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-block) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
             (oref this elements)
             ""))

(defun slack-create-rich-text-block (payload)
  (make-instance 'slack-rich-text-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :elements (mapcar #'slack-create-rich-text-block-element
                                   (plist-get payload :elements))))

(defclass slack-call-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type string)
   (join-url :initarg :join_url :type string)))

(cl-defmethod slack-block-to-string ((this slack-call-layout-block) &optional _option)
  (concat "Join URL: " (oref this join-url)))

(defun slack-create-call-layout-block (payload)
  (make-instance
   'slack-call-layout-block
   :type (plist-get payload :type)
   ;; Possibly only works for Zoom extension. Don't know about other
   ;; "call" blocks.
   :join_url (thread-first
               payload
               (plist-get :call)
               (plist-get :v1)
               (plist-get :join_url))))

(defclass slack-plan-task-card ()
  ((task-id :initarg :task_id :type (or null string) :initform nil)
   (title :initarg :title :type (or null string) :initform nil)
   (status :initarg :status :type (or null string) :initform nil)
   (details :initarg :details :initform nil)))

(defun slack-create-plan-task-card (payload)
  (let ((details-payload (plist-get payload :details)))
    (make-instance 'slack-plan-task-card
                   :task_id (plist-get payload :task_id)
                   :title (plist-get payload :title)
                   :status (plist-get payload :status)
                   :details (when (and details-payload
                                       (string= "rich_text"
                                                (plist-get details-payload :type)))
                              (mapcar #'slack-create-rich-text-block-element
                                      (plist-get details-payload :elements))))))

(defface slack-plan-block-title-face
  '((t (:weight bold :height 1.1)))
  "Face for plan layout block title."
  :group 'slack)

(defface slack-plan-task-complete-face
  '((t (:foreground "#859900")))
  "Face for completed plan tasks."
  :group 'slack)

(defface slack-plan-task-pending-face
  '((t (:foreground "#b58900")))
  "Face for pending plan tasks."
  :group 'slack)

(defun slack-plan-task-status-marker (status)
  (cond ((null status) "•")
        ((string= "complete" status)
         (propertize "✓" 'face 'slack-plan-task-complete-face))
        ((string= "in_progress" status)
         (propertize "◐" 'face 'slack-plan-task-pending-face))
        ((string= "failed" status)
         (propertize "✗" 'face 'slack-button-danger-block-element-face))
        (t (propertize "•" 'face 'slack-plan-task-pending-face))))

(cl-defmethod slack-block-to-string ((this slack-plan-task-card) &optional option)
  (with-slots (title status details) this
    (let* ((marker (slack-plan-task-status-marker status))
           (title-face (if (and status (string= "complete" status))
                           'slack-plan-task-complete-face
                         'slack-plan-task-pending-face))
           (header (format "  %s %s"
                           marker
                           (propertize (or title "") 'face title-face)))
           (details-str (when details
                          (let ((s (mapconcat
                                    #'(lambda (el)
                                        (slack-block-to-string el option))
                                    details
                                    "")))
                            (when (and s (> (length s) 0))
                              (mapconcat #'(lambda (line) (concat "      " line))
                                         (split-string s "\n")
                                         "\n"))))))
      (if details-str
          (concat header "\n" details-str)
        header))))

(defclass slack-plan-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "plan")
   (title :initarg :title :type (or null string) :initform nil)
   (tasks :initarg :tasks :type list :initform nil)))

(defun slack-create-plan-layout-block (payload)
  (make-instance 'slack-plan-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :title (plist-get payload :title)
                 :tasks (mapcar #'slack-create-plan-task-card
                                (plist-get payload :tasks))
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-plan-layout-block) &optional option)
  (with-slots (title tasks) this
    (let ((header (when title
                    (propertize title 'face 'slack-plan-block-title-face)))
          (task-strs (mapcar #'(lambda (task)
                                 (slack-block-to-string task option))
                             tasks)))
      (mapconcat #'identity
                 (cl-remove-if #'null
                               (append (list header) task-strs))
                 "\n"))))

(defface slack-table-border-face
  '((t (:foreground "#586e75")))
  "Face used for table borders."
  :group 'slack)

(defface slack-table-header-face
  '((t (:weight bold)))
  "Face used for table header cells."
  :group 'slack)

(defclass slack-table-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "table")
   (rows :initarg :rows :type list :initform nil)
   (num-columns :initarg :num_columns :type (or null number) :initform nil)
   (border :initarg :border :type (or null number) :initform nil)))

(defun slack-create-table-cell (cell)
  "Build a renderable value for a table CELL.
CELL may be nil, a `raw_text' plist, a `rich_text' plist (with or
without a `block_id'), or something else we don't yet understand."
  (cond
   ((null cell) nil)
   ((and (listp cell) (plist-member cell :type))
    (let ((type (plist-get cell :type)))
      (cond
       ((string= "rich_text" type)
        ;; `slack-rich-text-block' requires a string `block-id'; fall
        ;; back to an empty string when the payload omits one.
        (slack-create-rich-text-block
         (if (plist-get cell :block_id)
             cell
           (plist-put (copy-sequence cell) :block_id ""))))
       ((string= "raw_text" type)
        (or (plist-get cell :text) ""))
       (t (format "%S" cell)))))
   ((stringp cell) cell)
   (t (format "%S" cell))))

(defun slack-create-table-layout-block (payload)
  (make-instance 'slack-table-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :rows (mapcar #'(lambda (row)
                                   (mapcar #'slack-create-table-cell row))
                                (plist-get payload :rows))
                 :num_columns (plist-get payload :num_columns)
                 :border (plist-get payload :border)
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-table-layout-block) &optional option)
  (with-slots (rows) this
    (when (and rows (cl-every #'listp rows))
      (let* ((rendered-rows (mapcar #'(lambda (row)
                                        (mapcar #'(lambda (cell)
                                                    (let ((s (cond
                                                              ((null cell) "")
                                                              ((stringp cell) cell)
                                                              (t (slack-block-to-string cell option)))))
                                                      (or s "")))
                                                  row))
                                      rows))
             (num-cols (apply #'max (mapcar #'length rendered-rows)))
             (col-widths
              (cl-loop for col from 0 below num-cols
                       collect (apply #'max 1
                                      (mapcar #'(lambda (row)
                                                  (string-width
                                                   (substring-no-properties (or (nth col row) ""))))
                                                rendered-rows))))
             (pad (lambda (s w)
                    (let ((plain (substring-no-properties (or s ""))))
                      (concat plain (make-string (max 0 (- w (string-width plain))) ? )))))
             (border (propertize "|" 'face 'slack-table-border-face))
             (hline (concat (propertize "+" 'face 'slack-table-border-face)
                            (mapconcat #'(lambda (w)
                                           (propertize (make-string (+ w 2) ?-)
                                                       'face 'slack-table-border-face))
                                         col-widths
                                         (propertize "+" 'face 'slack-table-border-face))
                            (propertize "+" 'face 'slack-table-border-face)))
             (format-row #'(lambda (row header-p)
                             (apply #'concat border
                                     (cl-loop for col from 0 below num-cols
                                              for w = (nth col col-widths)
                                              for cell = (nth col row)
                                              for padded = (funcall pad cell w)
                                              collect (concat " "
                                                              (if header-p
                                                                  (propertize padded 'face 'slack-table-header-face)
                                                                padded)
                                                              " "
                                                              border)))
                                     )))
        (concat hline "\n"
                (funcall format-row (car rendered-rows) t) "\n"
                hline "\n"
                (mapconcat #'(lambda (row) (funcall format-row row nil))
                           (cdr rendered-rows)
                           (concat "\n"))
                (when (cdr rendered-rows) "\n")
                 hline)))))

(defface slack-timeline-point-complete-face
  '((t (:foreground "#859900")))
  "Face for completed timeline points."
  :group 'slack)

(defface slack-timeline-point-in-progress-face
  '((t (:foreground "#b58900")))
  "Face for in-progress timeline points."
  :group 'slack)

(defface slack-timeline-point-pending-face
  '((t (:foreground "#586e75")))
  "Face for pending timeline points."
  :group 'slack)

(defclass slack-timeline-point ()
  ((id :initarg :id :type (or null string) :initform nil)
   (text :initarg :text :type (or null string) :initform nil)
   (status :initarg :status :type (or null string) :initform nil)
   (contents :initarg :contents :type (or null list) :initform nil)
   (icon :initarg :icon :type (or null string) :initform nil)))

(defun slack-create-timeline-point (payload)
  (make-instance 'slack-timeline-point
                 :id (plist-get payload :id)
                 :text (plist-get payload :text)
                 :status (plist-get payload :status)
                 :contents (plist-get payload :contents)
                 :icon (plist-get payload :icon)))

(cl-defmethod slack-block-to-string ((this slack-timeline-point) &optional _option)
  (with-slots (text status) this
    (let ((marker (cond ((string= "complete" status)
                         (propertize "✓" 'face 'slack-timeline-point-complete-face))
                        ((string= "in_progress" status)
                         (propertize "◐" 'face 'slack-timeline-point-in-progress-face))
                        (t (propertize "○" 'face 'slack-timeline-point-pending-face)))))
      (format "  %s %s" marker (or text "")))))

(defclass slack-timeline-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "timeline")
   (points :initarg :points :type list :initform nil)
   (start-ts :initarg :start_ts :type (or null number) :initform nil)
   (end-ts :initarg :end_ts :type (or null number) :initform nil)))

(defun slack-create-timeline-layout-block (payload)
  (make-instance 'slack-timeline-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :points (mapcar #'slack-create-timeline-point
                                 (plist-get payload :points))
                 :start_ts (plist-get payload :start_ts)
                 :end_ts (plist-get payload :end_ts)
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-timeline-layout-block) &optional option)
  (with-slots (points) this
    (if points
        (concat (mapconcat #'(lambda (point) (slack-block-to-string point option))
                            points
                            "\n")
                "\n")
      "")))

(defface slack-video-block-title-face
  '((t (:weight bold)))
  "Face for video block title."
  :group 'slack)

(defface slack-video-block-meta-face
  '((t (:foreground "#586e75")))
  "Face for video block metadata."
  :group 'slack)

(defclass slack-video-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "video")
   (video-url :initarg :video_url :type (or null string) :initform nil)
   (thumbnail-url :initarg :thumbnail_url :type (or null string) :initform nil)
   (alt-text :initarg :alt_text :type (or null string) :initform nil)
   (title :initarg :title :type (or null slack-text-message-composition-object) :initform nil)
   (title-url :initarg :title_url :type (or null string) :initform nil)
   (author-name :initarg :author_name :type (or null string) :initform nil)
   (provider-name :initarg :provider_name :type (or null string) :initform nil)
   (provider-icon-url :initarg :provider_icon_url :type (or null string) :initform nil)
   (description :initarg :description :type (or null slack-text-message-composition-object) :initform nil)))

(defun slack-create-video-layout-block (payload)
  (make-instance 'slack-video-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :video_url (plist-get payload :video_url)
                 :thumbnail_url (plist-get payload :thumbnail_url)
                 :alt_text (plist-get payload :alt_text)
                 :title (slack-create-text-message-composition-object
                         (plist-get payload :title))
                 :title_url (plist-get payload :title_url)
                 :author_name (plist-get payload :author_name)
                 :provider_name (plist-get payload :provider_name)
                 :provider_icon_url (plist-get payload :provider_icon_url)
                 :description (slack-create-text-message-composition-object
                               (plist-get payload :description))
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-video-layout-block) &optional _option)
  (with-slots (video-url title title-url author-name provider-name description alt-text) this
    (let* ((title-str (when title (slack-block-to-string title)))
           (desc-str (when description (slack-block-to-string description)))
           (parts (cl-remove-if #'null
                     (list (when title-str
                             (propertize title-str 'face 'slack-video-block-title-face))
                           (when author-name
                             (propertize (format "by %s" author-name)
                                         'face 'slack-video-block-meta-face))
                           (when provider-name
                             (propertize (format "on %s" provider-name)
                                         'face 'slack-video-block-meta-face))
                           (when desc-str
                             (propertize desc-str 'face 'slack-video-block-meta-face))
                           (when (or title-url video-url)
                             (propertize "[video]"
                                         'face 'slack-channel-button-face
                                         'slack-attachment-mention-url (or title-url video-url)
                                         'keymap slack-attachment-mention-keymap
                                         'help-echo "RET: open video"))))))
      (if parts
          (concat (mapconcat #'identity parts " · ") "\n")
        ""))))

(defface slack-input-block-label-face
  '((t (:weight bold)))
  "Face for input layout block labels."
  :group 'slack)

(defface slack-input-block-hint-face
  '((t (:inherit shadow)))
  "Face for input layout block hints and optional markers."
  :group 'slack)

(defclass slack-input-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "input")
   (label :initarg :label :type (or null slack-text-message-composition-object) :initform nil)
   (hint :initarg :hint :type (or null slack-text-message-composition-object) :initform nil)
   (optional :initarg :optional :type boolean :initform nil)
   (dispatch-action :initarg :dispatch_action :type boolean :initform nil)
   (element :initarg :element :initform nil :type (or null slack-block-element))))

(defun slack-create-input-layout-block (payload)
  (let ((block-id (plist-get payload :block_id)))
    (make-instance 'slack-input-layout-block
                   :type (plist-get payload :type)
                   :block_id block-id
                   :label (slack-create-text-message-composition-object
                           (plist-get payload :label))
                   :hint (slack-create-text-message-composition-object
                          (plist-get payload :hint))
                   :optional (eq t (plist-get payload :optional))
                   :dispatch_action (eq t (plist-get payload :dispatch_action))
                   :element (slack-create-block-element
                             (plist-get payload :element)
                             block-id)
                   :payload payload)))

(cl-defmethod slack-block-to-string ((this slack-input-layout-block) &optional option)
  (with-slots (label hint optional element) this
    (let* ((label-str (when label (slack-block-to-string label option)))
           (optional-str (when optional
                           (propertize " (optional)"
                                       'face 'slack-input-block-hint-face)))
           (header (when label-str
                     (concat (propertize label-str
                                         'face 'slack-input-block-label-face)
                             optional-str)))
           (element-str (when element (slack-block-to-string element option)))
           (hint-str (when hint
                       (propertize (slack-block-to-string hint option)
                                   'face 'slack-input-block-hint-face))))
      (mapconcat #'identity
                 (cl-remove-if #'null (list header element-str hint-str))
                 "\n"))))

(cl-defmethod slack-block-find-action ((this slack-input-layout-block) action-id)
  (with-slots (element) this
    (when (and element
               (string= (slack-block-action-id element) action-id))
      element)))

(defclass slack-rich-text-block-element ()
  ((type :initarg :type :type string)
   (elements :initarg :elements :type list) ;; list of slack-rich-text-element
   (payload :initarg :payload :type (or null list) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S\n" (oref this payload)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-block-element) &optional _option)
  (format "Implement `slack-block-to-mrkdwn' for %S\n" (oref this payload)))

(defun slack-create-rich-text-block-element (payload)
  (let* ((type (plist-get payload :type))
         (element (cond
                   ((string= "rich_text_section" type)
                    (slack-create-rich-text-section payload))
                   ((string= "rich_text_preformatted" type)
                    (slack-create-rich-text-preformatted payload))
                   ((string= "rich_text_quote" type)
                    (slack-create-rich-text-quote payload))
                   ((string= "rich_text_list" type)
                    (slack-create-rich-text-list payload))
                   (t
                    (make-instance 'slack-rich-text-block-element
                                   :type (plist-get payload :type)
                                   :elements (mapcar #'slack-create-rich-text-element
                                                     (plist-get payload :elements)))))))
    (oset element payload payload)
    element))

(defclass slack-rich-text-section (slack-rich-text-block-element) ())

(cl-defmethod slack-block-to-string ((this slack-rich-text-section) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-string element option))
             (oref this elements)
             ""))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-section) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
             (oref this elements)
             ""))

(defun slack-create-rich-text-section (payload)
  (make-instance 'slack-rich-text-section
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-element
                                   (plist-get payload :elements))))

;; Code block does not use `style' in `slack-rich-text-element'
(defclass slack-rich-text-preformatted (slack-rich-text-block-element) ())

(cl-defmethod slack-block-to-string ((this slack-rich-text-preformatted) &optional option)
  (let ((text (mapconcat #'(lambda (element) (slack-block-to-string element option))
                         (oref this elements)
                         "")))
    (if (not slack-block-highlight-source)
        (propertize (concat text "\n")
                    'slack-defer-face #'(lambda (beg end)
                                          (overlay-put (make-overlay beg end)
                                                       'face 'slack-mrkdwn-code-block-face))
                    'face 'slack-mrkdwn-code-block-face)
      (pcase-let ((`(,lang . ,hl-text) (slack-block-fontify-text-natively text)))
        (concat
         "\n"
         (propertize
          (format "┌─ %s" lang)
          'face 'slack-block-highlight-source-overlay-face)
         "\n"
         (mapconcat
          'identity
          (mapcar
           (lambda (s)
             (propertize
              (if (string= "" s) " " s)
              'slack-defer-face #'(lambda (beg _end)
                                    (let ((ov (make-overlay beg beg)))
                                      (overlay-put
                                       ov 'before-string
                                       (propertize "│" 'face 'slack-block-highlight-source-overlay-face))))))
           (string-split (string-trim hl-text) "\n"))
          "\n")
         "\n"
         (propertize
          "└─"
          'face 'slack-block-highlight-source-overlay-face)
         "\n")))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-preformatted) &optional option)
  (let ((text (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
                         (oref this elements)
                         "")))
    (format "```\n%s\n\n```\n" text)))

(defun slack-create-rich-text-preformatted (payload)
  (make-instance 'slack-rich-text-preformatted
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-element
                                   (plist-get payload :elements))
                 ))

(defclass slack-rich-text-quote (slack-rich-text-block-element) ())

(cl-defmethod slack-block-to-string ((this slack-rich-text-quote) &optional option)
  (let* ((text (mapconcat #'(lambda (element) (slack-block-to-string element option))
                          (oref this elements)
                          ""))
         (texts (split-string text "[\n\r]" nil nil))
         (text-with-pad (mapconcat #'(lambda (text) (format "%s%s"
                                                            slack-mrkdwn-blockquote-sign
                                                            text))
                                   texts
                                   "\n")))

    (propertize (concat text-with-pad "\n")
                'face 'slack-mrkdwn-blockquote-face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-quote) &optional option)
  (let* ((text (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
                          (oref this elements)
                          ""))
         (texts (split-string text "[\n\r]" nil nil)))

    (concat (mapconcat #'(lambda (text) (format "> %s" text))
                       texts
                       "\n")
            "\n")))

(defun slack-create-rich-text-quote (payload)
  (make-instance 'slack-rich-text-quote
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-element
                                   (plist-get payload :elements))))

(defclass slack-rich-text-list (slack-rich-text-block-element)
  ((indent :initarg :indent :type number)
   (style :initarg :style :type string) ;; bullet or ordered
   ))

(cl-defmethod slack-block-to-string ((this slack-rich-text-list) &optional option)
  (let ((indent (make-string (* 2 (oref this indent)) ? ))
        (texts (mapcar #'(lambda (element) (slack-block-to-string element option))
                       (oref this elements)))
        (texts-with-dot nil)
        (dot slack-mrkdwn-list-bullet)
        (i 1))
    (dolist (text texts)
      (push (format "%s%s %s"
                    indent
                    (propertize (if (string= (oref this style) "ordered")
                                    (format "%s." i)
                                  dot)
                                'face 'slack-mrkdwn-list-face)
                    text)
            texts-with-dot)
      (setq i (+ i 1)))
    (concat (mapconcat #'identity (reverse texts-with-dot) "\n")
            "\n")))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-list) &optional option)
  (let* ((indent (make-string (* 2 (oref this indent)) ? ))
         (dot "-")
         (i 1))
    (concat (mapconcat #'(lambda (element)
                           (let ((text (format "%s%s %s"
                                               indent
                                               (if (string= (oref this style) "ordered")
                                                   (format "%s." i)
                                                 dot)
                                               (slack-block-to-mrkdwn element option))))
                             (setq i (+ i 1))
                             text))
                       (oref this elements)
                       "\n")
            "\n")))

(defun slack-create-rich-text-list (payload)
  (make-instance 'slack-rich-text-list
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-block-element
                                   (plist-get payload :elements))
                 :indent (plist-get payload :indent)
                 :style (plist-get payload :style)))


(defclass slack-rich-text-element-style ()
  ((bold :initarg :bold :type boolean)
   (italic :initarg :italic :type boolean)
   (strike :initarg :strike :type boolean)
   (code :initarg :code :type boolean)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-element-style) text)
  "Add property to TEXT according to THIS rich text."
  (let ((face (progn (or (and (oref this bold) 'slack-mrkdwn-bold-face)
                         (and (oref this italic) 'slack-mrkdwn-italic-face)
                         (and (oref this strike) 'slack-mrkdwn-strike-face)
                         (and (oref this code) 'slack-mrkdwn-code-face)))))
    (propertize text
                'face face
                ;; apparently font-lock is enabled with lui and the 'face ends up ignored
                'font-lock-face face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-element-style) text)
  (or (and (oref this bold) (format "*%s*" text))
      (and (oref this italic) (format "_%s_" text))
      (and (oref this strike) (format "~%s~" text))
      (and (oref this code) (format "`%s`" text))
      text))

(defun slack-create-rich-text-element-style (payload)
  (when payload
    (make-instance 'slack-rich-text-element-style
                   :bold (eq t (plist-get payload :bold))
                   :italic (eq t (plist-get payload :italic))
                   :strike (eq t (plist-get payload :strike))
                   :code (eq t (plist-get payload :code)))))

(defclass slack-rich-text-element ()
  ((type :initarg :type :type string)
   (style :initarg :style :type (or null slack-rich-text-element-style))
   (payload :initarg :payload :type (or null list) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S\n" (oref this payload)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-element) &optional _option)
  (format "Implement `slack-block-to-mrkdwn' for %S\n" (oref this payload)))

(defun slack-create-rich-text-element (payload)
  (let* ((type (plist-get payload :type))
         (element (cond
                   ((string= "text" type)
                    (slack-create-rich-text-text-element payload))
                   ((string= "channel" type)
                    (slack-create-rich-text-channel-element payload))
                   ((string= "user" type)
                    (slack-create-rich-text-user-element payload))
                   ((string= "emoji" type)
                    (slack-create-rich-text-emoji-element payload))
                   ((string= "link" type)
                    (slack-create-rich-text-link-element payload))
                   ((string= "team" type)
                    (slack-create-rich-text-team-element payload))
                   ((string= "usergroup" type)
                    (slack-create-rich-text-usergroup-element payload))
                   ((string= "date" type)
                    (slack-create-rich-text-date-element payload))
                   ((string= "broadcast" type)
                    (slack-create-rich-text-broadcast-element payload))
                   ((string= "message_mention" type)
                    (slack-create-rich-text-message-mention-element payload))
                   ((string= "attachment_mention" type)
                    (slack-create-rich-text-attachment-mention-element payload))
                   ((string= "canvas" type)
                    (slack-create-rich-text-canvas-element payload))
                   ((string= "citation" type)
                    (slack-create-rich-text-citation-element payload))
                   (t
                    (make-instance 'slack-rich-text-element
                                   :type (plist-get payload :type)
                                   :style (slack-create-rich-text-element-style
                                           (plist-get payload :style)))))))
    (oset element payload payload)
    element))

(defclass slack-rich-text-text-element (slack-rich-text-element)
  ((text :initarg :text :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-text-element) &optional _option)
  (let ((style (oref this style))
        (text (oref this text)))
    (if style (slack-block-to-string style text)
      (propertize text 'face 'slack-message-output-text))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-text-element) &optional _option)
  (let ((style (oref this style))
        (text (oref this text)))
    (if style (slack-block-to-mrkdwn style text)
      text)))

(defun slack-create-rich-text-text-element (payload)
  (make-instance 'slack-rich-text-text-element
                 :type (plist-get payload :type)
                 :text (plist-get payload :text)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

;; (:type "channel" :channel_id "CE096203E")
(defclass slack-rich-text-channel-element (slack-rich-text-element)
  ((channel-id :initarg :channel_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-channel-element) option)
  (let* ((team (plist-get option :team))
         (id (oref this channel-id))
         (room (slack-room-find id team))
         (room-name (if room (slack-room-name room team) "(no room)")))
    (unless team
      (error "`slack-rich-text-channel-element' need team as option"))

    (propertize (format "#%s" room-name)
                'room-id id
                'keymap slack-channel-button-keymap
                'face 'slack-channel-button-face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-channel-element) option)
  (let ((team (plist-get option :team))
        (id (oref this channel-id)))
    (unless team
      (error "`slack-rich-text-channel-element' need team as option"))

    (slack-propertize-mention-text 'slack-message-mention-face
                                   (format "#%s" (slack-room-name (slack-room-find id team) team))
                                   (format "<#%s>" id))))

(defun slack-create-rich-text-channel-element (payload)
  (make-instance 'slack-rich-text-channel-element
                 :type (plist-get payload :type)
                 :channel_id (plist-get payload :channel_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-user-element (slack-rich-text-element)
  ((user-id :initarg :user_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-user-element) option)
  (let ((team (plist-get option :team))
        (id (oref this user-id)))
    (unless team
      (error "`slack-rich-text-user-element' need team as option"))
    (let ((text (propertize (format "@%s" (or (slack-user-name id team) id))
                           'user-id id
                           'mouse-face 'highlight
                           'keymap slack-user-mention-keymap
                           'face 'slack-message-mention-face)))
      (when (slack-user-vip-p-id id team)
        (add-face-text-property 0 (length text) 'slack-user-vip-face nil text))
      text)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-user-element) option)
  (let ((team (plist-get option :team))
        (id (oref this user-id)))
    (unless team
      (error "`slack-rich-text-user-element' need team as option"))
    (slack-propertize-mention-text 'slack-message-mention-face
                                   (format "@%s" (or (slack-user-name id team) id))
                                   (format "<@%s>" id))))

(defun slack-create-rich-text-user-element (payload)
  (make-instance 'slack-rich-text-user-element
                 :type (plist-get payload :type)
                 :user_id (plist-get payload :user_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-emoji-element (slack-rich-text-element)
  ((name :initarg :name :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-emoji-element) &optional _option)
  (let ((name (oref this name)))
    (format ":%s:" name)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-emoji-element) &optional option)
  (slack-block-to-string this option))

(defun slack-create-rich-text-emoji-element (payload)
  (make-instance 'slack-rich-text-emoji-element
                 :type (plist-get payload :type)
                 :name (plist-get payload :name)))

(defclass slack-rich-text-link-element (slack-rich-text-element)
  ((url :initarg :url :type string)
   (text :initarg :text :type (or null string) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-link-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (format "<%s|%s>" url (or text url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-link-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if text
        (format "[%s](%s)" text url)
      url)))

(defun slack-create-rich-text-link-element (payload)
  (make-instance 'slack-rich-text-link-element
                 :type (plist-get payload :type)
                 :url (plist-get payload :url)
                 :text (plist-get payload :text)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-message-mention-element (slack-rich-text-element)
  ((url :initarg :url :type string)
   (text :initarg :text :type (or null string) :initform nil)
   (channel-id :initarg :channel_id :type (or null string) :initform nil)
   (author-id :initarg :author_id :type (or null string) :initform nil)
   (message-ts :initarg :message_ts :type (or null string) :initform nil)
   (thread-ts :initarg :thread_ts :type (or null string) :initform nil)))

(defvar slack-message-mention-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-open-message-mention-url)
    (define-key keymap [mouse-1] #'slack-open-message-mention-url)
    keymap))

(defun slack-open-message-mention-url ()
  "Open the message mention URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'slack-message-mention-url)))
    (when url
      (slack-open-url url))))

(cl-defmethod slack-block-to-string ((this slack-rich-text-message-mention-element) option)
  (let* ((team (plist-get option :team))
         (url (oref this url))
         (author-name (when (and team (oref this author-id))
                        (or (slack-user-name (oref this author-id) team)
                            (oref this author-id))))
         (channel-name (when (and team (oref this channel-id))
                         (let ((room (slack-room-find (oref this channel-id) team)))
                           (when room
                             (slack-room-name room team)))))
         (label (cond (author-name
                       (if channel-name
                           (format "@%s in #%s" author-name channel-name)
                         (format "@%s" author-name)))
                      (channel-name
                       (format "#%s" channel-name))
                      (t url))))
    (propertize label
                'face 'slack-channel-button-face
                'slack-message-mention-url url
                'keymap slack-message-mention-keymap
                'help-echo (format "RET: open message\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-message-mention-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if text
        (format "[%s](%s)" text url)
      url)))

(defun slack-create-rich-text-message-mention-element (payload)
  (make-instance 'slack-rich-text-message-mention-element
                 :type (plist-get payload :type)
                 :url (plist-get payload :url)
                 :text (plist-get payload :text)
                 :channel_id (plist-get payload :channel_id)
                 :author_id (plist-get payload :author_id)
                 :message_ts (plist-get payload :message_ts)
                 :thread_ts (plist-get payload :thread_ts)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-attachment-mention-element (slack-rich-text-element)
  ((url :initarg :url :type (or null string) :initform nil)
   (text :initarg :text :type (or null string) :initform nil)
   (app-id :initarg :app_id :type (or null string) :initform nil)
   (entity-id :initarg :entity_id :type (or null string) :initform nil)
   (icon-url :initarg :icon_url :type (or null string) :initform nil)
   (icon-name :initarg :icon_name :type (or null string) :initform nil)
   (product-name :initarg :product_name :type (or null string) :initform nil)
   (ts :initarg :ts :type (or null string) :initform nil)
   (channel-id :initarg :channel_id :type (or null string) :initform nil)))

(defvar slack-attachment-mention-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-open-attachment-mention-url)
    (define-key keymap [mouse-1] #'slack-open-attachment-mention-url)
    keymap))

(defun slack-open-attachment-mention-url ()
  "Browse the attachment mention URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'slack-attachment-mention-url)))
    (when url
      (browse-url url))))

(cl-defmethod slack-block-to-string ((this slack-rich-text-attachment-mention-element) &optional _option)
  (let* ((url (oref this url))
         (text (or (oref this text) url))
         (product (oref this product-name))
         (label (if product
                    (format "%s: %s" product text)
                  text)))
    (propertize label
                'face 'slack-channel-button-face
                'slack-attachment-mention-url url
                'keymap slack-attachment-mention-keymap
                'help-echo (format "RET: open link\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-attachment-mention-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if text
        (format "[%s](%s)" text url)
      url)))

(defun slack-create-rich-text-attachment-mention-element (payload)
  (make-instance 'slack-rich-text-attachment-mention-element
                 :type (plist-get payload :type)
                 :url (plist-get payload :url)
                 :text (plist-get payload :text)
                 :app_id (plist-get payload :app_id)
                 :entity_id (plist-get payload :entity_id)
                 :icon_url (plist-get payload :icon_url)
                 :icon_name (plist-get payload :icon_name)
                 :product_name (plist-get payload :product_name)
                 :ts (plist-get payload :ts)
                 :channel_id (plist-get payload :channel_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-canvas-element (slack-rich-text-element)
  ((file-id :initarg :file_id :type (or null string) :initform nil)
   (url :initarg :url :type (or null string) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-canvas-element) &optional _option)
  (let ((url (oref this url)))
    (propertize (or url "Canvas")
                'face 'slack-channel-button-face
                'slack-attachment-mention-url url
                'keymap slack-attachment-mention-keymap
                'help-echo (format "RET: open link\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-canvas-element) &optional _option)
  (let ((url (oref this url)))
    (if url
        (format "[%s](%s)" url url)
      "Canvas")))

(defun slack-create-rich-text-canvas-element (payload)
  (make-instance 'slack-rich-text-canvas-element
                 :type (plist-get payload :type)
                 :file_id (plist-get payload :file_id)
                 :url (plist-get payload :url)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-citation-element (slack-rich-text-element)
  ((text :initarg :text :type (or null string) :initform nil)
   (url :initarg :url :type (or null string) :initform nil)
   (index :initarg :index :type (or null number) :initform nil)
   (details :initarg :details :type (or null list) :initform nil)))

(defvar slack-citation-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-open-citation)
    (define-key keymap [mouse-1] #'slack-open-citation)
    (define-key keymap (kbd "w") #'slack-copy-citation-url)
    keymap))

(defun slack-copy-citation-url ()
  "Copy the citation URL at point to the kill ring."
  (interactive)
  (let ((url (get-text-property (point) 'slack-message-mention-url)))
    (when url
      (kill-new url)
      (message "Copied: %s" url))))

(defun slack-open-citation ()
  "Open the citation message at point."
  (interactive)
  (let ((url (get-text-property (point) 'slack-message-mention-url))
        (channel-id (get-text-property (point) 'slack-citation-channel-id))
        (message-ts (get-text-property (point) 'slack-citation-message-ts)))
    (if (and channel-id message-ts)
        (cl-block nil
          (let* ((team (cl-find-if
                        #'(lambda (team)
                            (slack-room-find channel-id team))
                        (hash-table-values slack-teams-by-token))))
            (when team
              (let ((room (slack-room-find channel-id team)))
                (when room
                  (slack-open-message team room message-ts nil)
                  (cl-return t)))))
          (when url (browse-url url)))
      (when url (browse-url url)))))

(cl-defmethod slack-block-to-string ((this slack-rich-text-citation-element) &optional _option)
  (let ((url (oref this url))
        (text (or (oref this text) ""))
        (details (oref this details))
        (channel-id nil)
        (message-ts nil))
    (when details
      (setq channel-id (plist-get details :channel)
            message-ts (plist-get details :message_ts)))
    (propertize text
                'face 'slack-channel-button-face
                'slack-message-mention-url url
                'slack-citation-channel-id channel-id
                'slack-citation-message-ts message-ts
                'keymap slack-citation-keymap
                'help-echo (format "RET: open message\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-citation-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if (and text url)
        (format "[%s](%s)" text url)
      (or text url ""))))

(defun slack-create-rich-text-citation-element (payload)
  (make-instance 'slack-rich-text-citation-element
                 :type (plist-get payload :type)
                 :text (plist-get payload :text)
                 :url (plist-get payload :url)
                 :index (plist-get payload :index)
                 :details (plist-get payload :details)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-team-element (slack-rich-text-element)
  ((team-id :initarg :team_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-team-element) &optional _option)
  (let* ((team-id (oref this team-id))
         (team (slack-team-find team-id)))
    (propertize (format "%s" (slack-team-name team))
                'face 'slack-message-mention-face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-team-element) &optional _option)
  (slack-block-to-string this))

(defun slack-create-rich-text-team-element (payload)
  (make-instance 'slack-rich-text-team-element
                 :type (plist-get payload :type)
                 :team_id (plist-get payload :team_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-usergroup-element (slack-rich-text-element)
  ((usergroup-id :initarg :usergroup_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-usergroup-element) &optional option)
  (let ((team (plist-get option :team))
        (id (oref this usergroup-id)))

    (unless team
      (error "`slack-rich-text-usergroup-element' need team as option"))

    (let ((usergroup (slack-usergroup-find id team)))
      (propertize (slack-format-usergroup usergroup)
                  'face 'slack-message-mention-keyword-face))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-usergroup-element) &optional option)
  (let ((team (plist-get option :team))
        (id (oref this usergroup-id)))

    (unless team
      (error "`slack-rich-text-usergroup-element' need team as option"))

    (let ((usergroup (slack-usergroup-find id team)))
      (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                     (slack-format-usergroup usergroup)
                                     (format "<!subteam^%s>" id)))))

(defun slack-create-rich-text-usergroup-element (payload)
  (make-instance 'slack-rich-text-usergroup-element
                 :type (plist-get payload :type)
                 :usergroup_id (plist-get payload :usergroup_id)))

(defclass slack-rich-text-date-element (slack-rich-text-element)
  ((timestamp :initarg :timestamp :type number)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-date-element) &optional _option)
  (slack-format-ts (oref this timestamp)))

(defun slack-create-rich-text-date-element (payload)
  (make-instance 'slack-rich-text-date-element
                 :type (plist-get payload :type)
                 :timestamp (if (stringp (plist-get payload :timestamp))
                                (string-to-number (plist-get payload :timestamp))
                              (plist-get payload :timestamp))))

(defclass slack-rich-text-broadcast-element (slack-rich-text-element)
  ((range :initarg :range :type string) ;; channel or everyone or here
   ))

(cl-defmethod slack-block-to-string ((this slack-rich-text-broadcast-element) &optional _option)
  (propertize (format "@%s" (oref this range))
              'face 'slack-message-mention-keyword-face))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-broadcast-element) &optional _option)
  (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                 (format "@%s" (oref this range))
                                 (format "<!%s>" (oref this range))))

(defun slack-create-rich-text-broadcast-element (payload)
  (make-instance 'slack-rich-text-broadcast-element
                 :type (plist-get payload :type)
                 :range (plist-get payload :range)))

(defclass slack-section-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "section")
   (text :initarg :text :type (or null slack-text-message-composition-object) :initform nil)
   (fields :initarg :fields :type (or list null) :initform nil) ;; list of slack-text-message-composition-object
   (accessory :initarg :accessory :initform nil :type (or null slack-block-element))))

(defun slack-create-section-layout-block (payload)
  (let ((accessory (slack-create-block-element
                    (plist-get payload :accessory)
                    (plist-get payload :block_id))))
    (make-instance 'slack-section-layout-block
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :block_id (plist-get payload :block_id)
                   :fields (mapcar #'slack-create-text-message-composition-object
                                   (plist-get payload :fields))
                   :accessory accessory)))

(cl-defmethod slack-block-to-string ((this slack-section-layout-block) &optional _option)
  (with-slots (fields accessory text) this
    (slack-format-message (slack-block-to-string text)
                          (mapconcat #'identity
                                     (mapcar #'slack-block-to-string
                                             fields)
                                     "\n")
                          (slack-block-to-string accessory))))

(cl-defmethod slack-block-find-action ((this slack-section-layout-block) action-id)
  (with-slots (accessory) this
    (when (and accessory
               (string= (slack-block-action-id accessory)
                        action-id))
      accessory)))

(defclass slack-divider-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "divider")))

(defun slack-create-divider-layout-block (payload)
  (make-instance 'slack-divider-layout-block
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((_this slack-divider-layout-block) &optional _option)
  (let ((columns (or lui-fill-column
                     0)))
    (make-string columns ?-)))

(defclass slack-image-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (title :initarg :title :initform nil (or null slack-text-message-composition-object))
   (image-height :initarg :image_height :type number)
   (image-width :initarg :image_width :type number)
   (image-bytes :initarg :image_bytes :type number)))

(defun slack-create-image-layout-block (payload)
  (make-instance 'slack-image-layout-block
                 :image_url (plist-get payload :image_url)
                 :alt_text (plist-get payload :alt_text)
                 :title (slack-create-text-message-composition-object
                         (plist-get payload :title))
                 :block_id (plist-get payload :block_id)
                 :image_width (plist-get payload :image_width)
                 :image_height (plist-get payload :image_height)
                 :image_bytes (plist-get payload :image_bytes)))

(cl-defmethod slack-block-to-string ((this slack-image-layout-block) &optional _option)
  (with-slots (image-url alt-text title image-height image-width image-bytes) this
    (let ((spec (list image-url
                      image-width
                      image-height
                      slack-image-max-height)))
      (slack-format-message (format "%s (%s kB)" alt-text (round (/ image-bytes 1000.0)))
                            (slack-image-string spec)))))

(defclass slack-actions-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "actions")
   (elements :initarg :elements :type list) ;; max 5 elements
   ))

(defun slack-create-actions-layout-block (payload)
  (make-instance 'slack-actions-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (slack-create-block-element
                                        e (plist-get payload :block_id)))
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-actions-layout-block) &optional _option)
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'slack-block-to-string
                       elements)
               " ")))

(cl-defmethod slack-block-find-action ((this slack-actions-layout-block) action-id)
  (with-slots (elements) this
    (cl-find-if #'(lambda (e) (string= action-id (slack-block-action-id e)))
                elements)))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)))

(defun slack-create-context-layout-block (payload)
  (make-instance 'slack-context-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (or (if (string= "image" (plist-get e :type))
                                               (slack-create-block-element e (plist-get payload :block_id))
                                             (slack-create-text-message-composition-object  e))))
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-context-layout-block) &optional _option)
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'(lambda (e) (slack-block-to-string e '(:max-image-height 30 :max-image-width 30)))
                       elements)
               " ")))

(cl-defmethod slack-block-find-action ((this slack-context-layout-block) action-id)
  (with-slots (elements) this
    (cl-find-if #'(lambda (e) (string= action-id (slack-block-action-id e)))
                elements)))

;; Block Elements
;; [Reference: Block elements | Slack](https://api.slack.com/reference/messaging/block-elements)
(defclass slack-block-element ()
  ((type :initarg :type :type string)
   (action-id :initarg :action_id :type string :initform "")
   (confirm :initarg :confirm :type (or null slack-confirmation-dialog-message-composition-object) :initform nil)
   (payload :initarg :payload :initform nil)))

(cl-defmethod slack-block-action-id ((this slack-block-element))
  (oref this action-id))

(cl-defmethod slack-block-to-string ((this slack-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

(cl-defmethod slack-block-handle-confirm ((this slack-block-element))
  (let ((confirm (oref this confirm)))
    (if (null confirm) t
      (slack-block-handle-confirm confirm))))

(cl-defmethod slack-block-select-from-options ((_this slack-block-element) options)
  (let ((alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                       options)))
    (slack-select-from-list (alist "Select Option: "))))

(defun slack-create-block-element (payload block-id)
  (when payload
    (let ((type (plist-get payload :type)))
      (cond
       ((string= "image" type)
        (slack-create-image-block-element payload))
       ((string= "button" type)
        (slack-create-button-block-element payload block-id))
       ((string= "static_select" type)
        (slack-create-static-select-block-element payload block-id))
       ((string= "external_select" type)
        (slack-create-external-select-block-element payload block-id))
       ((string= "users_select" type)
        (slack-create-user-select-block-element payload block-id))
       ((string= "conversations_select" type)
        (slack-create-conversation-select-block-element payload block-id))
       ((string= "channels_select" type)
        (slack-create-channel-select-block-element payload block-id))
       ((string= "overflow" type)
        (slack-create-overflow-block-element payload block-id))
       ((string= "datepicker" type)
        (slack-create-datepicker-block-element payload block-id))
       (t (make-instance 'slack-block-element
                         :type type
                         :payload payload))))))

(defclass slack-image-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (image-height :initarg :image_height :type (or null number))
   (image-width :initarg :image_width :type (or null number))
   (image-bytes :initarg :image_bytes :type (or null number))))

(defun slack-create-image-block-element (payload)
  (make-instance 'slack-image-block-element
                 :image_url (plist-get payload :image_url)
                 :alt_text (plist-get payload :alt_text)
                 :image_height (plist-get payload :image_height)
                 :image_width (plist-get payload :image_width)
                 :image_bytes (plist-get payload :image_bytes)))

(cl-defmethod slack-block-to-string ((this slack-image-block-element) &optional option)
  (with-slots (image-url image-height image-width) this
    (let ((spec (list image-url
                      image-width
                      image-height
                      (or (plist-get option :max-image-height)
                          slack-image-max-height)
                      (plist-get option :max-image-width))))
      (slack-image-string (cl-remove-if #'null spec)))))

(defclass slack-button-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "button")
   (text :initarg :text :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (block-id :initarg :block_id :type (or null string) :initform nil)
   (url :initarg :url :type (or string null) :initform nil)
   (value :initarg :value :type (or string null) :initform nil)
   (style :initarg :style :type string :initform "default") ;; primary, danger
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-button-block-element (payload block-id)
  (make-instance 'slack-button-block-element
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))
                 :block_id block-id
                 :action_id (plist-get payload :action_id)
                 :url (plist-get payload :url)
                 :value (plist-get payload :value)
                 :style (or (plist-get payload :style) "default")
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-button-block-action (buffer))

(defun slack-execute-button-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-button-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-button-block-element) &optional _option)
  (with-slots (text style) this
    (let ((face (cond ((string= "danger" style) 'slack-button-danger-block-element-face)
                      ((string= "primary" style) 'slack-button-primary-block-element-face)
                      (t 'slack-button-block-element-face))))
      (propertize (slack-block-to-string text)
                  'face face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET")
                              #'slack-execute-button-block-action)
                            map)))))

(defface slack-button-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to button block element"
  :group 'slack)

(defface slack-button-danger-block-element-face
  '((t (:inherit slack-button-block-element-face :color "#dc322f")))
  "Used to danger button block element"
  :group 'slack)

(defface slack-button-primary-block-element-face
  '((t (:inherit slack-button-block-element-face :color "#859900")))
  "Used to primary button block element"
  :group 'slack)

(cl-defmethod slack-block-action-payload ((this slack-button-block-element))
  (with-slots (block-id action-id value text) this
    (cl-remove-if #'null
                  (list (cons "block_id" (or block-id ""))
                        (cons "action_id" action-id)
                        (when value
                          (cons "value" value))
                        (cons "type" "button")
                        (cons "text" (slack-block-action-payload text))))))

(defclass slack-select-block-element (slack-block-element)
  ((placeholder :initarg :placeholder :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(cl-defmethod slack-block-to-string ((this slack-select-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

(cl-defmethod slack-block-select-from-option-groups ((_this slack-select-block-element) option-groups)
  (slack-if-let* ((group-alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                                       option-groups))
                  (group (slack-select-from-list (group-alist "Select Group: "))))
      (slack-block-select-from-option-group group)))

(defface slack-select-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to select block element"
  :group 'slack)

(defclass slack-static-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "static_select")
   (options :initarg :options :type (or null list) :initform nil) ;; list of slack-option-message-composition-object
   (option-groups :initarg :option_groups :type (or list null) :initform nil) ;; list of slack-option-groups-composition-object
   (initial-option :initarg :initial_option :initform nil (or null
                                                              slack-option-message-composition-object))
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-static-select-block-element (payload block-id)
  (let* ((options (plist-get payload :options))
         (option-groups (plist-get payload :option_groups))
         (initial-option (plist-get payload :initial_option)))
    (make-instance 'slack-static-select-block-element
                   :placeholder (slack-create-text-message-composition-object
                                 (plist-get payload :placeholder))
                   :action_id (plist-get payload :action_id)
                   :block_id block-id
                   :confirm (slack-create-confirmation-dialog-message-composition-object
                             (plist-get payload :confirm))
                   :options (when options
                              (mapcar #'slack-create-option-message-composition-object
                                      options))
                   :option_groups (unless options
                                    (mapcar #'slack-create-option-group-message-composition-object
                                            option-groups))
                   :initial_option (slack-create-option-message-composition-object
                                    initial-option))))

(cl-defgeneric slack-buffer-execute-static-select-block-action (buffer))

(defun slack-execute-static-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-static-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-static-select-block-element) &optional _option)
  (with-slots (initial-option placeholder) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-static-select-block-action)
                          map))))

(cl-defmethod slack-block-action-payload ((this slack-static-select-block-element))
  (with-slots (type action-id block-id placeholder) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id)
          (cons "placeholder" (slack-block-action-payload placeholder)))))

(cl-defmethod slack-block-select-option ((this slack-static-select-block-element))
  (with-slots (options option-groups) this
    (if options
        (slack-block-select-from-options this options)
      (slack-block-select-from-option-groups this option-groups))))

(defclass slack-external-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "external_select")
   (initial-option :initarg :initial_option :initform nil :type (or null
                                                                    slack-option-message-composition-object
                                                                    slack-option-group-message-composition-object))
   (min-query-length :initarg :min_query_length :type (or integer null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(cl-defgeneric slack-buffer-execute-external-select-block-action (buffer))

(defun slack-execute-external-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-external-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-external-select-block-element))
  (with-slots (placeholder initial-option) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-external-select-block-action)
                          map))))

(defun slack-create-external-select-block-element (payload block-id)
  (make-instance 'slack-external-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_option (slack-create-option-message-composition-object
                                  (plist-get payload :initial_option))
                 :min_query_length (plist-get payload :min_query_length)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defmethod slack-block-action-payload ((this slack-external-select-block-element))
  (with-slots (action-id block-id type) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defconst slack-block-suggestions-url "https://slack.com/api/blocks.suggestions")

(cl-defmethod slack-block-fetch-suggestions ((this slack-external-select-block-element) service-id container team on-success)
  (with-slots (action-id block-id min-query-length) this
    (let* ((query (read-from-minibuffer
                   (format "Query (minimum length: %s): " min-query-length)))
           (data (json-encode-alist (list (cons "value" query)
                                          (cons "action_id" action-id)
                                          (cons "block_id" block-id)
                                          (cons "service_id" service-id)
                                          (cons "container" container)))))
      (cl-labels
          ((success (&key data &allow-other-keys)
                    (slack-request-handle-error
                     (data "slack-block-fetch-suggestions")
                     (let ((options (mapcar #'slack-create-option-message-composition-object
                                            (plist-get data :options)))
                           (option-groups (mapcar #'slack-create-option-group-message-composition-object
                                                  (plist-get data :option_groups))))
                       (run-with-timer 1 nil on-success options option-groups)))))
        (slack-request
         (slack-request-create
          slack-block-suggestions-url
          team
          :type "POST"
          :data data
          :headers (list (cons "Content-Type"
                               "application/json;charset=utf-8"))
          :success #'success))))))

(defclass slack-user-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "users_select")
   (initial-user :initarg :initial_user :type (or string null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-user-select-block-element (payload block-id)
  (make-instance 'slack-user-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_user (plist-get payload :initial_user)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-user-select-block-action (buffer))

(defun slack-execute-user-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-user-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-user-select-block-element) &optional _option)
  (with-slots (initial-user placeholder) this
    (let ((props (list
                  'face 'slack-select-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-user-select-block-action)
                            map))))
      (if initial-user
          (apply #'propertize (format "USER: %s" initial-user)
                 (append (list 'slack-user-id initial-user
                               'slack-lazy-user-name t)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-action-payload ((this slack-user-select-block-element))
  (with-slots (type action-id block-id) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defclass slack-conversation-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "conversations_select")
   (initial-conversation :initarg :initial_conversation :type (or string null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-conversation-select-block-element (payload block-id)
  (make-instance 'slack-conversation-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_conversation (plist-get payload :initial_conversation)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-conversation-select-block-action (buffer))

(defun slack-execute-conversation-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-conversation-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-conversation-select-block-element) &optional _option)
  (with-slots (initial-conversation placeholder) this
    (let ((props (list 'face 'slack-select-block-element-face
                       'slack-action-payload (slack-block-action-payload this)
                       'keymap (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "RET") #'slack-execute-conversation-select-block-action)
                                 map))))
      (if initial-conversation
          (apply #'propertize (format "CONVERSATION: %s" initial-conversation)
                 (append (list 'slack-conversation-id initial-conversation
                               'slack-lazy-conversation-name t)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-action-payload ((this slack-conversation-select-block-element))
  (with-slots (type action-id block-id) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

;; only public channel
(defclass slack-channel-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "channels_select")
   (initial-channel :initarg :initial_channel :type (or string null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-channel-select-block-element (payload block-id)
  (make-instance 'slack-channel-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_channel (plist-get payload :initial_channel)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-channel-select-block-action (buffer))

(defun slack-execute-channel-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-channel-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-channel-select-block-element) &optional _option)
  (with-slots (placeholder initial-channel) this
    (let ((props (list
                  'face 'slack-select-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-channel-select-block-action)
                            map))))
      (if initial-channel
          (apply #'propertize (format "CHANNEL: %s" initial-channel)
                 (append (list 'slack-lazy-conversation-name t
                               'slack-conversation-id initial-channel)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-action-payload ((this slack-channel-select-block-element))
  (with-slots (block-id action-id type) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defclass slack-overflow-menu-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "overflow")
   (action-id :initarg :action_id :type string)
   (block-id :initarg :block_id :type (or null string) :initform nil)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-overflow-block-element (payload block-id)
  (make-instance 'slack-overflow-menu-block-element
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :options (mapcar #'slack-create-option-message-composition-object
                                  (plist-get payload :options))
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(defface slack-overflow-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to overflow block element"
  :group 'slack)

(cl-defgeneric slack-buffer-execute-overflow-menu-block-action (buffer))

(defun slack-execute-overflow-menu-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-overflow-menu-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-overflow-menu-block-element) &optional _option)
  (propertize " … "
              'face 'slack-overflow-block-element-face
              'slack-action-payload (slack-block-action-payload this)
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") #'slack-execute-overflow-menu-block-action)
                        map)))

(cl-defmethod slack-block-action-payload ((this slack-overflow-menu-block-element))
  (with-slots (type action-id block-id) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defclass slack-date-picker-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "datepicker")
   (action-id :initarg :action_id :type string)
   (block-id :initarg :block_id :type (or null string) :initform nil)
   (placeholder :initarg :placeholder :initform nil :type (or null slack-text-message-composition-object))
   (initial-date :initarg :initial_date :type (or null string) :initform nil) ;; "YYYY-MM-DD"
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-datepicker-block-element (payload block-id)
  (make-instance 'slack-date-picker-block-element
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :initial_date (plist-get payload :initial_date)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-datepicker-block-action (buffer))

(defun slack-execute-datepicker-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-datepicker-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-date-picker-block-element) &optional _option)
  (with-slots (placeholder initial-date) this
    (let ((text (or initial-date
                    (slack-block-to-string placeholder)
                    "Pick a date")))
      (propertize text
                  'face 'slack-date-picker-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-datepicker-block-action)
                            map)))))

(defface slack-date-picker-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to date picker block element"
  :group 'slack)

(cl-defmethod slack-block-action-payload ((this slack-date-picker-block-element))
  (with-slots (type action-id block-id initial-date) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

;; Message Composition Objects
;; [Reference: Message composition objects | Slack](https://api.slack.com/reference/messaging/composition-objects)
(defclass slack-message-composition-object () ())

(cl-defmethod slack-block-action-id ((_this slack-message-composition-object))
  "")

(cl-defmethod slack-block-to-string ((this slack-message-composition-object) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (eieio-object-class-name this)))

(defclass slack-text-message-composition-object (slack-message-composition-object)
  ((type :initarg :type :type string) ;; plain_text or mrkdwn
   (text :initarg :text :type string)
   (emoji :initarg :emoji :type (or null boolean) :initform nil)
   (verbatim :initarg :verbatim :type (or null boolean) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-text-message-composition-object) &optional _option)
  (with-slots (text type) this
    (propertize text 'slack-text-type (cond ((string= "plain_text" type) 'plain)
                                            ((string= "mrkdwn" type) 'mrkdwn)
                                            (t nil)))))

(cl-defmethod slack-block-action-payload ((this slack-text-message-composition-object))
  (with-slots (type text emoji verbatim) this
    (list (cons "type" type)
          (cons "text" text)
          (cons "emoji" (or emoji :json-false)))))

(defun slack-create-text-message-composition-object (payload)
  (when payload
    (make-instance 'slack-text-message-composition-object
                   :type (plist-get payload :type)
                   :text (plist-get payload :text)
                   :emoji (eq t (plist-get payload :emoji))
                   :verbatim (eq t (plist-get payload :verbatim)))))

(defclass slack-confirmation-dialog-message-composition-object (slack-message-composition-object)
  ((title :initarg :title :type slack-text-message-composition-object)
   (text :initarg :text :type slack-text-message-composition-object)
   (confirm :initarg :confirm :type slack-text-message-composition-object)
   (deny :initarg :deny :type slack-text-message-composition-object)))

(cl-defmethod slack-block-handle-confirm ((this slack-confirmation-dialog-message-composition-object))
  (with-slots (title text) this
    (yes-or-no-p (format "%s\n%s"
                         (slack-block-to-string title)
                         (slack-block-to-string text)))))

(defun slack-create-confirmation-dialog-message-composition-object (payload)
  (when payload
    (ignore-errors
      (make-instance 'slack-confirmation-dialog-message-composition-object
                     :title (slack-create-text-message-composition-object
                             (plist-get payload :title))
                     :text (slack-create-text-message-composition-object
                            (plist-get payload :text))
                     :confirm (slack-create-text-message-composition-object
                               (plist-get payload :confirm))
                     :deny (slack-create-text-message-composition-object
                            (plist-get payload :deny))))))

(defclass slack-option-message-composition-object (slack-message-composition-object)
  ((text :initarg :text :type slack-text-message-composition-object)
   (value :initarg :value :type string)))

(defun slack-create-option-message-composition-object (payload)
  (when payload
    (make-instance 'slack-option-message-composition-object
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :value (or (plist-get payload :value) ""))))

(cl-defmethod slack-block-to-string ((this slack-option-message-composition-object))
  (with-slots (text) this
    (slack-block-to-string text)))

(defclass slack-option-group-message-composition-object (slack-message-composition-object)
  ((label :initarg :label :type slack-text-message-composition-object)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   ))

(cl-defmethod slack-block-select-from-option-group ((this slack-option-group-message-composition-object))
  (slack-if-let* ((options-alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                                         (oref this options))))
      (slack-select-from-list (options-alist (format "Select Option (%s): " (slack-block-to-string this))))))

(cl-defmethod slack-block-to-string ((this slack-option-group-message-composition-object))
  (with-slots (label) this
    (slack-block-to-string label)))

(defun slack-create-option-group-message-composition-object (payload)
  (make-instance 'slack-option-group-message-composition-object
                 :label (slack-create-text-message-composition-object
                         (plist-get payload :label))
                 :options (mapcar #'slack-create-option-message-composition-object
                                  (plist-get payload :options))))

(cl-defmethod slack-block-to-string ((_this null) &optional _option)
  nil)

;; POST
(defconst slack-block-actions-url "https://slack.com/api/blocks.actions")
(defun slack-block-action-execute (service-id actions container team)
  (let ((data (json-encode-alist
               (list (cons "actions" actions)
                     (cons "service_id" service-id)
                     (cons "container" container)
                     (cons "client_token" (slack-team-client-token team))))))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (message "DATA: %S" data)))
      (slack-request
       (slack-request-create
        slack-block-actions-url
        team
        :type "POST"
        :data data
        :headers (list (cons "Content-Type"
                             "application/json;charset=utf-8"))
        :success #'success)))))

;;; Utils

(defun slack-block-fontify-text-natively (text)
  (let* ((map '((cpp c++-mode)
                (clojure lisp-mode)
                (csharp java-mode)
                (emacslisp emacs-lisp-mode)
                (matlab octave-mode)
                (objc c-mode)
                (shell shell-script-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string text)))
    (let* ((lang (symbol-name language))
           ;; (ts (intern (concat lang "-ts-mode")))
           (normal (intern (concat lang "-mode")))
           (other (cadr (assoc language map)))
           (mode (cond
                  ;; ((fboundp ts) ts)
                  ((fboundp normal) normal)
                  ((fboundp other) other)
                  (t 'prog-mode))))
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure)
        (cons (or lang "src") (buffer-string))))))

(provide 'slack-block)
;;; slack-block.el ends here
