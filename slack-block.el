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
(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-request)
;; (require 'slack-buffer)
(declare-function slack-execute-button-block-action "slack-buffer")
(declare-function slack-execute-static-select-block-action "slack-buffer")
(declare-function slack-execute-external-select-block-action "slack-buffer")
(declare-function slack-execute-user-select-block-action "slack-buffer")
(declare-function slack-execute-conversation-select-block-action "slack-buffer")
(declare-function slack-execute-channel-select-block-action "slack-buffer")
(declare-function slack-execute-overflow-menu-block-action "slack-buffer")
(declare-function slack-execute-datepicker-block-action "slack-buffer")
;; (require 'slack-message-formatter)
(declare-function slack-format-message "slack-message-formatter")
(require 'slack-image)

(defvar slack-completing-read-function)

;; Layout Blocks
;; [Reference: Message layout blocks | Slack](https://api.slack.com/reference/messaging/blocks)
(defclass slack-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(cl-defmethod slack-block-find-action ((_this slack-layout-block) _action-id)
  nil)

(cl-defmethod slack-block-to-string ((this slack-layout-block) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (eieio-object-class-name this)))

(defun slack-create-layout-block (payload)
  (let ((type (plist-get payload :type)))
    (cond
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
     )))

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
    (when (and (slot-boundp accessory 'action-id)
               (string= action-id
                        (oref accessory action-id)))
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
    (cl-find-if #'(lambda (e) (and (slot-boundp e 'action-id)
                                   (string= action-id (oref e action-id))))
                elements)))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)))

(defun slack-create-context-layout-block (payload)
  (make-instance 'slack-context-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (or
                                        (slack-create-block-element
                                         e (plist-get payload :block_id))
                                        (slack-create-text-message-composition-object  e)))

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
    (cl-find-if #'(lambda (e) (and (slot-boundp e 'action-id)
                                   (string= action-id (oref e action-id))))
                elements)))

;; Block Elements
;; [Reference: Block elements | Slack](https://api.slack.com/reference/messaging/block-elements)
(defclass slack-block-element () ((type :initarg :type :type string)))
(cl-defmethod slack-block-to-string ((this slack-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (eieio-object-class-name this)))

(cl-defmethod slack-block-handle-confirm ((this slack-block-element))
  (if (slot-boundp this 'confirm)
      (with-slots (confirm) this
        (if (null confirm) t
          (with-slots (title text) confirm
            (yes-or-no-p (format "%s\n%s"
                                 (slack-block-to-string title)
                                 (slack-block-to-string text))))))
    t))

(cl-defmethod slack-block-select-from-options ((_this slack-block-element) options)
  (let ((alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                       options)))
    (slack-select-from-list (alist "Select Option: "))))

(defun slack-create-block-element (payload block-id)
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
     )))

(defclass slack-image-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (image-height :initarg :image_height :type number)
   (image-width :initarg :image_width :type number)
   (image-bytes :initarg :image_bytes :type number)))

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

(defface slack-button-block-element-face
  '((t (:box (:line-width 1 :style released-button :foreground "#2aa198"))))
  "Used to button block element"
  :group 'slack)

(defface slack-button-danger-block-element-face
  '((t (:inherit slack-button-block-element-face :foreground "#dc322f")))
  "Used to danger button block element"
  :group 'slack)

(defface slack-button-primary-block-element-face
  '((t (:inherit slack-button-block-element-face :foreground "#859900")))
  "Used to primary button block element"
  :group 'slack)

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

(cl-defmethod slack-block-action-payload ((this slack-button-block-element))
  (with-slots (block-id action-id value text) this
    (list (cons "block_id" (or block-id ""))
          (cons "action_id" action-id)
          (cons "value" value)
          (cons "type" "button")
          (cons "text" (slack-block-action-payload text)))))

(defclass slack-select-block-element (slack-block-element)
  ((placeholder :initarg :placeholder :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(cl-defmethod slack-block-to-string ((this slack-select-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this type)))

(cl-defmethod slack-block-select-from-option-groups ((_this slack-select-block-element) option-groups)
  (slack-if-let*
      ((group-alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                            option-groups))
       (group (slack-select-from-list (group-alist "Select Group: ")))
       (options-alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                              (oref group options))))
      (slack-select-from-list (options-alist (format "Select Option (%s): "
                                                     (slack-block-to-string group))))))

(defface slack-select-block-element-face
  '((t (:box (:line-width 1 :style released-button :forground "#2aa198"))))
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

(cl-defmethod slack-block-to-string ((this slack-external-select-block-element))
  (with-slots (placeholder initial-option) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-external-select-block-action)
                          map))))

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
  '((t (:box (:line-width 1 :style released-button :forground "#2aa198"))))
  "Used to overflow block element"
  :group 'slack)

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

(defface slack-date-picker-block-element-face
  '((t (:box (:line-width 1 :style released-button :forground "#2aa198"))))
  "Used to date picker block element"
  :group 'slack)

(cl-defmethod slack-block-to-string ((this slack-date-picker-block-element) &optional _option)
  (with-slots (placeholder initial-date) this
    (let ((text (or initial-date
                    (slack-block-to-string placeholder))))
      (propertize text
                  'face 'slack-date-picker-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-datepicker-block-action)
                            map)))))

(cl-defmethod slack-block-action-payload ((this slack-date-picker-block-element))
  (with-slots (type action-id block-id initial-date) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

;; Message Composition Objects
;; [Reference: Message composition objects | Slack](https://api.slack.com/reference/messaging/composition-objects)
(defclass slack-message-composition-object () ())
(cl-defmethod slack-block-to-string ((this slack-message-composition-object) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (eieio-object-class-name this)))

(defclass slack-text-message-composition-object (slack-message-composition-object)
  ((type :initarg :type :type string) ;; plain_text or mrkdwn
   (text :initarg :text) ;; :type string?
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

(defun slack-create-confirmation-dialog-message-composition-object (payload)
  (when payload
    (make-instance 'slack-confirmation-dialog-message-composition-object
                   :title (slack-create-text-message-composition-object
                           (plist-get payload :title))
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :confirm (slack-create-text-message-composition-object
                             (plist-get payload :confirm))
                   :deny (slack-create-text-message-composition-object
                          (plist-get payload :deny)))))

(defclass slack-option-message-composition-object (slack-message-composition-object)
  ((text :initarg :text :type slack-text-message-composition-object)
   (value :initarg :value :type string)))

(defun slack-create-option-message-composition-object (payload)
  (when payload
    (make-instance 'slack-option-message-composition-object
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :value (plist-get payload :value))))

(cl-defmethod slack-block-to-string ((this slack-option-message-composition-object))
  (with-slots (text) this
    (slack-block-to-string text)))

(defclass slack-option-group-message-composition-object (slack-message-composition-object)
  ((label :initarg :label :type slack-text-message-composition-object)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   ))

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

(provide 'slack-block)
;;; slack-block.el ends here
