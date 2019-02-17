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
;; (require 'slack-message-formatter)
(declare-function slack-format-message "slack-message-formatter")
(require 'slack-image)

;; Layout Blocks
;; [Reference: Message layout blocks | Slack](https://api.slack.com/reference/messaging/blocks)
(defclass slack-layout-block () ((type :initarg :type :type string)))
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
   (text :initarg :text :type slack-text-message-composition-object)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (fields :initarg :fields :type (or list null) :initform nil) ;; list of slack-text-message-composition-object
   (accessory :initarg :accessory :initform nil :type (or null slack-block-element))))

(defun slack-create-section-layout-block (payload)
  (let ((accessory (slack-create-block-element
                    (plist-get payload :accessory))))
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

(defclass slack-divider-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "divider")
   (block-id :initarg :block_id :type (or null string) :initform nil)))

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
   (block-id :initarg :block_id :type (or string null) :initform nil)
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
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(defun slack-create-actions-layout-block (payload)
  (make-instance 'slack-actions-layout-block
                 :elements (mapcar #'slack-create-block-element
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-actions-layout-block) &optional _option)
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'slack-block-to-string
                       elements)
               " ")))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(defun slack-create-context-layout-block (payload)
  (make-instance 'slack-context-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (or
                                        (slack-create-block-element e)
                                        (slack-create-text-message-composition-object  e)))

                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-context-layout-block) &optional _option)
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'(lambda (e) (slack-block-to-string e '(:max-image-height 30 :max-image-width 30)))
                       elements)
               " ")))

;; Block Elements
;; [Reference: Block elements | Slack](https://api.slack.com/reference/messaging/block-elements)
(defclass slack-block-element () ((type :initarg :type :type string)))
(cl-defmethod slack-block-to-string ((this slack-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (eieio-object-class-name this)))

(defun slack-create-block-element (payload)
  (let ((type (plist-get payload :type)))
    (cond
     ((string= "image" type)
      (slack-create-image-block-element payload))
     ((string= "button" type)
      (slack-create-button-block-element payload))
     ((string= "static_select" type)
      (slack-create-static-select-block-element payload))
     ((string= "external_select" type)
      (slack-create-external-select-block-element payload))
     ((string= "users_select" type)
      (slack-create-user-select-block-element payload))
     ((string= "conversations_select" type)
      (slack-create-conversation-select-block-element payload))
     ((string= "channels_select" type)
      (slack-create-channel-select-block-element payload))
     ((string= "overflow" type)
      (slack-create-overflow-block-element payload))
     ((string= "datepicker" type)
      (slack-create-datepicker-block-element payload))
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
   (url :initarg :url :type (or string null) :initform nil)
   (value :initarg :value :type (or string null) :initform nil)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-composition-object))))

(defun slack-create-button-block-element (payload)
  (make-instance 'slack-button-block-element
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))
                 :action_id (plist-get payload :action_id)
                 :url (plist-get payload :url)
                 :value (plist-get payload :value)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(defface slack-button-block-element-face
  '((t (:box (:line-width 1 :style released-button :forground "#2aa198"))))
  "Used to button block element"
  :group 'slack)

(cl-defmethod slack-block-to-string ((this slack-button-block-element) &optional _option)
  (with-slots (text) this
    (propertize (slack-block-to-string text)
                'face 'slack-button-block-element-face)))

(defclass slack-select-block-element (slack-block-element)
  ((placeholder :initarg :placeholder :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(cl-defmethod slack-block-to-string ((this slack-select-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this type)))

(defface slack-select-block-element-face
  '((t (:box (:line-width 1 :style released-button :forground "#2aa198"))))
  "Used to select block element"
  :group 'slack)

(defclass slack-static-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "static_select")
   (options :initarg :options :type (or null list) :initform nil) ;; list of slack-option-message-composition-object
   (option-groups :initarg :option_groups :type (or list null) :initform nil) ;; list of slack-option-groups-composition-object
   (initial-option :initarg :initial_option :initform nil (or null
                                                              slack-option-message-composition-object))))

(defun slack-create-static-select-block-element (payload)
  (let* ((options (plist-get payload :options))
         (option-groups (plist-get payload :option_groups))
         (initial-option (plist-get payload :initial_option)))
    (make-instance 'slack-static-select-block-element
                   :placeholder (slack-create-text-message-composition-object
                                 (plist-get payload :placeholder))
                   :action_id (plist-get payload :action_id)
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
                'face 'slack-select-block-element-face)))

(defclass slack-external-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "external_select")
   (initial-option :initarg :initial_option :initform nil :type (or null
                                                                    slack-option-message-composition-object
                                                                    slack-option-group-message-composition-object))
   (min-query-length :initarg :min_query_length :type (or integer null) :initform nil)))

(defun slack-create-external-select-block-element (payload)
  (make-instance 'slack-external-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :initial_option (slack-create-option-message-composition-object
                                  (plist-get payload :initial_option))
                 :min_query_length (plist-get payload :min_query_length)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defmethod slack-block-to-string ((this slack-external-select-block-element))
  (with-slots (placeholder initial-option) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face)))

(defclass slack-user-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "users_select")
   (initial-user :initarg :initial_user :type (or string null) :initform nil)))

(defun slack-create-user-select-block-element (payload)
  (make-instance 'slack-user-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :initial_user (plist-get payload :initial_user)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defmethod slack-block-to-string ((this slack-user-select-block-element) &optional _option)
  (with-slots (initial-user placeholder) this
    (if initial-user
        (propertize (format "USER: %s" initial-user)
                    'face 'slack-select-block-element-face
                    'slack-user-id initial-user
                    'slack-lazy-user-name t)
      (propertize (slack-block-to-string placeholder)
                  'face 'slack-select-block-element-face))))

(defclass slack-conversation-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "conversations_select")
   (initial-conversation :initarg :initial_conversation :type (or string null) :initform nil)))

(defun slack-create-conversation-select-block-element (payload)
  (make-instance 'slack-conversation-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :initial_conversation (plist-get payload :initial_conversation)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defmethod slack-block-to-string ((this slack-conversation-select-block-element) &optional _option)
  (with-slots (initial-conversation placeholder) this
    (if initial-conversation
        (propertize (format "CONVERSATION: %s"
                            initial-conversation)
                    'face 'slack-select-block-element-face
                    'slack-conversation-id initial-conversation
                    'slack-lazy-conversation-name t)
      (propertize (slack-block-to-string placeholder)
                  'face 'slack-select-block-element-face))))

;; only public channel
(defclass slack-channel-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "channels_select")
   (initial-channel :initarg :initial_channel :type (or string null) :initform nil)))

(defun slack-create-channel-select-block-element (payload)
  (make-instance 'slack-channel-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :initial_channel (plist-get payload :initial_channel)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defmethod slack-block-to-string ((this slack-channel-select-block-element) &optional _option)
  (with-slots (placeholder initial-channel) this
    (if initial-channel
        (propertize (format "CHANNEL: %s" initial-channel)
                    'face 'slack-select-block-element-face
                    'slack-lazy-conversation-name t
                    'slack-conversation-id initial-channel)
      (propertize (slack-block-to-string placeholder)
                  'face 'slack-select-block-element-face))))

(defclass slack-overflow-menu-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "overflow")
   (action-id :initarg :action_id :type string)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-overflow-block-element (payload)
  (make-instance 'slack-overflow-menu-block-element
                 :action_id (plist-get payload :action_id)
                 :options (mapcar #'slack-create-option-message-composition-object
                                  (plist-get payload :options))
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(defface slack-overflow-block-element-face
  '((t (:box (:line-width 1 :style released-button :forground "#2aa198"))))
  "Used to overflow block element"
  :group 'slack)

(cl-defmethod slack-block-to-string ((_this slack-overflow-menu-block-element) &optional _option)
  (propertize " … " 'face 'slack-overflow-block-element-face))

(defclass slack-date-picker-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "datepicker")
   (action-id :initarg :action_id :type string)
   (placeholder :initarg :placeholder :initform nil :type (or null slack-text-message-composition-object))
   (initial-date :initarg :initial_date :type (or null string) :initform nil) ;; "YYYY-MM-DD"
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-datepicker-block-element (payload)
  (make-instance 'slack-date-picker-block-element
                 :action_id (plist-get payload :action_id)
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
      (propertize text 'face 'slack-date-picker-block-element-face))))

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
  (with-slots (text) this
    text))

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

(defun slack-create-option-group-message-composition-object (payload)
  (make-instance 'slack-option-group-message-composition-object
                 :label (slack-create-text-message-composition-object
                         (plist-get payload :label))
                 :options (mapcar #'slack-create-option-message-composition-object
                                  (plist-get payload :options))))

(cl-defmethod slack-block-to-string ((_this null) &optional _option)
  nil)

(provide 'slack-block)
;;; slack-block.el ends here
