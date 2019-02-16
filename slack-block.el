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

;; Layout Blocks
;; [Reference: Message layout blocks | Slack](https://api.slack.com/reference/messaging/blocks)
(defclass slack-layout-block () ((type :initarg :type :type string)))

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
  (make-instance 'slack-section-layout-block
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))
                 :block_id (plist-get payload :block_id)
                 :fields (mapcar #'slack-create-text-message-composition-object
                                 (plist-get payload :fields))
                 :accessory (slack-create-block-element
                             (plist-get payload :accessory))))

(defclass slack-divider-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "divider")
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-divider-layout-block (payload)
  (make-instance 'slack-divider-layout-block
                 :block_id (plist-get payload :block_id)))

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

(defclass slack-actions-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "actions")
   (elements :initarg :elements :type list) ;; max 5 elements
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(defun slack-create-actions-layout-block (payload)
  (make-instance 'slack-actions-layout-block
                 :elements (mapcar #'slack-create-block-element
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(defun slack-create-context-layout-block (payload)
  (make-instance 'slack-context-layout-block
                 :elements (mapcar #'slack-create-block-element
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

;; Block Elements
;; [Reference: Block elements | Slack](https://api.slack.com/reference/messaging/block-elements)
(defclass slack-block-element () ((type :initarg :type :type string)))

(defun slack-create-block-element (payload)
  (let ((type (plist-get payload :type)))
    (cond
     ((string= "image" type))
     ((string= "button" type))
     ((string= "static_select" type))
     ((string= "external_select" type))
     ((string= "users_select" type))
     ((string= "conversations_select" type))
     ((string= "channels_select" type))
     ((string= "overflow" type))
     ((string= "datepicker" type))
     )))

(defclass slack-image-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)))

(defclass slack-button-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "button")
   (text :initarg :text :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (url :initarg :url :type (or string null) :initform nil)
   (value :initarg :value :type (or string null) :initform nil)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-composition-object))))

(defclass slack-select-block-element (slack-block-element)
  ((placeholder :initarg :placeholder :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-composition-object))))

(defclass slack-static-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "static_select")
   (options :initarg :options :type (or null list) :initform nil) ;; list of slack-option-message-composition-object
   (option-groups :initarg :option_groups :type (or list null) :initform nil) ;; list of slack-option-groups-composition-object
   (initial-option :initarg :initial_option :initform nil (or null
                                                              slack-option-message-composition-object
                                                              slack-option-group-message-composition-object))))

(defclass slack-external-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "external_select")
   (initial-option :initarg :initial_option :initform nil :type (or null
                                                                    slack-option-message-composition-object
                                                                    slack-option-group-message-composition-object))
   (min-query-length :initarg :min_query_length :type (or integer null) :initform nil)))

(defclass slack-user-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "users_select")
   (initial-user :initarg :initial_user :type (or string null) :initform nil)))

(defclass slack-conversation-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "conversations_select")
   (initial-conversation :initarg :initial_conversation :type (or string null) :initform nil)))

;; only public channel
(defclass slack-channel-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "channels_select")
   (initial-channel :initarg :initial_channel :type (or string null) :initform nil)))

(defclass slack-overflow-menu-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "overflow")
   (action-id :initarg :action_id :type string)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defclass slack-date-picker-block-element ()
  ((type :initarg :type :type string :initform "datepicker")
   (action-id :initarg :action_id :type string)
   (placeholder :initarg :placeholder :initform nil :type (or null slack-text-message-composition-object))
   (initial-date :initarg :initial_date :type (or null string) :initform nil)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

;; Message Composition Objects
;; [Reference: Message composition objects | Slack](https://api.slack.com/reference/messaging/composition-objects)
(defclass slack-message-composition-object () ())
(defclass slack-text-message-composition-object (slack-message-composition-object)
  ((type :initarg :type :type string) ;; plain_text or mrkdwn
   (text :initarg :text) ;; :type string?
   (emoji :initarg :emoji :type (or null boolean) :initform nil)
   (verbatim :initarg :verbatim :type (or null boolean) :initform nil)))

(defun slack-create-text-message-composition-object (payload)
  (make-instance 'slack-text-message-composition-object
                 :type (plist-get payload :type)
                 :text (plist-get payload :text)
                 :emoji (plist-get payload :emoji)
                 :verbatim (eq t (plist-get payload :verbatim))))

(defclass slack-confirmation-dialog-message-composition-object (slack-message-composition-object)
  ((title :initarg :title :type slack-text-message-composition-object)
   (text :initarg :text :type slack-text-message-composition-object)
   (confirm :initarg :confirm :type slack-text-message-composition-object)
   (deny :initarg :confirm :type slack-text-message-composition-object)))

(defclass slack-option-message-composition-object (slack-message-composition-object)
  ((text :initarg :text :type slack-text-message-composition-object)
   (value :initarg :value :type string)))

(defclass slack-option-group-message-composition-object (slack-message-composition-object)
  ((label :initarg :label :type slack-text-message-composition-object)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   ))

(provide 'slack-block)
;;; slack-block.el ends here
