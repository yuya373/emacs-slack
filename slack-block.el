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
(defclass slack-section-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "section")
   (text :initarg :text :type slack-text-message-composition-object)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (fields :initarg :fields :type (or list null) :initform nil) ;; list of slack-text-message-composition-object
   (accessory :initarg :accessory :initform nil :type (or null slack-block-element))))

(defclass slack-divider-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "divider")
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defclass slack-image-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (title :initarg :title :initform nil (or null slack-text-message-composition-object))
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(defclass slack-actions-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "actions")
   (elements :initarg :elements :type list) ;; max 5 elements
   (block-id :initarg :block_id :type (or string null) :initform nil)))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)
   (block-id :initarg :block_id :type (or string null) :initform nil)))

;; Block Elements
;; [Reference: Block elements | Slack](https://api.slack.com/reference/messaging/block-elements)
(defclass slack-block-element () ((type :initarg :type :type string)))
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
