;;; slack-attachment.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-request)
(require 'slack-selectable)

(defclass slack-attachment ()
  ((fallback :initarg :fallback :initform nil)
   (title :initarg :title :initform nil)
   (title-link :initarg :title_link :initform nil)
   (pretext :initarg :pretext :initform nil)
   (text :initarg :text :initform nil)
   (author-name :initarg :author_name :initform nil)
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :initform '())
   (image-url :initarg :image_url :initform nil)
   (image-width :initarg :image_width :initform nil)
   (image-height :initarg :image_height :initform nil)
   (thumb-url :initarg :thumb_url)
   (is-share :initarg :is_share :initform nil)
   (footer :initarg :footer :initform nil)
   (color :initarg :color :initform nil)
   (ts :initarg :ts :initform nil)
   (author-subname :initarg :author_subname :initform nil)
   (callback-id :initarg :callback_id :initform nil)
   (id :initarg :id :initform nil)
   (actions :initarg :actions :initform '())))

(defclass slack-shared-message (slack-attachment)
  ((channel-id :initarg :channel_id :initform nil)
   (channel-name :initarg :channel_name :initform nil)
   (from-url :initarg :from_url :initform nil)))

(defclass slack-attachment-field ()
  ((title :initarg :title :initform nil)
   (value :initarg :value :initform nil)
   (short :initarg :short :initform nil)))

(defclass slack-attachment-action-confirmation ()
  ((title :initarg :title :initform nil)
   (text :initarg :text :type string)
   (ok-text :initarg :ok_text :type string :initform "Okay")
   (dismiss-text :initarg :dismiss_text :type string :initform "Cancel")))

(defclass slack-attachment-action ()
  ((id :initarg :id :type string)
   (name :initarg :name :type string)
   (text :initarg :text :type string)
   (type :initarg :type :type string)
   (value :initarg :value :initform nil)
   (confirm :initarg :confirm :initform nil
            :type (or null slack-attachment-action-confirmation))
   (style :initarg :style :type string :initform "default")
   (url :initarg :url :type (or null string) :initform nil)))

(defclass slack-attachment-select-action (slack-attachment-action slack-selectable)
  ((min-query-length :initarg :min_query_length :type (or null number) :initform nil)))

(defclass slack-attachment-select-action-option (slack-selectable-option) ())

(defclass slack-attachment-select-action-option-group
  (slack-selectable-option-group) ())



(defun slack-attachment-action-create (payload)
  (cl-labels
      ((create-option (option)
                      (apply #'make-instance
                             'slack-attachment-select-action-option
                             (slack-collect-slots
                              'slack-attachment-select-action-option
                              option)))
       (create-option-group
        (option-group)
        (when (plist-get option-group :options)
          (setq option-group
                (plist-put option-group
                           :options
                           (mapcar #'create-option
                                   (plist-get option-group :options)))))
        (apply #'make-instance
               'slack-attachment-select-action-option-group
               (slack-collect-slots
                'slack-attachment-select-action-option-group
                option-group))))
    (let* ((properties payload)
           (type (plist-get payload :type)))

      (when (plist-get payload :confirm)
        (setq properties (plist-put properties
                                    :confirm
                                    (apply #'make-instance
                                           'slack-attachment-action-confirmation
                                           (slack-collect-slots
                                            'slack-attachment-action-confirmation
                                            (plist-get payload :confirm))))))
      (cond
       ((string= type "select")
        (progn
          (setq properties
                (plist-put properties
                           :options
                           (mapcar #'create-option
                                   (plist-get properties :options))))
          (setq properties
                (plist-put properties
                           :option_groups
                           (mapcar #'create-option-group
                                   (plist-get properties :option_groups))))
          (setq properties
                (plist-put properties
                           :selected_options
                           (mapcar #'create-option
                                   (plist-get properties :selected_options))))
          (apply #'make-instance 'slack-attachment-select-action
                 (slack-collect-slots 'slack-attachment-select-action properties))))
       (t
        (apply #'make-instance 'slack-attachment-action
               (slack-collect-slots 'slack-attachment-action properties)))))))

(defun slack-attachment-create (payload)
  (let ((properties payload))
    (setq payload
          (plist-put payload :fields
                     (mapcar #'(lambda (field)
                                 (apply #'slack-attachment-field
                                        (slack-collect-slots 'slack-attachment-field
                                                             field)))
                             (append (plist-get payload :fields) nil))))
    (setq payload
          (plist-put payload :actions
                     (mapcar #'slack-attachment-action-create
                             (plist-get payload :actions))))

    (when (numberp (plist-get payload :ts))
      (setq payload
            (plist-put payload :ts (number-to-string (plist-get payload :ts)))))

    ;; (message "PAYLOAD: %s" payload)

    (if (plist-get payload :is_share)
        (apply #'slack-shared-message "shared-attachment"
               (slack-collect-slots 'slack-shared-message payload))
      (apply #'slack-attachment "attachment"
             (slack-collect-slots 'slack-attachment payload)))))

(defmethod slack-image-spec ((this slack-attachment))
  (with-slots (image-url image-height image-width) this
    (when image-url
      (list image-url image-width image-height slack-image-max-height))))

(defface slack-message-action-primary-face
  '((t (:box (:line-width 1 :style released-button)
             :foreground "#2aa198")))
  "Face used to primary action."
  :group 'slack)

(defface slack-message-action-danger-face
  '((t (:box (:line-width 1 :style released-button)
             :foreground "#FF6E64")))
  "Face used to danger action."
  :group 'slack)

(defvar slack-attachment-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-attachment-action-run)
    (define-key keymap [mouse-1] #'slack-attachment-action-run)
    keymap))

(defmethod slack-attachment-action-run-payload ((this slack-attachment-action)
                                                team
                                                common-payload
                                                service-id)
  (with-slots (id name text type value style) this
    (cons (cons "actions" (list (list (cons "id" id)
                                      (cons "name" name)
                                      (cons "text" text)
                                      (cons "type" type)
                                      (cons "value" value)
                                      (cons "style" style))))
          common-payload)))

(defmethod slack-attachment-action-get-suggestions ((this
                                                     slack-attachment-select-action)
                                                    team
                                                    common-payload
                                                    service-id
                                                    after-success)
  (with-slots (name) this
    (let ((url "https://slack.com/api/chat.attachmentSuggestion")
          (params (list (cons "service_id" service-id)
                        (cons "payload"
                              (json-encode-alist
                               (cons
                                (cons "name" name)
                                (cons (cons "value"
                                            (read-from-minibuffer
                                             (format "Start typing to see results... (minimum: %s) "
                                                     (oref this min-query-length))))
                                      common-payload)))))))

      (cl-labels
          ((log-error (err)
                      (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                         err
                                         url
                                         params)
                                 team :level 'error))
           (on-success (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-attachment-action-get-suggestions"
                              #'log-error))
                       (funcall after-success (plist-get data :options))))
        (slack-request
         (slack-request-create
          url
          team
          :type "POST"
          :success #'on-success
          :params params
          :sync t))))))

(defmethod slack-attachment-action-selected-options ((this
                                                      slack-attachment-select-action)
                                                     team
                                                     common-payload
                                                     service-id)
  (with-slots (data-source) this
    (cond
     ((string= data-source "external")
      (let ((option))
        (cl-labels
            ((on-success (options)
                         (let ((selected
                                (funcall slack-completing-read-function
                                         ""
                                         (cons "" (mapcar #'(lambda (e)
                                                              (plist-get e :text))
                                                          options))
                                         nil t)))

                           (setq option
                                 (cl-find-if #'(lambda (e)
                                                 (string= selected
                                                          (plist-get e :text)))
                                             options)))))
          (slack-attachment-action-get-suggestions this
                                                   team
                                                   common-payload
                                                   service-id
                                                   #'on-success)
          (if option
              (list (list (cons "value" (plist-get option :value))))
            (slack-attachment-action-selected-options
             this team common-payload service-id)))))
     ((string= data-source "conversations")
      (let ((room-id (oref (slack-room-select (append (oref team channels)
                                                      (oref team groups)
                                                      (oref team ims))
                                              team)
                           id)))
        (list (list (cons "value" room-id)))))
     ((string= data-source "channels")
      (let ((channel-id (oref (slack-room-select (oref team channels)
                                                 team) id)))
        (list (list (cons "value" channel-id)))))
     ((string= data-source "users")
      (let ((user-id (plist-get (slack--user-select team) :id)))
        (list (list (cons "value" user-id)))))
     ((string= data-source "static")
      (slack-if-let*
          ((option (slack-selectable-select-from-static-data-source this))
           (selected-options (list (list (cons "value"
                                               (oref option value))))))
          selected-options
        (error "Option is not selected")))
     (t (error "%s's data-source: %s is not implemented"
               (oref this name)
               (oref this data-source))))))

(defmethod slack-attachment-action-run-payload ((this slack-attachment-select-action)
                                                team
                                                common-payload
                                                service-id)
  (with-slots (id name text type value style data-source min-query-length) this
    (slack-if-let*
        ((selected-options (slack-attachment-action-selected-options this
                                                                     team
                                                                     common-payload
                                                                     service-id)))
        (cons (cons "actions"
                    (list (list (cons "id" id)
                                (cons "name" name)
                                (cons "text" text)
                                (cons "type" type)
                                (cons "style" style)
                                (cons "data_source" data-source)
                                (cons "min_query_length" min-query-length)
                                (cons "selected_options" selected-options))))
              common-payload)
      (error "Option is not selected"))))

(defmethod slack-attachment-action-confirm ((this slack-attachment-action))
  (with-slots (confirm) this
    (if confirm
        (with-slots (title text ok-text dismiss-text) confirm
          (yes-or-no-p (format "%s%s"
                               (if title
                                   (format "%s\n" title)
                                 "")
                               text)))
      t)))

(defun slack-attachment-action-run ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (oref buffer room))
                  (team (oref buffer team))
                  (type (get-text-property (point) 'type))
                  (attachment-id (get-text-property (point) 'attachment-id))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property (point) 'action)))
      (when (slack-attachment-action-confirm action)
        (slack-if-let* ((callback-id (get-text-property (point) 'callback-id))
                        (common-payload (list
                                         (cons "attachment_id" (number-to-string
                                                                attachment-id))
                                         (cons "callback_id" callback-id)
                                         (cons "is_ephemeral" (oref message
                                                                    is-ephemeral))
                                         (cons "message_ts" ts)
                                         (cons "channel_id" (oref room id))))
                        (service-id (if (slack-bot-message-p message)
                                        (slack-message-bot-id message)
                                      "B01")))
            (let ((url "https://slack.com/api/chat.attachmentAction")
                  (params (list (cons "payload"
                                      (json-encode-alist
                                       (slack-attachment-action-run-payload
                                        action
                                        team
                                        common-payload
                                        service-id)))
                                (cons "service_id" service-id)
                                (cons "client_token"
                                      (slack-team-client-token team)))))
              (cl-labels
                  ((log-error (err)
                              (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                                 err
                                                 url
                                                 params)
                                         team
                                         :level 'error))
                   (on-success (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-attachment-action-run" #'log-error))))
                (slack-request
                 (slack-request-create
                  url
                  team
                  :type "POST"
                  :params params
                  :success #'on-success))))
          (slack-if-let* ((url (oref action url)))
              (browse-url url))))))

(defmethod slack-attachment-callback-id ((this slack-attachment))
  (oref this callback-id))

(defmethod slack-attachment-id ((this slack-attachment))
  (oref this id))

(defmethod slack-attachment-action-face ((this slack-attachment-action))
  (with-slots (style) this
    (or (and (string= "danger" style)
             'slack-message-action-danger-face)
        (and (string= "primary" style)
             'slack-message-action-primary-face)
        'slack-message-action-face)))

(defmethod slack-attachment-action-display-text ((this slack-attachment-action))
  (replace-regexp-in-string ":" " " (oref this text)))


(defmethod slack-attachment-action-display-text ((this slack-attachment-select-action))
  (let ((base (call-next-method)))
    (with-slots (selected-options) this
      (format "%s%s" base (if (and selected-options (car selected-options))
                              (format " (%s)"
                                      (slack-selectable-text (car selected-options)))
                            "")))))


(defmethod slack-attachment-action-to-string ((action slack-attachment-select-action)
                                              attachment team)
  (with-slots (id name text type data-source style options option-groups) action
    (let* ((callback-id (slack-attachment-callback-id attachment))
           (attachment-id (slack-attachment-id attachment))
           (face (slack-attachment-action-face action)))
      (propertize (slack-attachment-action-display-text action)
                  'type type
                  'face face
                  'attachment-id attachment-id
                  'callback-id callback-id
                  'action action
                  'keymap slack-attachment-action-keymap))))

(defmethod slack-attachment-action-to-string ((action slack-attachment-action)
                                              attachment team)
  (with-slots (id name text type value style) action
    (let* ((callback-id (slack-attachment-callback-id attachment))
           (attachment-id (slack-attachment-id attachment))
           (face (slack-attachment-action-face action)))
      (propertize (slack-attachment-action-display-text action)
                  'type type
                  'face face
                  'keymap slack-attachment-action-keymap
                  'attachment-id attachment-id
                  'callback-id callback-id
                  'action action))))

(defmethod slack-message-to-string ((attachment slack-attachment) team)
  (with-slots
      (fallback text ts color from-url footer fields pretext actions) attachment
    (let* ((pad-raw (propertize "|" 'face 'slack-attachment-pad))
           (pad (or (and color (propertize pad-raw 'face (list :foreground (concat "#" color))))
                    pad-raw))
           (header-raw (slack-attachment-header attachment))
           (header (and (not (slack-string-blankp header-raw))
                        (format "%s\t%s" pad
                                (propertize header-raw
                                            'face 'slack-attachment-header))))
           (pretext (and pretext (format "%s\t%s" pad pretext)))
           (body (and text (format "%s\t%s" pad (mapconcat #'identity
                                                           (split-string text "\n")
                                                           (format "\n\t%s\t" pad)))))
           (fields (if fields (mapconcat #'(lambda (field)
                                             (slack-attachment-field-to-string field
                                                                               (format "\t%s" pad)))
                                         fields
                                         (format "\n\t%s\n" pad))))
           (actions (if actions
                        (format "%s\t%s"
                                pad
                                (mapconcat #'(lambda (action)
                                               (slack-attachment-action-to-string
                                                action
                                                attachment
                                                team))
                                           actions
                                           " "))))
           (footer (if footer
                       (format "%s\t%s"
                               pad
                               (propertize
                                (format "%s%s" footer
                                        (or (and ts (format "|%s" (slack-message-time-to-string ts)))
                                            ""))
                                'face 'slack-attachment-footer))))
           (image (slack-image-string (slack-image-spec attachment)
                                      (format "\t%s\t" pad))))
      (slack-message-unescape-string
       (slack-format-message
        (or (and header (format "\t%s\n" header)) "")
        (or (and pretext (format "\t%s\n" pretext)) "")
        (or (and body (format "\t%s" body)) "")
        (or (and fields fields) "")
        (or (and actions (format "\t%s" actions)) "")
        (or (and footer (format "\n\t%s" footer)) "")
        (or (and image (< 0 (length image))
                 (format "\n\t%s\t%s" pad image)) ""))
       team))))

(defmethod slack-attachment-header ((attachment slack-attachment))
  (with-slots (title title-link author-name author-subname) attachment
    (concat (or (and title title-link (slack-linkfy title title-link))
                title
                "")
            (or author-name author-subname ""))))

(defmethod slack-attachment-field-to-string ((field slack-attachment-field) &optional pad)
  (unless pad (setq pad ""))
  (let ((title (propertize (or (oref field title) "") 'face 'slack-attachment-field-title))
        (value (mapconcat #'(lambda (e) (format "\t%s" e))
                          (split-string (or (oref field value) "") "\n")
                          (format "\n%s\t" pad))))
    (format "%s\t%s\n%s\t%s" pad title pad value)))

(defmethod slack-attachment-to-alert ((a slack-attachment))
  (with-slots (title fallback pretext) a
    (if (and title (< 0 (length title)))
        title
      (if (and pretext (< 0 (length pretext)))
          (format "%s\n%s" pretext fallback)
        fallback))))

(defmethod slack-selectable-prompt ((this slack-attachment-select-action))
  (format "%s :" (oref this text)))

(provide 'slack-attachment)
;;; slack-attachment.el ends here
