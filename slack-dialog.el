;;; slack-dialog.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@yuya373>
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
(require 'slack-util)
(require 'slack-selectable)

(defclass slack-dialog ()
  ((title :initarg :title :type string)
   (callback-id :initarg :callback_id :type string)
   (elements :initarg :elements :type list)
   (state :initarg :state :type (or null string) :initform nil)
   (submit-label :initarg :submit_label :type string :initform "Submit")
   (notify-on-cancel :initarg :notify_on_cancel :type boolean :initform nil)
   (error-message :initarg :error-message :type (or null string) :initform nil)
   ))

(defclass slack-dialog-element ()
  ((name :initarg :name :type string)
   (label :initarg :label :type string)
   (type :initarg :type :type string)
   (optional :initarg :optional :type boolean :initform nil)
   (placeholder :initarg :placeholder :type (or null string) :initform nil)
   (value :initarg :value :type (or null string) :initform nil)
   (errors :initarg :errors :type list :initform '())))

(defclass slack-dialog-element-error ()
  ((name :initarg :name :type string)
   (error-message :initarg :error-message :type string)))

(defclass slack-dialog-text-element (slack-dialog-element)
  ((max-length :initarg :max_length :type number :initform 150)
   (min-length :initarg :min_length :type number :initform 0)
   (hint :initarg :hint :type (or null string) :initform nil)
   ;; one of email, number, tel or url
   (subtype :initarg :subtype :type (or null string) :initform nil)))

(defclass slack-dialog-textarea-element (slack-dialog-text-element)
  ((max-length :initarg :max_length :type number :initform 3000)))

(defclass slack-dialog-select-element (slack-dialog-element slack-selectable)
  ((min-query-length :initarg :min_query_length :type (or null number) :initform nil)))

(defclass slack-dialog-select-option (slack-selectable-option)
  ((label :initarg :label :type string)))

(defclass slack-dialog-select-option-group (slack-selectable-option-group)
  ((label :initarg :label :type string)))

(defmethod slack-dialog-element-value ((this slack-dialog-element))
  (or (oref this value) ""))

(defmethod slack-dialog-element-value ((this slack-dialog-select-element))
  (with-slots (data-source selected-options) this
    (or (cond
         ((string= data-source "external")
          (and selected-options
               (car selected-options)
               (oref (car selected-options) value)))
         (t (oref this value)))
        "")))

(defmethod slack-equalp ((this slack-dialog-element) other)
  (string= (oref this name)
           (oref other name)))

(defmethod slack-dialog-selected-option ((this slack-dialog-select-element))
  (with-slots (data-source value options selected-options) this
    (if (string= data-source "static")
        (cl-find-if #'(lambda (op) (string= (oref op value)
                                            value))
                    options)
      (and selected-options (car selected-options)))))

(defmethod slack-selectable-text ((this slack-dialog-select-option))
  (oref this label))

(defmethod slack-selectable-text ((this slack-dialog-select-option-group))
  (oref this label))

(defmethod slack-selectable-prompt ((this slack-dialog-select-element))
  (format "%s :"
          (oref this label)))

(defun slack-dialog-text-element-create (payload)
  (apply #'make-instance 'slack-dialog-text-element
         (slack-collect-slots 'slack-dialog-text-element payload)))

(defun slack-dialog-textarea-element-create (payload)
  (apply #'make-instance 'slack-dialog-textarea-element
         (slack-collect-slots 'slack-dialog-textarea-element payload)))

(defun slack-dialog-select-element-create (payload)
  (let ((options
         (mapcar #'(lambda (e)
                     (apply #'make-instance 'slack-dialog-select-option
                            (slack-collect-slots 'slack-dialog-select-option
                                                 e)))
                 (plist-get payload :options)))
        (option-groups
         (mapcar #'(lambda (e)
                     (apply #'make-instance
                            'slack-dialog-select-option-group
                            (slack-collect-slots 'slack-dialog-select-option-group
                                                 e)))
                 (plist-get payload :option_groups))))
    (setq payload (plist-put payload :options options))
    (setq payload (plist-put payload :option_groups option-groups))
    (apply #'make-instance 'slack-dialog-select-element
           (slack-collect-slots 'slack-dialog-select-element
                                payload))))

(defun slack-dialog-element-create (payload)
  (let ((type (plist-get payload :type)))
    (cond
     ((string= type "select")
      (slack-dialog-select-element-create payload))
     ((string= type "text")
      (slack-dialog-text-element-create payload))
     ((string= type "textarea")
      (slack-dialog-textarea-element-create payload))
     (t (error "Unknown dialog element type: %s" type)))))

(defun slack-dialog-create (payload)
  (let ((elements (mapcar #'slack-dialog-element-create
                          (plist-get payload :elements))))
    (setq payload (plist-put payload :elements elements))
    (apply #'make-instance 'slack-dialog
           (slack-collect-slots 'slack-dialog payload))))

(defmethod slack-dialog-element-validate ((this slack-dialog-element) value)
  (with-slots (optional label) this
    (when (and (not optional)
               (or (null value)
                   (< (length value) 1)))
      (error "%s must not be empty" label))))

(defmethod slack-dialog-element-validate ((_this slack-dialog-select-element) _value)
  (call-next-method))

(defmethod slack-dialog-element-validate ((this slack-dialog-text-element) value)
  (call-next-method)
  (with-slots (min-length max-length label) this
    (when (< max-length (length value))
      (error "%s must be less than %s" label max-length))
    (when (< (length value) min-length)
      (error "%s must be greater than %s" label min-length))))

(defmethod slack-dialog-execute ((this slack-dialog-text-element) _dialog-id team)
  (with-slots (hint value placeholder label optional) this
    (let* ((prompt (format "%s%s%s : "
                           label
                           (if hint (format " (%s)" hint) "")
                           (if optional " (optional)" "")))
           (value (read-from-minibuffer prompt value)))
      (slack-dialog-element-validate this value)
      value)))

(defmethod slack-dialog-execute ((this slack-dialog-textarea-element) _dialog-id team)
  (call-next-method))

(defmethod slack-dialog-select-element-get-suggestions ((this slack-dialog-select-element)
                                                        dialog-id team after-success)
  (let* ((url "https://slack.com/api/dialog.selectSuggestion")
         (min-query-length (oref this min-query-length))
         (prompt (format "Type hints to see options (minimum: %s) : " min-query-length))
         (params (list (cons "dialog_id" dialog-id)
                       (cons "name" (oref this name))
                       (cons "value" (read-from-minibuffer prompt)))))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-dialog-select-fetch-suggestion")
                      (funcall after-success data))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :sync t
        :success #'on-success)))))

(defmethod slack-dialog-execute ((this slack-dialog-select-element) dialog-id team)
  (slack-if-let* ((selected (slack-dialog--execute this dialog-id team)))
      (cdr selected)))

(defmethod slack-dialog--execute ((this slack-dialog-select-element) dialog-id team)
  (with-slots (data-source) this
    (cond
     ((string= data-source "external")
      (let ((result-option nil))
        (cl-labels
            ((log-error (err)
                        (slack-log (format "Error: %s" err) team :level 'error))
             (select (options)
                     (slack-if-let*
                         ((selected (completing-read "Choose an Option..."
                                                     (mapcar #'(lambda (option)
                                                                 (plist-get option :label))
                                                             options)
                                                     nil t)))
                         (cl-find-if #'(lambda (option) (string= selected
                                                                 (plist-get option :label)))
                                     options)))
             (after-success (data)
                            (slack-request-handle-error
                             (data "slack-dialog-execute"
                                   #'log-error)
                             (let ((option-groups (plist-get data :option_groups))
                                   (options (plist-get data :options)))
                               (when option-groups
                                 (slack-if-let* ((selected-option-group (select option-groups)))
                                     (setq options (plist-get selected-option-group :options))))
                               (when options
                                 (slack-if-let* ((selected (select options)))
                                     (setq result-option selected)))))))
          (slack-dialog-select-element-get-suggestions this
                                                       dialog-id
                                                       team
                                                       #'after-success)
          (if result-option
              (cons (plist-get result-option :label)
                    (plist-get result-option :value))))))
     ((string= data-source "conversations")
      (slack-if-let* ((rooms (append (oref team channels)
                                     (oref team groups)
                                     (oref team ims)))
                      (room (slack-room-select rooms team)))
          (cons (slack-room-name room team)
                (oref room id))))
     ((string= data-source "channels")
      (slack-if-let* ((channels (oref team channels))
                      (channel (slack-room-select channels team)))
          (cons (slack-room-name channel team)
                (oref channel id))))
     ((string= data-source "users")
      (slack-if-let* ((id (plist-get (slack--user-select team) :id)))
          (cons (slack-user-name id team)
                id)))
     ((string= "static" data-source)
      (slack-if-let* ((selected (slack-selectable-select-from-static-data-source this)))
          (cons (oref selected label)
                (oref selected value))))
     (t (error "Unknown element's data-source: %s" data-source))
     )))

(defmethod slack-dialog--submit ((_this slack-dialog)
                                 dialog-id team submission
                                 &optional after-success)
  (let ((url "https://slack.com/api/dialog.submit")
        (params (list (cons "submission" (json-encode-alist submission))
                      (cons "dialog_id" dialog-id))))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (when (functionp after-success)
                       (funcall after-success data))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :success #'on-success)))))

(defmethod slack-dialog-submit ((this slack-dialog) dialog-id team)
  (with-slots (elements) this
    (let ((submission (mapcar #'(lambda (element)
                                  (let ((value (slack-dialog-execute element dialog-id team)))
                                    (cons (oref element name) value)))
                              elements)))
      (slack-dialog--submit this dialog-id team submission))))

(defun slack-dialog-get (id team)
  (let ((url "https://slack.com/api/dialog.get")
        (params (list (cons "dialog_id" id))))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (slack-request-handle-error
                      (data "slack-dialog-get")
                      (slack-if-let*
                          ((payload (plist-get data :dialog))
                           (dialog (slack-dialog-create payload)))
                          ;; (slack-dialog-submit dialog id team)
                          (slack-buffer-display
                           (slack-create-dialog-buffer id
                                                       dialog
                                                       team))
                        ))))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :params params
        :success #'on-success)))))

(defmethod slack-dialog-notify-cancel ((this slack-dialog) dialog-id team)
  (slack-if-let* ((url "https://slack.com/api/dialog.notifyCancel")
                  (params (list (cons "dialog_id" dialog-id)))
                  (notify-cancelp (oref this notify-on-cancel)))
      (cl-labels
          ((on-success (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-dialog-notify-cancel"))))
        (slack-request
         (slack-request-create
          url
          team
          :type "POST"
          :params params
          :success #'on-success)))))

(defmethod slack-dialog-clear-errors ((this slack-dialog))
  (oset this error-message nil)
  (dolist (element (oref this elements))
    (oset element errors nil)))

(provide 'slack-dialog)
;;; slack-dialog.el ends here
