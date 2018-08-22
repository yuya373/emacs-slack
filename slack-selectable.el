;;; slack-selectable.el ---                          -*- lexical-binding: t; -*-

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

(defclass slack-selectable ()
  (
   ;; one of users, channels, conversations, external or static
   (data-source :initarg :data_source :type string :initform "static")
   (options :initarg :options :initform nil)
   (option-groups :initarg :option_groups :initform nil)
   (selected-options :initarg :selected_options :type (or null list) :initform '())
   ))

(defclass slack-selectable-option ()
  ((text :initarg :text :type string)
   (value :initarg :value :type string)))

(defclass slack-selectable-option-group ()
  ((text :initarg :text :type string)
   (options :initarg :options :initform nil)))

(defmethod slack-selectable-text ((this slack-selectable-option))
  (oref this text))

(defmethod slack-selectable-text ((this slack-selectable-option-group))
  (oref this text))

(defmethod slack-selectable-select-from-static-data-source ((this slack-selectable))
  (cl-labels
      ((select-option (options)
                      (select (slack-selectable-prompt this)
                              (mapcar #'(lambda (option)
                                          (cons (slack-selectable-text option)
                                                (oref option value)))
                                      options)))
       (select-option-group
        (option-groups)
        (slack-if-let*
            ((text (select (slack-selectable-prompt this)
                           (mapcar #'(lambda (option-group)
                                       (slack-selectable-text option-group))
                                   option-groups))))
            (find-option text option-groups)))
       (find-option (text options)
                    (cl-find-if #'(lambda (option)
                                    (string= text (slack-selectable-text option)))
                                options))
       (select (prompt options)
               (funcall slack-completing-read-function
                        prompt
                        options
                        nil t)))
    (with-slots (options option-groups) this
      (slack-if-let*
          ((options (if option-groups
                        (oref (select-option-group option-groups)
                              options)
                      options))
           (option-text (select-option options)))
          (find-option option-text options)))))

(provide 'slack-selectable)
;;; slack-selectable.el ends here
