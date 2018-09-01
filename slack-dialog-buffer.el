;;; slack-dialog-buffer.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  南優也

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eieio)
(require 'slack-buffer)
(require 'slack-dialog)

(define-derived-mode slack-dialog-buffer-mode fundamental-mode "Slack Dialog Buffer"
  (setq-local default-directory slack-default-directory)
  (setq-local buffer-read-only t))

(defvar slack-dialog-submit-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-dialog-buffer-submit)
    (define-key map [mouse-1] #'slack-dialog-buffer-submit)
    map))

(defvar slack-dialog-cancel-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-dialog-buffer-cancel)
    (define-key map [mouse-1] #'slack-dialog-buffer-cancel)
    map))

(defvar slack-dialog-select-element-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-dialog-buffer-select)
    (define-key map [mouse-1] #'slack-dialog-buffer-select)
    map))

(defface slack-dialog-element-placeholder-face
  '((t (:inherit font-lock-comment-face :slant normal
                 ;; :box (:line-width 1 :color "#fff")
                 )))
  "Used to dialog's element placeholder"
  :group 'slack)

(defface slack-dialog-element-error-face
  '((t (:inherit font-lock-warning-face)))
  "Used to dialog's element error message"
  :group 'slack)

(defface slack-dialog-element-hint-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Used to dialog's element hint"
  :group 'slack)

(defface slack-dialog-element-label-face
  '((t (:weight bold)))
  "Used to dialog's element label"
  :group 'slack)

(defface slack-dialog-select-element-input-face
  '((t (:box (:line-width 1 :style released-button))))
  "Used to dialog's select element input"
  :group 'slack)

(defface slack-dialog-title-face
  '((t (:weight bold :height 1.2)))
  "Used to dialog's title"
  :group 'slack)

(defface slack-dialog-submit-button-face
  '((t (:box (:line-width 1 :style released-button)
             :foreground "#2aa198")))
  "Used to dialog's submit button"
  :group 'slack)

(defface slack-dialog-cancel-button-face
  '((t (:box (:line-width 1 :style released-button))))
  "Used to dialog's cancel button"
  :group 'slack)

(defclass slack-dialog-buffer (slack-buffer)
  ((dialog-id :initarg :dialog-id :type string)
   (dialog :initarg :dialog :type slack-dialog)))

(defmethod slack-buffer-name :static ((_class slack-dialog-buffer) dialog-id dialog team)
  (with-slots (title) dialog
    (format "*Slack Dialog - %s [%s] : %s*"
            title
            dialog-id
            (slack-team-name team))))

(defmethod slack-buffer-name ((this slack-dialog-buffer))
  (with-slots (dialog-id dialog team) this
    (slack-buffer-name 'slack-dialog-buffer
                       dialog-id dialog team)))

(defmethod slack-buffer-find :static ((class slack-dialog-buffer)
                                      dialog-id dialog team)
  (slack-buffer-find-4 class dialog-id dialog team))

(defmethod slack-buffer-insert-label ((this slack-dialog-element))
  (with-slots (label optional) this
    (insert (propertize label
                        'face 'slack-dialog-element-label-face))
    (when optional
      (insert " (optional)"))))

(defmethod slack-buffer-insert-hint ((this slack-dialog-text-element))
  (with-slots (hint) this
    (when hint
      (insert "\n")
      (insert (propertize hint
                          'face 'slack-dialog-element-hint-face))
      (insert "\n"))))

(defvar slack-dialog-element-edit-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-dialog-buffer-open-edit-element-buffer)
    (define-key map [mouse-1] #'slack-dialog-buffer-open-edit-element-buffer)
    map))

(defmethod slack-buffer-insert-edit-button ((this slack-dialog-text-element))
  (insert (propertize " Edit "
                      'face '(:box (:line-width 1 :style released-button))
                      'keymap slack-dialog-element-edit-button-map
                      'slack-dialog-element this)))

(defun slack-dialog-buffer-open-edit-element-buffer ()
  (interactive)
  (slack-if-let*
      ((element (get-text-property (point) 'slack-dialog-element))
       (buffer slack-current-buffer)
       (team (oref buffer team))
       (edit-buffer (slack-create-dialog-element-edit-buffer
                     buffer element team)))
      (slack-buffer-display edit-buffer)))

(defmethod slack-buffer-insert-placeholder ((this slack-dialog-text-element))
  (with-slots (placeholder) this
    (insert (propertize placeholder
                        'face 'slack-dialog-element-placeholder-face))))

(defmethod slack-buffer-insert-errors ((this slack-dialog-element))
  (with-slots (errors) this
    (mapc #'(lambda (err)
              (insert (propertize (oref err error-message)
                                  'face 'slack-dialog-element-error-face))
              (insert "\n"))
          errors)))

(defmethod slack-buffer-insert ((this slack-dialog-text-element))
  (with-slots (value placeholder errors) this
    (slack-buffer-insert-label this)
    (insert " ")
    (slack-buffer-insert-edit-button this)
    (insert "\n")
    (if value
        (insert value)
      (if placeholder
          (slack-buffer-insert-placeholder this)
        (insert "Click Edit")))
    (insert "\n")
    (slack-buffer-insert-errors this)
    (slack-buffer-insert-hint this)))

(defmethod slack-buffer-insert ((this slack-dialog-textarea-element))
  (with-slots (value placeholder) this
    (slack-buffer-insert-label this)
    (insert "  ")
    (slack-buffer-insert-edit-button this)
    (insert "\n")
    (if value
        (insert value)
      (if placeholder
          (slack-buffer-insert-placeholder this)
        (insert "Click Edit")))
    (insert "\n")
    (slack-buffer-insert-errors this)
    (slack-buffer-insert-hint this)))

(defun slack-dialog-buffer-select ()
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (oref buffer team))
       (dialog (oref buffer dialog))
       (dialog-id (oref buffer dialog-id))
       (element-name (get-text-property (point) 'slack-dialog-element-name))
       (dialog-element (cl-find-if #'(lambda (el) (string= element-name
                                                           (oref el name)))
                                   (oref dialog elements)))
       (selected (slack-dialog--execute dialog-element
                                        dialog-id
                                        team))
       (label (car selected))
       (value (cdr selected))
       (option (make-instance 'slack-dialog-select-option
                              :label label
                              :value value)))
      (progn
        (oset dialog-element selected-options (list option))
        (oset dialog-element value value)
        (slack-dialog-buffer-redisplay buffer))))

(defmethod slack-buffer-insert-select-button ((this slack-dialog-select-element))
  (let ((label (slack-if-let*
                   ((selected (slack-dialog-selected-option this)))
                   (slack-selectable-text selected)
                 "Choose an option...")))

    (insert (propertize (format " %s " label)
                        'face 'slack-dialog-select-element-input-face
                        'keymap slack-dialog-select-element-map
                        'slack-dialog-element-name (oref this name)))))

(defmethod slack-buffer-insert ((this slack-dialog-select-element))
  (slack-buffer-insert-label this)
  (insert "\n")
  (slack-buffer-insert-select-button this)
  (insert "\n")
  (slack-buffer-insert-errors this))

(defun slack-dialog-buffer-submit ()
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer))
      (slack-dialog-buffer--submit buffer)))

(defmethod slack-dialog-buffer--submit ((this slack-dialog-buffer))
  (with-slots (dialog dialog-id team) this
    (with-slots (elements) dialog
      (dolist (element elements)
        (let ((value (slack-dialog-element-value element)))
          (slack-dialog-element-validate element value)))
      (let ((params (mapcar #'(lambda (element)
                                (cons (oref element name)
                                      (slack-dialog-element-value element)))
                            elements)))
        (cl-labels
            ((create-dialog-element-error
              (payload)
              (make-instance #'slack-dialog-element-error
                             :name (plist-get payload :name)
                             :error-message (plist-get payload :error)))
             (set-dialog-element-error
              (dialog-error elements)
              (slack-if-let*
                  ((element (cl-find-if #'(lambda (el)
                                            (string= (oref el name)
                                                     (oref dialog-error name)))
                                        elements))
                   (new-errors (cons dialog-error
                                     (cl-remove-if #'(lambda (e)
                                                       (string= (oref e name)
                                                                (oref dialog-error
                                                                      name)))
                                                   (oref element errors)))))
                  (oset element errors new-errors)))
             (after-success
              (data)
              (slack-if-let* ((err (plist-get data :error)))
                  (progn
                    (oset dialog error-message err)
                    (dolist (dialog-error (mapcar #'create-dialog-element-error
                                                  (plist-get data :dialog_errors)))
                      (set-dialog-element-error dialog-error elements))

                    (slack-dialog-buffer-redisplay this))
                (slack-dialog-buffer-kill-buffer this))))
          (slack-dialog-clear-errors dialog)
          (slack-dialog--submit dialog dialog-id team params #'after-success))))))

(defun slack-dialog-buffer-cancel ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (with-slots (dialog dialog-id team) buffer
        (slack-dialog-notify-cancel dialog dialog-id team)
        (slack-dialog-buffer-kill-buffer buffer))))

(defmethod slack-dialog-buffer-kill-buffer ((this slack-dialog-buffer))
  (slack-if-let* ((buffer-name (slack-buffer-name this))
                  (buf (get-buffer buffer-name))
                  (win (get-buffer-window buf)))
      (progn
        (kill-buffer buf)
        (when (< 1 (count-windows))
          (delete-window win)))))

(defmethod slack-buffer-insert ((this slack-dialog-buffer))
  (with-slots (dialog) this
    (with-slots (error-message title elements submit-label) dialog
      (let ((inhibit-read-only t))
        (insert (propertize title
                            'face 'slack-dialog-title-face))
        (when error-message
          (insert "\n")
          (insert (propertize error-message
                              'face 'slack-dialog-element-error-face)))
        (insert "\n\n")
        (mapc #'(lambda (el)
                  (slack-buffer-insert el)
                  (insert "\n"))
              elements)
        (insert "\n")
        (insert (propertize " Cancel "
                            'face 'slack-dialog-cancel-button-face
                            'keymap slack-dialog-cancel-button-map))
        (insert "\t")
        (insert (propertize (format " %s " submit-label)
                            'face 'slack-dialog-submit-button-face
                            'keymap slack-dialog-submit-button-map))
        (goto-char (point-min))))))

(defmethod slack-buffer-init-buffer ((this slack-dialog-buffer))
  (let* ((buf (generate-new-buffer (slack-buffer-name this)))
         (dialog (oref this dialog))
         (dialog-id (oref this dialog-id))
         (team (oref this team)))
    (with-current-buffer buf
      (slack-dialog-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert this))
    (slack-buffer-push-new-4 'slack-dialog-buffer
                             dialog-id dialog team)))

(defun slack-create-dialog-buffer (dialog-id dialog team)
  (slack-if-let*
      ((buf (slack-buffer-find 'slack-dialog-buffer
                               dialog-id
                               dialog
                               team)))
      buf
    (make-instance 'slack-dialog-buffer
                   :dialog-id dialog-id
                   :dialog dialog
                   :team team)))

(defmethod slack-dialog-buffer-save-element-value ((this slack-dialog-buffer)
                                                   name
                                                   value)
  (with-slots (dialog) this
    (with-slots (elements) dialog
      (let ((element (cl-find-if #'(lambda (el)
                                     (string= name
                                              (oref el name)))
                                 elements)))
        (oset element value value)
        (slack-dialog-buffer-redisplay this)))))

(defmethod slack-dialog-buffer-redisplay ((this slack-dialog-buffer))
  (slack-if-let* ((bufname (slack-buffer-name this))
                  (buf (get-buffer bufname)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (cur-point (point)))
          (delete-region (point-min) (point-max))
          (slack-buffer-insert this)
          (when (and (< (point-min) cur-point)
                     (< cur-point (point-max)))
            (goto-char cur-point))))))

(provide 'slack-dialog-buffer)
;;; slack-dialog-buffer.el ends here
