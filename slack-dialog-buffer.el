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
  (setq-local buffer-read-only t)
  (add-hook 'after-change-functions #'slack-dialog-buffer-process-text-input nil t))

(defvar slack-dialog-text-element-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") nil)
    map))

(defvar slack-dialog-textarea-element-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'forward-line)
    map))

(defvar slack-dialog-submit-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-dialog-buffer-submit)
    (define-key map [mouse-1] #'slack-dialog-buffer-submit)
    map))

(defvar slack-dialog-select-element-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-dialog-buffer-select)
    (define-key map [mouse-1] #'slack-dialog-buffer-select)
    map))

(defface slack-dialog-element-error-face
  '((t (:inherit font-lock-warning-face)))
  "Used to dialog's element error message"
  :group 'slack)

(defface slack-dialog-element-hint-face
  '((t (:inherit font-lock-comment-face)))
  "Used to dialog's element hint"
  :group 'slack)

(defface slack-dialog-text-element-input-face
  '((t (:box (:line-width 1))))
  "Used to dialog's text element input"
  :group 'slack)

(defface slack-dialog-textarea-element-input-face
  '((t (:box (:line-width 1))))
  "Used to dialog's textarea element input"
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

(defun slack-dialog-end-of-field ()
  (1- (next-single-property-change (point)
                                   'slack-dialog-element
                                   nil
                                   (point-max))))

(defun slack-dialog-beginning-of-field ()
  (cond
   ((bobp) (point))
   ((not (eq (get-text-property (point) 'slack-dialog-element)
             (get-text-property (1- (point)) 'slack-dialog-element)))
    (point))
   (t (previous-single-property-change (point)
                                       'slack-dialog-element
                                       nil
                                       (point-min)))))

(defun slack-dialog-buffer-inspect ()
  (interactive)
  (message "%S" (line-end-position)))

(defmethod slack-dialog-element-input-face ((_this slack-dialog-textarea-element))
  'slack-dialog-textarea-element-input-face)

(defmethod slack-dialog-element-input-face ((_this slack-dialog-text-element))
  'slack-dialog-text-element-input-face)

(defmethod slack-dialog-element-input-face ((_this slack-dialog-select-element))
  'slack-dialog-select-element-input-face)

(defun slack-dialog-buffer-process-text-input (beg end replace-length)
  (slack-if-let*
      ((upper-bounds (< (1+ end) (point-max)))
       (lower-bounds (> (1- end) (point-min)))
       (length (- end beg replace-length))
       (pos (if (get-text-property (1+ end) 'slack-dialog-element)
                (1+ end)
              (if (get-text-property (1- end) 'slack-dialog-element)
                  (1- end))))
       (dialog-element (get-text-property pos 'slack-dialog-element)))
      (when (or (slack-dialog-textarea-element-p dialog-element)
                (slack-dialog-text-element-p dialog-element))
        (let ((inhibit-read-only t)
              (properties (slack-dialog-element-input-text-properties dialog-element)))
          (set-text-properties beg end properties)
          (when (< 0 length)
            (save-excursion
              (goto-char (if (slack-dialog-textarea-element-p dialog-element)
                             (1- (line-end-position))
                           (slack-dialog-end-of-field)))
              (while (and (< 0 length)
                          (eql (char-after (1- (point))) ? ))
                (delete-region (1- (point)) (point))
                (cl-decf length))))
          (when (> 0 length)
            (save-excursion
              (goto-char end)
              (goto-char (if (slack-dialog-textarea-element-p dialog-element)
                             (1- (line-end-position))
                           (1+ (slack-dialog-end-of-field))))
              (let ((cur-point (point)))
                (insert (make-string (abs length) ? ))
                (set-text-properties cur-point (point) properties))
              (goto-char (1- end))))))))

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

(defmethod slack-dialog-element-input-text-properties ((this slack-dialog-text-element))
  (list 'face 'slack-dialog-text-element-input-face
        'inhibit-read-only t
        'slack-dialog-element this
        'keymap slack-dialog-text-element-map))

(defmethod slack-buffer-insert-label ((this slack-dialog-element))
  (with-slots (label optional) this
    (insert (propertize label
                        'face 'slack-dialog-element-label-face))
    (when optional
      (insert " (optional)"))))

(defmethod slack-buffer-insert-hint ((this slack-dialog-text-element))
  (with-slots (hint) this
    (when hint
      (insert (propertize hint
                          'face 'slack-dialog-element-hint-face))
      (insert "\n"))))

(defmethod slack-buffer-insert ((this slack-dialog-text-element))
  (with-slots (value) this
    (slack-buffer-insert-label this)
    (insert "\n")
    (insert (apply #'propertize
                   (or (and value
                            (let ((pad-count (- 20 (length value))))
                              (if (< 0 pad-count)
                                  (format "%s%s" value (make-string pad-count ? ))
                                value)))
                       (make-string 20 ? ))
                   (slack-dialog-element-input-text-properties this)))
    (insert "\n")
    (slack-buffer-insert-hint this)))

(defmethod slack-dialog-element-input-text-properties ((this slack-dialog-textarea-element))
  (list 'face 'slack-dialog-textarea-element-input-face
        'inhibit-read-only t
        'keymap slack-dialog-textarea-element-map
        'slack-dialog-element this))

(defmethod slack-buffer-insert ((this slack-dialog-textarea-element))
  (with-slots (value) this
    (slack-buffer-insert-label this)
    (insert "\n")
    (let ((start (point))
          (lines 5)
          (width 50)
          (default-value (or value ""))
          (end nil))
      (insert default-value)
      (when (< (count-lines start (point)) lines)
        (dotimes (_ (- lines (count-lines start (point))))
          (insert "\n")))
      (setq end (point-marker))
      (goto-char start)
      (while (< (point) end)
        (end-of-line)
        (let ((pad (- width (- (point) (line-beginning-position)))))
          (when (< 0 pad)
            (insert (make-string pad ? ))))
        (set-text-properties (line-beginning-position) (point)
                             (slack-dialog-element-input-text-properties this))
        (forward-line 1)))
    (insert "\n")
    (slack-buffer-insert-hint this)))

(defun slack-dialog-buffer-select ()
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (oref buffer team))
       (dialog-element (get-text-property (point)
                                          'slack-dialog-element))
       (dialog-id (oref buffer dialog-id))
       (selected (slack-dialog--execute dialog-element
                                        dialog-id
                                        team))
       (label (car selected))
       (value (cdr selected))
       (option (make-instance 'slack-dialog-select-option
                              :label label
                              :value value))
       (beg (slack-dialog-beginning-of-field))
       (end (slack-dialog-end-of-field))
       (inhibit-read-only t))
      (progn
        (oset dialog-element selected-options (list option))
        (oset dialog-element value value)
        (save-excursion
          (delete-region beg (1+ end))
          (slack-buffer-insert-select-button dialog-element)))))

(defmethod slack-buffer-insert-select-button ((this slack-dialog-select-element))
  (let ((label (slack-if-let*
                   ((selected (slack-dialog-selected-option this)))
                   (slack-selectable-text selected)
                 "Choose an option...")))

    (insert (propertize (format " %s " label)
                        'face 'slack-dialog-select-element-input-face
                        'keymap slack-dialog-select-element-map
                        'slack-dialog-element this))))

(defmethod slack-buffer-insert ((this slack-dialog-select-element))
  (slack-buffer-insert-label this)
  (insert "\n")
  (slack-buffer-insert-select-button this)
  (insert "\n"))

(defun slack-dialog-buffer-submit ()
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (let ((params (make-hash-table :test 'equal))
          (elements (make-hash-table :test 'equal)))
      (while (< (point) (point-max))
        (let* ((line (thing-at-point 'line))
               (element (get-text-property 0
                                           'slack-dialog-element
                                           line))
               (error-message-region
                (get-text-property 0
                                   'slack-dialog-error-message
                                   line)))
          (when error-message-region
            (slack-if-let*
                ((inhibit-read-only t)
                 (beg (line-beginning-position))
                 (end (1+ (next-single-property-change
                           beg
                           'slack-dialog-error-message
                           nil
                           (point-max)))))
                (delete-region beg end)))

          (when element
            (puthash (oref element name)
                     element
                     elements))

          (when (and element
                     (or (slack-dialog-text-element-p element)
                         (slack-dialog-textarea-element-p element)))
            (let* ((beg (slack-dialog-beginning-of-field))
                   (end (slack-dialog-end-of-field))
                   (value (buffer-substring-no-properties beg end))
                   (name (oref element name)))
              (when (string-match " +\\'" value)
                (setq value (substring value 0 (match-beginning 0))))
              (when (< 0 (length value))
                (let ((prev-value (gethash name params nil)))
                  (puthash name
                           (or (and prev-value
                                    (format "%s\n%s"
                                            prev-value
                                            value))
                               value)
                           params)))))

          (when (and element
                     (slack-dialog-select-element-p element))
            (let ((name (oref element name))
                  (value (oref element value)))
              (when value
                (puthash name value params))))
          (forward-line 1)))

      (with-slots (dialog dialog-id team) slack-current-buffer
        (cl-labels
            ((after-success (data)
                            (slack-if-let*
                                ((err (plist-get data :error)))
                                (slack-dialog-buffer-display-error
                                 slack-current-buffer
                                 err
                                 (plist-get data :dialog_errors))
                              (slack-dialog-buffer-kill-buffer
                               slack-current-buffer)))
             (validate (key)
                       (slack-dialog-element-validate
                        (gethash key elements)
                        (gethash key params)))
             (build-param (key)
                          (cons key (gethash key params))))
          (let ((submission (mapcar #'(lambda (key)
                                        (validate key)
                                        (build-param key))
                                    (hash-table-keys params))))
            (slack-dialog--submit dialog
                                  dialog-id
                                  team
                                  submission
                                  #'after-success)))))))

(defmethod slack-dialog-buffer-display-error ((this slack-dialog-buffer) _err dialog-errors)
  (slack-if-let* ((inhibit-read-only t)
                  (buffer-name (slack-buffer-name this))
                  (buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (setq displayed-errors '())
          (while (< (point-min) (point))
            (let* ((line (thing-at-point 'line))
                   (element (get-text-property 0
                                               'slack-dialog-element
                                               line)))
              (when (and element
                         (not (cl-find (oref element name)
                                       displayed-errors
                                       :test #'string=)))
                (slack-if-let*
                    ((dialog-error (cl-find-if
                                    #'(lambda (e)
                                        (string= (plist-get e :name)
                                                 (oref element name)))
                                    dialog-errors))
                     (error-message (plist-get dialog-error :error)))
                    (progn
                      (push (oref element name) displayed-errors)
                      (save-excursion
                        (goto-char (1+ (slack-dialog-end-of-field)))
                        (insert "\n")
                        (let ((start (point)))
                          (insert error-message)
                          (put-text-property
                           start (point)
                           'face 'slack-dialog-element-error-face)
                          (put-text-property
                           start (point)
                           'slack-dialog-error-message t))))))
              (forward-line -1)))))))

(defmethod slack-dialog-buffer-kill-buffer ((this slack-dialog-buffer))
  (slack-if-let* ((buffer-name (slack-buffer-name this))
                  (buf (get-buffer buffer-name))
                  (win (get-buffer-window buf)))
      (progn
        (kill-buffer buf)
        (when (< 1 (count-windows))
          (delete-window win)))))

(defmethod slack-buffer-init-buffer ((this slack-dialog-buffer))
  (let* ((buf (generate-new-buffer (slack-buffer-name this)))
         (dialog (oref this dialog))
         (dialog-id (oref this dialog-id))
         (team (oref this team)))
    (with-slots (title elements submit-label) dialog
      (with-current-buffer buf
        (slack-dialog-buffer-mode)
        (slack-buffer-set-current-buffer this)
        (let ((inhibit-read-only t))
          (insert (propertize title
                              'face 'slack-dialog-title-face))
          (insert "\n\n")
          (mapc #'(lambda (el)
                    (slack-buffer-insert el)
                    (insert "\n"))
                elements)
          (insert "\n")
          (insert (propertize " Cancel "
                              'face 'slack-dialog-cancel-button-face))
          (insert "\t")
          (insert (propertize (format " %s " submit-label)
                              'face 'slack-dialog-submit-button-face
                              'keymap slack-dialog-submit-button-map))
          (goto-char (point-min))
          )))
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

(provide 'slack-dialog-buffer)
;;; slack-dialog-buffer.el ends here
