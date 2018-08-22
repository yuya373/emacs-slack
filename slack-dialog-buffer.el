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

(defun slack-dialog-end-of-field ()
  (1- (next-single-property-change (point)
                                   'slack-dialog-element
                                   nil
                                   (point-max))))

(defun slack-dialog-buffer-process-text-input (beg end replace-length)
  (slack-if-let*
      ((text (buffer-substring-no-properties beg end))
       (length (- end beg replace-length))
       (inhibit-read-only t)
       (pos (and (< (1+ end) (point-max))
                 (> (1- end) (point-min))
                 (if (get-text-property (1+ end)
                                        'slack-dialog-element)
                     (1+ end)
                   (if (get-text-property (1- end)
                                          'slack-dialog-element)
                       (1- end)))))
       (properties (text-properties-at pos)))
      (progn
        (set-text-properties beg end properties)
        (when (< 0 length)
          (save-excursion
            (goto-char (slack-dialog-end-of-field))
            (while (and (< 0 length)
                        (eql (char-after (1- (point))) ? ))
              (delete-region (1- (point)) (point))
              (cl-decf length))))
        (when (> 0 length)
          (save-excursion
            (goto-char end)
            (goto-char (slack-dialog-end-of-field))
            (let ((cur-point (point)))
              (insert (make-string (abs length) ? ))
              (set-text-properties cur-point (point) properties))
            )))

    (message "TEXT: %s, LENGTH: %s, %s"
             text replace-length (- end beg replace-length))))

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
          (insert title)
          (insert "\n")
          (mapc #'(lambda (el)
                    (with-slots (label) el
                      (insert label)
                      (insert "\n")
                      (insert (propertize (make-string 20 ? )
                                          'face '(:box (:line-width 1))
                                          'inhibit-read-only t
                                          'slack-dialog-element el))
                      (insert "\n")))
                elements)
          (insert submit-label)
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
