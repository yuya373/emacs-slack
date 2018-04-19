;;; slack-search-result-buffer.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2017

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
(require 'slack-buffer)

(define-derived-mode slack-search-result-buffer-mode slack-buffer-mode "Slack Search Result"
  (remove-hook 'lui-post-output-hook 'slack-display-image t))

(defclass slack-search-result-buffer (slack-buffer)
  ((search-result :initarg :search-result :type slack-search-result)))

(defmethod slack-buffer-name :static ((class slack-search-result-buffer) search-result team)
  (with-slots (query sort sort-dir) search-result
    (format "*Slack - %s : Search Result - QUERY: %s, ORDER BY: %s %s"
            (oref team name)
            query
            sort
            (upcase sort-dir))))

(defmethod slack-buffer-name ((this slack-search-result-buffer))
  (with-slots (search-result team) this
    (slack-buffer-name (eieio-object-class-name this) search-result team)))

(defun slack-create-search-result-buffer (search-result team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-search-result-buffer
                                             search-result
                                             team)))
      buffer
    (make-instance 'slack-search-result-buffer
                   :team team
                   :search-result search-result)))

(defmethod slack-buffer-insert ((this slack-search-result-buffer) match)
  (with-slots (team) this
    (let* ((time (slack-ts-to-time (slack-ts match)))
           (lui-time-stamp-time time)
           (lui-time-stamp-format "[%Y-%m-%d %H:%M:%S]"))
      (lui-insert (slack-message-to-string match team) t)
      (lui-insert "" t))))

(defmethod slack-buffer-has-next-page-p ((this slack-search-result-buffer))
  (with-slots (search-result) this
    (slack-search-has-next-page-p search-result)))

(defmethod slack-buffer-insert-history ((this slack-search-result-buffer))
  (with-slots (team search-result) this
    (let* ((paging (oref search-result paging))
           (per-page (oref paging count))
           (matches (last (oref search-result matches) per-page))
           (cur-point (point)))
      (cl-loop for match in matches
               do (slack-buffer-insert this match))
      (goto-char cur-point))))

(defmethod slack-buffer-request-history ((this slack-search-result-buffer) after-success)
  (with-slots (team search-result) this
    (slack-search-request search-result after-success team
                          (slack-search-paging-next-page
                           (oref search-result paging)))))

(defmethod slack-buffer-init-buffer ((this slack-search-result-buffer))
  (let ((buffer (generate-new-buffer (slack-buffer-name this))))
    (with-current-buffer buffer
      (slack-search-result-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (search-result) this
        (let* ((messages (oref search-result matches)))
          (cl-loop for m in messages
                   do (slack-buffer-insert this m)))
        (let ((lui-time-stamp-position nil))
          (if (slack-search-has-next-page-p search-result)
              (slack-buffer-insert-load-more this)))))

    (with-slots (search-result team) this
      (slack-buffer-push-new-3 'slack-search-result-buffer
                               search-result
                               team))
    buffer))

(defmethod slack-buffer-loading-message-end-point ((this slack-search-result-buffer))
  (previous-single-property-change (point-max)
                                   'loading-message))

(defmethod slack-buffer-delete-load-more-string ((this slack-search-result-buffer))
  (let* ((inhibit-read-only t)
         (loading-message-end
          (slack-buffer-loading-message-end-point this))
         (loading-message-start
          (previous-single-property-change loading-message-end
                                           'loading-message)))
    (delete-region loading-message-start
                   loading-message-end)))

(defmethod slack-buffer-prepare-marker-for-history ((_this slack-search-result-buffer)))

(defmethod slack-buffer-insert--history ((this slack-search-result-buffer))
  (slack-buffer-insert-history this)
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n"))))

(provide 'slack-search-result-buffer)
;;; slack-search-result-buffer.el ends here
