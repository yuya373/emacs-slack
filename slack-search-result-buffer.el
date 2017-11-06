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
(require 'slack-buffer)

(define-derived-mode slack-search-result-buffer-mode slack-buffer-mode "Slack Search Result")

(defclass slack-search-result-buffer (slack-buffer)
  ((search-result :initarg :search-result :type slack-search-result)
   (oldest :type string :initform "")))

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

(defface slack-search-result-message-header-face
  '((t (:weight bold :height 1.1 :underline t)))
  "Face used to search message header."
  :group 'slack)
;; (:inherit (markdown-code-face font-lock-constant-face))
(defface slack-search-result-message-username-face
  '((t (:inherit slack-message-output-header :underline nil)))
  ""
  :group 'slack)

(defmethod slack-buffer-insert ((this slack-search-result-buffer) match)
  (with-slots (team) this
    (with-slots (channel username) match
      (let* ((time (slack-ts-to-time (slack-ts match)))
             (room (slack-room-find (oref channel id) team))
             (header (propertize (format "%s%s"
                                         (if (slack-channel-p room)
                                             "#" "@")
                                         (slack-room-name room))
                                 'face 'slack-search-result-message-header-face)))
        (let ((lui-time-stamp-time time)
              (lui-time-stamp-format "[%Y-%m-%d %H:%M:%S]"))
          (lui-insert (propertize (format "%s\n%s"
                                          header
                                          (slack-message-to-string (oref match message) team))
                                  'ts (slack-ts match))
                      t))))))

(defmethod slack-buffer-has-next-page-p ((this slack-search-result-buffer))
  (with-slots (search-result) this
    (slack-search-has-next-page-p search-result)))

(defmethod slack-buffer-insert-history ((this slack-search-result-buffer))
  (with-slots (team search-result) this
    (let ((matches (oref search-result matches))
          (before-oldest (oref this oldest)))
      (oset this oldest (slack-ts (car matches)))
      (cl-loop for match in matches
               do (and (string< (slack-ts match) before-oldest)
                       (slack-buffer-insert this match)))

      (slack-if-let* ((point (slack-buffer-ts-eq (point-min)
                                           (point-max)
                                           before-oldest)))
          (goto-char point)))))

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
        (let ((lui-time-stamp-position nil))
          (if (slack-search-has-next-page-p search-result)
              (slack-buffer-insert-load-more this)))
        (let* ((messages (oref search-result matches))
               (oldest-message (car messages)))
          (cl-loop for m in messages
                   do (slack-buffer-insert this m))
          (when oldest-message
            (oset this oldest (slack-ts oldest-message)))))
      (goto-char (point-max)))
    (with-slots (search-result team) this
      (slack-buffer-push-new-3 'slack-search-result-buffer
                               search-result
                               team))
    buffer))


(provide 'slack-search-result-buffer)
;;; slack-search-result-buffer.el ends here
