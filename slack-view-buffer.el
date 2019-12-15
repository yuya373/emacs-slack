;;; slack-view-buffer.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
(require 'slack-team)
(require 'slack-block)
(require 'slack-view)

(define-derived-mode slack-view-buffer-mode slack-buffer-mode "Slack View"
  (setq-local lui-max-buffer-size nil)
  (add-hook 'lui-post-output-hook 'slack-display-image t t))

(defclass slack-view-buffer (slack-buffer)
  ((channel-id :initarg :channel-id :type (or null string) :initform nil)
   (client-token :initarg :client-token :type string)
   (timeout-range :initarg :timeout-range :type number)
   (event-ts :initarg :event-ts :type string)
   (root-view-id :initarg :root-view-id :type string)
   (current-view-id :initarg :current-view-id :type string)
   (current-view :initarg :curren-view :type slack-view)
   (views :initarg :views :type list)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-view-buffer)) view team)
  (format "*Slack - %s View: %s [%s]"
          (slack-team-name team)
          (oref view title)
          (oref view root-view-id)))

(cl-defmethod slack-buffer-init-buffer :after ((this slack-view-buffer))
  (slack-buffer-push-new-3 (eieio-object-class-name this)
                           (oref this current-view)
                           (oref this team)))

(cl-defmethod slack-buffer-name ((this slack-view-buffer))
  (slack-buffer-name (eieio-object-class-name this)
                     (oref this current-view)
                     (oref this team)))

(cl-defmethod slack-buffer-buffer ((this slack-view-buffer))
  (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
      (progn
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (slack-buffer-insert this)))
        buf)
    (slack-buffer-init-buffer this)))

(cl-defmethod slack-buffer-init-buffer ((this slack-view-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-view-buffer-mode)
      (slack-buffer-set-current-buffer buf)
      (slack-buffer-insert this))
    buf))

(cl-defmethod slack-buffer-insert ((this slack-view-buffer))
  (delete-region (point-min) (marker-position lui-output-marker))
  (let* ((view (oref this current-view))
         (title (oref view title)))
    (lui-insert (slack-block-to-string title))
    ))

(defun slack-create-view-buffer (team &key channel-id client-token timeout-range event-ts view)
  (slack-view-buffer :team team
                     :channel-id channel-id
                     :client-token client-token
                     :timeout-range timeout-range
                     :event-ts event-ts
                     :root-view-id (oref view root-view-id)
                     :current-view-id (oref view id)
                     :current-view view
                     :views (list view)))

(provide 'slack-view-buffer)
;;; slack-view-buffer.el ends here
