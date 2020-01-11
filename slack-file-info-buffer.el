;;; slack-file-info-buffer.el ---                    -*- lexical-binding: t; -*-

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
(require 'slack-message)
(require 'slack-file)
(require 'slack-message-formatter)
(require 'slack-message-reaction)
(require 'slack-star)

(defvar slack-file-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-file-display)
    (define-key map [mouse-1] #'slack-file-display)
    map))

(defun slack-file-display ()
  (interactive)
  (slack-if-let* ((id (get-text-property (point) 'file))
                  (buf slack-current-buffer))
      (slack-buffer-display-file buf id)))

(cl-defmethod slack-buffer-display-file ((this slack-buffer) file-id)
  (let ((team (slack-buffer-team this)))
    (cl-labels
        ((open (file &rest _args)
               (slack-buffer-display (slack-create-file-info-buffer team file))))
      (slack-file-request-info file-id 1 team #'open))))

(define-derived-mode slack-file-info-buffer-mode slack-buffer-mode  "Slack File Info"
  (setq-local lui-max-buffer-size nil)
  (add-hook 'lui-post-output-hook 'slack-display-image t t))

(defclass slack-file-info-buffer (slack-buffer)
  ((file :initarg :file :type slack-file)))

(cl-defmethod slack-buffer-name ((this slack-file-info-buffer))
  (let ((file (oref this file))
        (team (slack-buffer-team this)))
    (format "*Slack - %s File: %s"
            (oref team name)
            (or (slack-file-title file)
                (oref file id)))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-file-info-buffer)) file)
  (oref file id))

(cl-defmethod slack-buffer-key ((this slack-file-info-buffer))
  (slack-buffer-key 'slack-file-info-buffer (oref this file)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-file-info-buffer)))
  'slack-file-info-buffer)

(defun slack-create-file-info-buffer (team file)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer team file)))
      (progn
        (oset buffer file file)
        buffer)
    (slack-file-info-buffer :team-id (oref team id) :file file)))

(cl-defmethod slack-buffer-init-buffer ((this slack-file-info-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-file-info-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert this t))
    buf))

(cl-defmethod slack-buffer-download-file ((this slack-file-info-buffer) file-id)
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find file-id team)))
      (slack-file-download file team)))

(cl-defmethod slack-buffer-run-file-action ((this slack-file-info-buffer) file-id)
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find file-id team)))
      (slack-file-run-action file this)))

(cl-defmethod slack-buffer-file-content-to-string ((this slack-file-info-buffer))
  (with-slots (file) this
    (slack-if-let* ((content (oref file content))
                    (html (oref content content-highlight-html))
                    (css (oref content content-highlight-css)))
        (propertize (concat "<style>\n" css "</style>" "\n" html)
                    'slack-file-html-content t)
      "")))

(cl-defmethod slack-file-body-to-string ((file slack-file))
  (let* ((url (oref file url-private))
         (type (slack-file-type file))
         (size (slack-file-size file))
         (title (slack-file-title file)))
    (slack-format-message (propertize (format "<%s|%s>" url title)
                                      'face '(:weight bold))
                          (format "%s%s"
                                  (or (and size (format "%s " size)) "")
                                  type))))

(cl-defmethod slack-file-body-to-string ((this slack-file-email))
  (let* ((label-face '(:foreground "#586e75" :weight bold))
         (from (format "%s %s"
                       (propertize "From:" 'face label-face)
                       (mapconcat #'(lambda (e) (oref e original))
                                  (oref this from)
                                  ", ")))
         (to (format "%s %s"
                     (propertize "To:" 'face label-face)
                     (mapconcat #'(lambda (e) (oref e original))
                                (oref this to)
                                ", ")))
         (cc (format "%s %s"
                     (propertize "CC:" 'face label-face)
                     (mapconcat #'(lambda (e) (oref e original))
                                (oref this cc)
                                ", ")))
         (subject (format "%s %s"
                          (propertize "Subject:" 'face label-face)
                          (propertize (oref this subject)
                                      'face '(:weight bold :height 1.1))))
         (body (propertize (format "\n%s" (oref this plain-text))
                           'slack-defer-face #'slack-put-email-body-overlay))
         (date (format "%s %s"
                       (propertize "Date:" 'face label-face)
                       (slack-format-ts (oref this created)))))
    (mapconcat #'identity
               (list from to cc subject date "" body)
               "\n")))

(cl-defmethod slack-message-to-string ((this slack-file-comment) team)
  (with-slots (user comment) this
    (let ((name (slack-user-name user team))
          (status (slack-user-status user team)))
      (format "%s\n%s\n"
              (propertize (format "%s %s" name status)
                          'face 'slack-message-output-header)
              (slack-unescape comment team)))))

(cl-defmethod slack-buffer-file-to-string ((this slack-file-info-buffer))
  (let* ((file (oref this file))
         (team (slack-buffer-team this))
         (user-name (slack-user-name (oref file user) team))
         (header (format "%s %s %s%s"
                         (propertize user-name
                                     'face '(:weight bold))
                         (if (oref file is-starred) ":star:" "")
                         (if (slack-file-downloadable-p file)
                             (format "%s "
                                     (slack-file-download-button file))
                           "")
                         (slack-file-action-button file)))
         (timestamp (and (oref file timestamp)
                         (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (seconds-to-time
                                              (oref file timestamp)))))
         (body (slack-file-body-to-string file))
         (content (slack-buffer-file-content-to-string this))
         (thumb (or (and (slack-file-image-p file)
                         (slack-message-large-image-to-string file))
                    (slack-message-image-to-string file)))
         (comments (mapconcat #'(lambda (comment)
                                  (slack-message-to-string comment team))
                              (oref file comments)
                              "\n")))
    (propertize (slack-format-message header
                                      timestamp
                                      " "
                                      body
                                      " "
                                      content
                                      " "
                                      thumb
                                      " "
                                      comments)
                'file-id (oref file id))))

(cl-defmethod slack-buffer-insert ((this slack-file-info-buffer) &optional not-tracked-p)
  (let ((file (oref this file)))
    (let ((lui-time-stamp-position nil))
      (lui-insert-with-text-properties
       (slack-buffer-file-to-string this)
       ;; saved-text-properties not working??
       'file-id (oref file id)
       'ts (slack-ts file)
       'not-tracked-p not-tracked-p)))
  (slack-if-let* ((html-beg (cl-loop for i from (point-min) to lui-output-marker
                                     if (get-text-property i 'slack-file-html-content)
                                     return i))
                  (html-end (next-single-property-change html-beg
                                                         'slack-file-html-content))
                  (inhibit-read-only t))
      (shr-render-region html-beg html-end))
  (goto-char (point-min)))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-file-info-buffer) reaction _ts)
  (let ((file (oref this file))
        (team (slack-buffer-team this)))
    (slack-file-add-reaction (oref file id) reaction team)))

(cl-defmethod slack-buffer-add-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-add-url)
        (file (oref this file))
        (team (slack-buffer-team this)))
    (slack-star-api-request url
                            (list (slack-message-star-api-params file))
                            team)))

(cl-defmethod slack-buffer-remove-star ((this slack-file-info-buffer) _ts)
  (let ((url slack-message-stars-remove-url)
        (file (oref this file))
        (team (slack-buffer-team this)))
    (slack-star-api-request url
                            (list (slack-message-star-api-params
                                   file))
                            team)))

(cl-defmethod slack-buffer--replace ((this slack-file-info-buffer) _ts)
  (slack-if-let* ((buffer (slack-buffer-buffer this)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (slack-buffer-insert this)))))

(cl-defmethod slack-buffer-update ((this slack-file-info-buffer))
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (slack-buffer-insert this)))))

(defun slack-file-update ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (file (oref buf file))
                  (team (slack-buffer-team buf))
                  (page (oref file page)))
      (slack-file-request-info
       file page team
       #'(lambda (file team &rest _args)
           (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer team)))
               (slack-buffer-replace buffer file))))))

(cl-defmethod slack-file-run-action ((file slack-file) buf)
  (let* ((actions (list (and (not (slack-file-info-buffer-p buf))
                             (cons "View details"
                                   #'(lambda ()
                                       (slack-buffer-display-file
                                        buf
                                        (slack-file-id file)))))
                        (cons "Copy link to file"
                              #'(lambda ()
                                  (kill-new (oref file permalink))))
                        (if (oref file is-starred)
                            (cons "Unstar file"
                                  #'(lambda ()
                                      (slack-buffer-remove-star
                                       buf
                                       (slack-file-id file))))
                          (cons "Star file"
                                #'(lambda ()
                                    (slack-buffer-add-star
                                     buf
                                     (slack-file-id file)))))
                        (cons "Open original"
                              #'(lambda ()
                                  (browse-url (oref file url-private))))))
         (selected (completing-read "Action: "
                                    (cl-remove-if #'null actions)
                                    nil t))
         (action (cdr-safe (assoc-string selected actions))))
    (when action
      (funcall action))))

(provide 'slack-file-info-buffer)
;;; slack-file-info-buffer.el ends here
