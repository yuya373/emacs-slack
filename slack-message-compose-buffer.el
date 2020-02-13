;;; slack-message-compose-buffer.el ---              -*- lexical-binding: t; -*-

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
(require 'slack-message-editor)

(defconst slack-file-get-upload-url-url "https://slack.com/api/files.getUploadURL")
(defconst slack-file-upload-complete-url "https://slack.com/api/files.completeUpload")

(define-derived-mode slack-message-compose-buffer-mode
  slack-edit-message-mode
  "Slack Compose Message")

(defclass slack-message-compose-buffer-file ()
  ((path :initarg :path type string)
   (filename :initarg :filename :type string)
   (id :initarg :id :type (or null string) :initarg nil)))

(cl-defmethod slack--upload-file ((this slack-message-compose-buffer-file) team url cb)
  (let ((path (oref this path)))
    (cl-labels
        ((on-file-upload (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-upload-file")
                          (funcall cb t (oref this id))))
         (on-error (&allow-other-keys)
                   (funcall cb nil)))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :files (list (cons "file" path))
        :parser #'(lambda () (buffer-substring-no-properties (point-min) (point-max)))
        :headers (list (cons "Content-Type" "multipart/form-data"))
        :timeout nil
        :success #'on-file-upload
        :error #'on-error)))))

(cl-defmethod slack-upload-file ((this slack-message-compose-buffer-file) team cb)
  (let ((filename (oref this filename))
        (path (oref this path)))
    (cl-labels
        ((on-error (&allow-other-keys)
                   (funcall cb nil))
         (on-get-upload-url (&key data &allow-other-keys)
                            (condition-case err-var
                                (slack-request-handle-error
                                 (data "files.getUploadUrl"
                                       (lambda (err)
                                         (slack-log (format "Failed to get upload url of %s. %s" filename err)
                                                    team)
                                         (funcall cb nil)))
                                 (let ((url (plist-get data :upload_url))
                                       (file-id (plist-get data :file)))
                                   (oset this id file-id)
                                   (slack--upload-file this team url cb)))
                              (progn
                                (slack-log (format "Failed to get upload url. Error: %s" err-var)
                                           team)
                                (funcall cb nil)))))
      (slack-request
       (slack-request-create
        slack-file-get-upload-url-url
        team
        :type "POST"
        :params (list (cons "filename" filename)
                      (cons "length" (number-to-string (file-attribute-size (file-attributes path)))))
        :headers (list (cons "Content-Type" "multipart/form-data"))
        :success #'on-get-upload-url
        :error #'on-error)))))

(cl-defun slack-upload-files (team files &key on-success on-error)
  (let ((files-count (length files))
        (result nil)
        (timer nil)
        (failed-p nil))
    (cl-labels
        ((on-upload (success-p &optional file-id)
                    (if success-p
                        (push file-id result)
                      (setq failed-p t))))
      (dolist (file files)
        (slack-upload-file file team #'on-upload))
      (setq timer (run-at-time t 1 #'(lambda ()
                                       (slack-log (format "Uploading files... (%s/%s)" (length result) files-count)
                                                  team)
                                       (when failed-p
                                         (funcall on-error)
                                         (cancel-timer timer))
                                       (when (<= files-count (length result))
                                         (funcall on-success files)
                                         (cancel-timer timer))))))))

(cl-defun slack-files-upload-complete (team files message-payload &key (on-success nil) (on-error nil))
  (cl-labels ((on-complete (&key data &allow-other-keys)
                           (slack-request-handle-error
                            (data "slack-files-upload-complete"
                                  #'(lambda (err)
                                      (slack-log (format "Failed to files upload complete. FILES: %s, ERROR: %s"
                                                         (mapcar #'(lambda (file) (oref file filename))
                                                                 files)
                                                         err)
                                                 team)
                                      (when (functionp on-error)
                                        (funcall on-error))))
                            (when (functionp on-success)
                              (funcall on-success)))))
    (slack-request
     (slack-request-create
      slack-file-upload-complete-url
      team
      :type "POST"
      :data (json-encode (append (list (cons "files" (mapcar #'(lambda (file)
                                                                 (list (cons "id" (oref file id))
                                                                       (cons "title" (oref file filename))))
                                                             files)))
                                 message-payload))
      :headers (list (cons "Content-Type" "application/json;charset=utf-8"))
      :success #'on-complete))))

(defclass slack-message-compose-buffer (slack-buffer)
  ((room-id :initarg :room-id type string))
  :abstract t)

(cl-defmethod slack-buffer-room ((this slack-message-compose-buffer))
  (with-slots (room-id) this
    (slack-room-find room-id (slack-buffer-team this))))

(cl-defmethod slack-buffer-send-message ((this slack-message-compose-buffer) _message)
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (kill-buffer)
      (if (> (count-windows) 1) (delete-window)))))

(cl-defmethod slack-buffer-init-buffer ((this slack-message-compose-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-compose-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))


(provide 'slack-message-compose-buffer)
;;; slack-message-compose-buffer.el ends here
