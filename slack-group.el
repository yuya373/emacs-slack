;;; slack-group.el --- slack group functions
;;; Commentary:
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
;; Keywords:
;;; Code:

(defvar slack-groups)
(defvar slack--group-open-url "https://slack.com/api/groups.open")
(defvar slack-group-history-url "https://slack.com/api/groups.history")
(defvar slack-group-buffer-name "*Slack - Private Group*")
(defvar slack-update-group-list-url "https://slack.com/api/groups.list")

(defun slack-group-find (id)
  (find-if (lambda (group) (string= id (gethash "id" group)))
           slack-groups))

(defun slack-group-find-by-name (name)
  (find-if (lambda (group) (string= name (gethash "name" group)))
           slack-groups))

(defun slack-group-name (id)
  (let ((group (slack-group-find id)))
    (if group
        (gethash "name" group))))

(defun slack-group-names ()
  (mapcar (lambda (group)
            ;; (cons (gethash "id" group) (gethash "name" group))
            (gethash "name" group)
            )
          slack-groups))

(defun slack-group-id (name)
  (let ((group (slack-group-find-by-name name)))
    (unless group
      (error "group not found"))
    (gethash "id" group)))

(defun slack-group-get-buffer-name (group)
  (concat slack-group-buffer-name " : " (gethash "name" group)))

(defun slack-group (name)
  (interactive (list (slack-group-read-list
                      "Select Group: "
                      (slack-group-names))))
  (let ((group (slack-group-find-by-name name)))
    (unless group
      (error "slack-group (%s) not found." name))
    (slack-group-update-history group)
    (switch-to-buffer-other-window (slack-buffer-create
                                    group
                                    (slack-group-get-buffer-name group)))))

(defun slack-update-group-list ()
  (interactive)
  (cl-labels ((on-update-group-list
               (&key data &allow-other-keys)
               (unless (gethash "ok" data)
                 (error "slack-update-group-list failed"))
               (setf slack-groups (gethash "groups" data))))
    (slack-request
     slack-update-group-list-url
     :params (list (cons "token" slack-token))
     :success #'on-update-group-list)))

(defun slack-group-update-history (group)
  (cl-labels ((on-group-update
               (&key data &allow-other-keys)
               (unless (gethash "ok" data)
                 (error data))
               (slack-message-set group
                                  (gethash "messages" data))))
    (slack-request
     slack-group-history-url
     :params (list (cons "token" slack-token)
                   (cons "channel" (gethash "id" group)))
     :success #'on-group-update)))

(defun slack-group-read-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

(provide 'slack-group)
;;; slack-group.el ends here
