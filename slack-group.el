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
(defvar slack-group-subscription '())

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


(defun slack-group-subscribedp (group)
  (let ((group-name (gethash "name" group)))
    (and group-name
         (memq (interm group-name) slack-group-subscription))))

(defun slack-group-get-buffer-name (group)
  (concat slack-group-buffer-name " : " (gethash "name" group)))

(defun slack-group-buffer-header (group)
  (concat "Private Group: " (gethash "name" group) "\n"))

(defun slack-group (name)
  (interactive (list (slack-group-read-list
                      "Select Group: "
                      (slack-group-names))))
  (let ((group (slack-group-find-by-name name)))
    (slack-group-update-history group)
    (switch-to-buffer-other-window
     (slack-buffer-create (slack-group-get-buffer-name group)
                          (gethash "id" group)
                          (slack-group-buffer-header group)
                          (gethash "messages" group)))))

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
  (cl-labels ((on-group-update (&key data &allow-other-keys)
                               (slack-group-on-update-history
                                data group)))
    (slack-request
     slack-group-history-url
     :params (list (cons "token" slack-token)
                   (cons "channel" (gethash "id" group)))
     :success #'on-group-update)))

(defun slack-group-on-update-history (data group)
  (unless (plist-get data :ok)
    (error "%s" data))
  (let ((messages (plist-get data :messages)))
    (slack-group-set-messages group
                              (slack-message-create-with-room
                               messages
                               group))))

(defun slack-group-read-list (prompt choices)
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

(defun slack-group-set-messages (group messages)
  (puthash "messages" messages group))

(provide 'slack-group)
;;; slack-group.el ends here
