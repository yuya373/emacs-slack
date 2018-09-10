;;; slack-user.el ---slack user interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'slack-util)
(require 'slack-request)
(require 'slack-room)

(defconst slack-dnd-team-info-url "https://slack.com/api/dnd.teamInfo")
(defconst slack-dnd-end-dnd-url "https://slack.com/api/dnd.endDnd")
(defconst slack-dnd-set-snooze-url "https://slack.com/api/dnd.setSnooze")
(defconst slack-set-presence-url "https://slack.com/api/users.setPresence")
(defconst slack-user-info-url "https://slack.com/api/users.info")
(defconst slack-user-profile-set-url "https://slack.com/api/users.profile.set")
(defconst slack-bot-info-url "https://slack.com/api/bots.info")
(defconst slack-bot-list-url "https://slack.com/api/bots.list")
(defvar slack-current-user-id nil)

(defun slack-user--find (id team)
  (with-slots (users) team
    (cl-find-if (lambda (user)
                  (string= id (plist-get user :id)))
                users)))

(defun slack-user-find-by-name (name team)
  (with-slots (users) team
    (cl-find-if (lambda (user)
                  (string= name (plist-get user :name)))
                users)))

(defun slack-user-get-id (name team)
  (let ((user (slack-user-find-by-name name team)))
    (if user
        (plist-get user :id))))

(defun slack-user-name (id team)
  (slack-if-let* ((user (slack-user--find id team)))
      (if (oref team full-and-display-names)
          (plist-get user :real_name)
        (plist-get user :name))))

(defun slack-user-status (id team)
  (let* ((user (slack-user--find id team))
         (profile (and user (plist-get user :profile)))
         (emoji (and profile (plist-get profile :status_emoji)))
         (text (and profile (plist-get profile :status_text))))
    (mapconcat #'identity (cl-remove-if #'null (list emoji text))
               " ")))

(defun slack-user-names (team)
  (with-slots (users) team
    (mapcar (lambda (u) (cons (plist-get u :name) u))
            (cl-remove-if #'slack-user-hidden-p users))))

(defun slack-user-dnd-in-range-p (user)
  (let ((current (time-to-seconds))
        (dnd-start (plist-get (plist-get user :dnd_status) :next_dnd_start_ts))
        (dnd-end (plist-get (plist-get user :dnd_status) :next_dnd_end_ts)))
    (and dnd-start dnd-end
         (<= dnd-start current)
         (<= current dnd-end))))

(defun slack-user-dnd-status-to-string (user)
  (if (slack-user-dnd-in-range-p user)
      "Z"
    nil))

(defun slack-user-presence-to-string (user)
  (if (string= (plist-get user :presence) "active")
      "*"
    " "))

(defun slack-user-set-status ()
  (interactive)
  (let ((team (slack-team-select))
        (emoji (slack-select-emoji))
        (text (read-from-minibuffer "Text: ")))
    (slack-user-set-status-request  team emoji text)))

(defun slack-user-set-status-request (team emoji text)
  (cl-labels ((on-success
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-user-set-status-request"))))
    (slack-request
     (slack-request-create
      slack-user-profile-set-url
      team
      :type "POST"
      :data (list (cons "id" (oref team self-id))
                  (cons "profile"
                        (json-encode (list (cons "status_text" text)
                                           (cons "status_emoji" emoji)))))
      :success #'on-success))))

(defun slack-bot-info-request (bot_id team &optional after-success)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-bot-info-request")
                    (push (plist-get data :bot) (oref team bots))
                    (if after-success
                        (funcall after-success team)))))
    (slack-request
     (slack-request-create
      slack-bot-info-url
      team
      :params (list (cons "bot" bot_id))
      :success #'on-success))))

(defun slack-bot-list-update (&optional team)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-bot-list-update")
           (oset team bots (append (plist-get data :bots) nil)))))
      (slack-request
       (slack-request-create
        slack-bot-list-url
        team
        :success #'on-success)))))

(defface slack-user-profile-header-face
  '((t (:foreground "#FFA000"
                    :weight bold
                    :height 1.5)))
  "Face used to user profile header."
  :group 'slack)

(defface slack-user-profile-property-name-face
  '((t (:weight bold :height 1.2)))
  "Face used to user property."
  :group 'slack)

(define-derived-mode slack-user-profile-mode fundamental-mode "Slack User"
  ""
  (setq-local default-directory slack-default-directory))

(defun slack-user-profile (user)
  (plist-get user :profile))

(defun slack-user-fname (user)
  (plist-get (slack-user-profile user) :first_name))

(defun slack-user-lname (user)
  (plist-get (slack-user-profile user) :last_name))

(defun slack-user-header (user)
  (let* ((fname (slack-user-fname user))
         (lname (slack-user-lname user))
         (name (plist-get user :name)))
    (or (and fname lname
             (format "%s %s - @%s"
                     (slack-user-fname user)
                     (slack-user-lname user)
                     (plist-get user :name)))
        name)))

(defun slack-user-timezone (user)
  (let ((offset (/ (plist-get user :tz_offset) (* 60 60))))
    (format "%s, %s"
            (or (plist-get user :tz)
                (plist-get user :tz_label))
            (if (<= 0 offset)
                (format "+%s hour" offset)
              (format "%s hour" offset)))))

(defun slack-user-property-to-str (value title)
  (and value (< 0 (length value))
       (format "%s\n\t%s"
               (propertize title 'face 'slack-user-profile-property-name-face)
               value)))

(defun slack-user-profile-to-string (id team)
  (let* ((user (slack-user--find id team))
         (image (slack-image-string (list (slack-user-image-url user 512)
                                          nil nil nil (window-width
                                                       (get-buffer-window
                                                        (current-buffer))
                                                       t))
                                    nil t))
         (profile (slack-user-profile user))
         (header (propertize (slack-user-header user)
                             'face 'slack-user-profile-header-face))
         (presence (slack-user-property-to-str (plist-get user :presence) "Presence"))
         (status (slack-user-property-to-str (slack-user-status id team) "Status"))
         (timezone (slack-user-property-to-str (slack-user-timezone user) "Timezone"))
         (email (slack-user-property-to-str (plist-get profile :email) "Email"))
         (phone (slack-user-property-to-str (plist-get profile :phone) "Phone"))
         (skype (slack-user-property-to-str (plist-get profile :skype) "Skype"))
         (body (mapconcat #'identity
                          (cl-remove-if #'null
                                        (list presence status timezone email phone skype))
                          "\n"))
         (dm-button (propertize "[Open Direct Message]"
                                'face '(:underline t)
                                'keymap (let ((map (make-sparse-keymap)))
                                          (define-key map (kbd "RET")
                                            #'(lambda ()
                                                (interactive)
                                                (slack-if-let* ((buf slack-current-buffer))
                                                    (slack-buffer-display-im buf))))
                                          map))))
    (format "\n%s\n\n%s%s\n%s\n\n%s" image header (format "  (%s)" id) body dm-button)))

(defun slack-user-self-p (user-id team)
  (string= user-id (oref team self-id)))

(defun slack-user-name-alist (team &key filter)
  (let ((users (oref team users)))
    (mapcar #'(lambda (e) (cons (slack-user-name (plist-get e :id) team) e))
            (if filter (funcall filter users)
              users))))

(defun slack-user-hidden-p (user)
  (not (eq (plist-get user :deleted) :json-false)))

(defun slack--user-select (team)
  (slack-select-from-list ((slack-user-names team) "Select User: ")))

(defun slack-user-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-user-name-alist
                 team
                 :filter #'(lambda (users)
                             (cl-remove-if #'slack-user-hidden-p users)))))
    (slack-select-from-list (alist "Select User: ")
        (let ((buf (slack-create-user-profile-buffer team (plist-get selected :id))))
          (slack-buffer-display buf)))))

(cl-defun slack-user-info-request (user-id team &key after-success)
  (cl-labels
      ((on-success
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-user-info-request")
         (oset team users (cons (plist-get data :user)
                                (cl-remove-if #'(lambda (user)
                                                  (string= (plist-get user :id) user-id))
                                              (oref team users))))
         (slack-im-open (plist-get data :user))
         (when after-success (funcall after-success)))))
    (slack-request
     (slack-request-create
      slack-user-info-url
      team
      :params (list (cons "user" user-id))
      :success #'on-success))))

(defun slack-user-image-url-24 (user)
  (plist-get (slack-user-profile user) :image_24))

(defun slack-user-image-url-32 (user)
  (plist-get (slack-user-profile user) :image_32))

(defun slack-user-image-url-48 (user)
  (plist-get (slack-user-profile user) :image_48))

(defun slack-user-image-url-72 (user)
  (plist-get (slack-user-profile user) :image_72))

(defun slack-user-image-url-512 (user)
  (plist-get (slack-user-profile user) :image_512))

(defun slack-user-image-url (user size)
  (cond
   ((eq size 24) (slack-user-image-url-24 user))
   ((eq size 32) (slack-user-image-url-32 user))
   ((eq size 48) (slack-user-image-url-48 user))
   ((eq size 72) (slack-user-image-url-72 user))
   ((eq size 512) (slack-user-image-url-512 user))
   (t (slack-user-image-url-32 user))))

(defun slack-user-fetch-image (user size team)
  (let* ((image-url (slack-user-image-url user size))
         (file-path (and image-url (slack-profile-image-path image-url team))))
    (when file-path
      (if (file-exists-p file-path) file-path
        (slack-url-copy-file image-url file-path
                             :success (lambda ()
                                        (slack-log (format "Success download Image: %s"
                                                           file-path)
                                                   team)))))
    file-path))

(cl-defun slack-user-image (user team &optional (size 32))
  (when user
    (let ((image (slack-user-fetch-image user size team)))
      (when image
        (create-image image nil nil :ascent 80)))))

(defun slack-user-presence (user)
  (plist-get user :presence))

(defun slack-request-set-presence (team &optional presence)
  (unless presence
    (let ((current-presence (slack-user-presence (slack-user--find (oref team self-id)
                                                                   team))))

      (setq presence (or (and (string= current-presence "away") "auto")
                         "away"))
      ))
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-request-set-presence"))))
    (slack-request
     (slack-request-create
      slack-set-presence-url
      team
      :success #'on-success
      :params (list (cons "presence" presence))))))

(defun slack-request-dnd-set-snooze (team time)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-request-dnd-set-snooze")
                    (message "setSnooze: %s" data))))
    (let* ((input (slack-parse-time-string time))
           (num-minutes (and time (/ (- (time-to-seconds input) (time-to-seconds))
                                     60))))
      (unless num-minutes
        (error "Invalid time string %s" time))
      (slack-request
       (slack-request-create
        slack-dnd-set-snooze-url
        team
        :success #'on-success
        :params (list (cons "num_minutes" (format "%s" num-minutes))))))))

(defun slack-request-dnd-end-dnd (team)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-request-dnd-end-dnd")
                    (message "endDnd: %s" data))))
    (slack-request
     (slack-request-create
      slack-dnd-end-dnd-url
      team
      :success #'on-success
      ))))

(defun slack-user-update-dnd-status (user dnd-status)
  (plist-put user :dnd_status dnd-status))

(defun slack-request-dnd-team-info (team &optional after-success)
  (cl-labels
      ((on-success
        (&key data &allow-other-keys)
        (slack-request-handle-error
         (data "slack-request-dnd-team-info")
         (let ((users (plist-get data :users)))
           (oset team users
                 (cl-loop for user in (oref team users)
                          collect (plist-put
                                   user
                                   :dnd_status
                                   (plist-get users
                                              (intern (format ":%s"
                                                              (plist-get user :id)))))))))
        (when (functionp after-success)
          (funcall after-success team))))
    (slack-request
     (slack-request-create
      slack-dnd-team-info-url
      team
      :success #'on-success))))

(provide 'slack-user)
;;; slack-user.el ends here
