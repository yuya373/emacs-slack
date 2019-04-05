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

(require 'eieio)
(require 'slack-util)
(require 'slack-request)
(require 'slack-emoji)
(require 'slack-dnd-status)
(require 'slack-bot)

(defvar slack-completing-read-function)

(defconst slack-dnd-end-dnd-url "https://slack.com/api/dnd.endDnd")
(defconst slack-dnd-set-snooze-url "https://slack.com/api/dnd.setSnooze")
(defconst slack-set-presence-url "https://slack.com/api/users.setPresence")
(defconst slack-user-info-url "https://slack.com/api/users.info")
(defconst slack-user-list-url "https://slack.com/api/users.list")
(defconst slack-user-profile-set-url "https://slack.com/api/users.profile.set")
(defvar slack-current-user-id nil)

(defcustom slack-dnd-sign "Z"
  "Used to indicate user is dnd status."
  :group 'slack
  :type 'string)

(defface slack-user-dnd-face
  '((t (:foreground "#2aa198" :weight bold)))
  "Used to `slack-user-dnd-sign'"
  :group 'slack)

(defcustom slack-user-active-string "*"
  "If user is active, use this string with `slack-user-active-face'."
  :type 'string
  :group 'slack)

(defface slack-user-active-face
  '((t (:foreground "#2aa198" :weight bold)))
  "Used to `slack-user-active-string'"
  :group 'slack)

(cl-defmethod slack-user-find ((id string) team)
  (gethash id (oref team users)))
;; TODO remove this. use `slack-user-find'
(defun slack-user--find (id team)
  "Find user by ID from TEAM."
  (gethash id (oref team users)))

(defun slack-user-id (user)
  "Get id of USER."
  (when user
    (plist-get user :id)))

(defun slack-user-name (id team)
  "Find user by ID in TEAM, then return user's name."
  (slack-if-let* ((user (slack-user--find id team)))
      (slack-user--name user team)))

(defun slack-user--name (user team)
  (let ((real-name (slack-user-real-name user))
        (display-name (slack-user-display-name user)))
    (if (or (oref team full-and-display-names)
            (slack-string-blankp display-name))
        real-name
      display-name)))

(defun slack-user-real-name (user)
  (slack-if-let* ((profile (slack-user-profile user)))
      (plist-get profile :real_name_normalized)))

(defun slack-user-display-name (user)
  (slack-if-let* ((profile (slack-user-profile user)))
      (plist-get profile :display_name_normalized)))

(defun slack-user-label (user team)
  (format "%s%s %s"
          (or (slack-user-dnd-status-to-string user team) " ")
          (or (slack-user-presence-to-string user team) " ")
          (slack-user--name user team)))

(defun slack-user--status (user)
  (let* ((profile (and user (plist-get user :profile)))
         (emoji (and profile (plist-get profile :status_emoji)))
         (text (and profile (plist-get profile :status_text))))
    (mapconcat #'identity (cl-remove-if #'null (list emoji text))
               " ")))

(defun slack-user-status (id team)
  "Find user by ID in TEAM, then return user's status in string."
  (let* ((user (slack-user--find id team)))
    (slack-user--status user)))

(defun slack-user-names (team &optional filter)
  "Return all users as alist (\"user-name\" . user) in TEAM."
  (let ((users (cl-remove-if #'slack-user-hidden-p
                             (slack-team-users team))))
    (mapcar (lambda (u) (cons (slack-user--name u team) u))
            (if (functionp filter)
                (funcall filter users)
              users))))

(defun slack-user-dnd-in-range-p (user team)
  (slack-if-let* ((statuses (oref team dnd-status))
                  (status (gethash (plist-get user :id)
                                   statuses)))
      (slack-dnd-in-range-p status)))

(defun slack-user-dnd-status-to-string (user team)
  (if (slack-user-dnd-in-range-p user team)
      (propertize slack-dnd-sign
                  'face 'slack-user-dnd-face)
    nil))

(defun slack-user-presence-to-string (user team)
  (slack-if-let* ((statuses (oref team presence))
                  (presence (gethash (plist-get user :id)
                                     statuses)))
      (if (string= presence "active")
          (propertize slack-user-active-string
                      'face 'slack-user-active-face))))

(defun slack-user-set-status ()
  (interactive)
  (let* ((team (slack-team-select))
         (emoji (slack-select-emoji team))
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

(defun slack-user-profile (user)
  (plist-get user :profile))

(defun slack-user-fname (user)
  (plist-get (slack-user-profile user) :first_name))

(defun slack-user-lname (user)
  (plist-get (slack-user-profile user) :last_name))

(defun slack-user-header (user team)
  (let* ((real-name (slack-user-real-name user))
         (display-name (slack-user-display-name user))
         )
    (format "%s%s"
            (if (oref team full-and-display-names)
                (format "%s - " real-name)
              (if (slack-string-blankp display-name)
                  ""
                (format "%s - " display-name)))
            (if (oref team full-and-display-names)
                display-name
              real-name))))

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


(defun slack-user-self-p (user-id team)
  (string= user-id (oref team self-id)))

(defun slack-user-name-alist (team &key filter)
  (let ((users (slack-team-users team)))
    (mapcar #'(lambda (e) (cons (slack-user-label e team) e))
            (if filter (funcall filter users)
              users))))

(defun slack-user-hidden-p (user)
  (not (eq (plist-get user :deleted) :json-false)))

(defun slack--user-select (team)
  (slack-select-from-list ((slack-user-names team) "Select User: ")))

(cl-defun slack-users-info-request (user--ids team &key after-success)
  (let ((bot-ids nil)
        (user-ids nil))
    (cl-loop for id in user--ids
             do (if (string-prefix-p "B" id)
                    (push id bot-ids)
                  (push id user-ids)))
    (if bot-ids
        (slack-bots-info-request bot-ids
                                 team
                                 #'(lambda () (slack-user-info-request user-ids
                                                                       team
                                                                       :after-success after-success)))
      (let* ((batch-size 30)
             (iter-count (ceiling (/ (length user-ids) (float batch-size))))
             (queue nil))
        (cl-loop for i from 0 to (1- iter-count)
                 do (push (cl-subseq user-ids
                                     (* i batch-size)
                                     (min (+ (* i batch-size) batch-size)
                                          (length user-ids)))
                          queue))
        (setq queue (reverse queue))
        (cl-labels
            ((on-success
              (&key data &allow-other-keys)
              (slack-request-handle-error
               (data "slack-users-info-request")
               (let* ((users (plist-get data :users)))
                 (slack-team-set-users team users)))
              (if (< 0 (length queue))
                  (progn
                    (slack-log (format "Fetching users... [%s/%s]"
                                       (* batch-size (- iter-count (length queue)))
                                       (length user-ids))
                               team :level 'info)
                    (request (pop queue)))
                (when (functionp after-success)
                  (funcall after-success))))
             (request (user-ids)
                      (slack-request
                       (slack-request-create
                        slack-user-info-url
                        team
                        :params (list (cons "users"
                                            (mapconcat #'identity user-ids ",")))
                        :success #'on-success))))
          (request (pop queue)))))))

(cl-defun slack-user-info-request (user-id team &key after-success)
  (cond
   ((not (< 0 (length user-id)))
    (when (functionp after-success) (funcall after-success)))
   ((listp user-id)
    (slack-users-info-request user-id team :after-success after-success))
   ((string-prefix-p "B" user-id)
    (slack-bot-info-request user-id team after-success))
   (t (cl-labels
          ((on-success
            (&key data &allow-other-keys)
            (slack-request-handle-error
             (data "slack-user-info-request")
             (let ((user (plist-get data :user)))
               (slack-team-set-users team (list user))))
            (when (functionp after-success)
              (funcall after-success))))
        (slack-request
         (slack-request-create
          slack-user-info-url
          team
          :params (list (cons "user" user-id))
          :success #'on-success))))))

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

(defun slack-user-presence (user team)
  (gethash (plist-get user :id)
           (oref team presence)))

(defun slack-request-set-presence (team &optional presence)
  (unless presence
    (let ((current-presence (gethash (oref team self-id)
                                     (oref team presence)
                                     "")))

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

(defun slack-user-equal-p (a b)
  (string= (plist-get a :id) (plist-get b :id)))

(defalias 'slack-bot-list-update 'slack-user-list-update)
(defun slack-user-list-update (&optional team)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((on-list-update
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-im-list-update")
           (let* ((members (plist-get data :members))
                  (response_metadata (plist-get data
                                                :response_metadata))
                  (next-cursor (and response_metadata
                                    (plist-get response_metadata
                                               :next_cursor))))
             (slack-team-set-users team members)

             (if (and next-cursor (< 0 (length next-cursor)))
                 (request next-cursor)
               (progn
                 (slack-log "Slack User List Updated"
                            team :level 'info))))))
         (request (&optional next-cursor)
                  (slack-request
                   (slack-request-create
                    slack-user-list-url
                    team
                    :params (list (cons "limit" "1000")
                                  (and next-cursor
                                       (cons "cursor" next-cursor)))
                    :success #'on-list-update))))
      (request))))

(provide 'slack-user)
;;; slack-user.el ends here
