;;; slack-vip.el --- VIP/priority user support          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; Keywords: tools

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

;; Slack "priority contacts" (VIP) support.  VIP users are rendered with
;; `slack-user-vip-face' and can be filtered, added and removed.
;;
;; The VIP set lives in the team's `priority-users' slot.  It is populated
;; from three sources:
;;   - the user plist field named by `slack-user-vip-field', synced when
;;     users are cached (see `slack-vip-sync-user');
;;   - the `users.priority.add' / `users.priority.remove' calls;
;;   - `slack-vip-list-update', which fetches `users.priority.list'
;;     (best-effort, since the endpoint is undocumented).

;;; Code:

(require 'eieio)
(require 'subr-x)
(require 'slack-util)
(require 'slack-team)
(require 'slack-request)

(declare-function slack-user--refresh-visible-buffers "slack-user")
(declare-function slack-user--find "slack-user")
(declare-function slack-user--name "slack-user")
(declare-function slack-user-names "slack-user")
(declare-function slack-create-user-profile-buffer "slack-user-profile-buffer")
(declare-function slack-buffer-display "slack-buffer")
(defvar slack-completing-read-function)

(defconst slack-user-priority-add-url
  "https://slack.com/api/users.priority.add")
(defconst slack-user-priority-remove-url
  "https://slack.com/api/users.priority.remove")
(defconst slack-user-priority-list-url
  "https://slack.com/api/users.priority.list")

(defcustom slack-user-vip-field :is_priority
  "User plist key that marks a user as VIP/priority.
Slack does not document this field; if your `users.info' responses use
a different key, customize this to match.  When the key is present and
truthy on a cached user, the user is treated as VIP."
  :type 'symbol
  :group 'slack)

(defface slack-user-vip-face
  '((t (:weight bold :slant italic :background "#b58900"
               :foreground "#073642")))
  "Face used for the `[VIP]' badge label appended next to VIP user
names (e.g. in message headers, user lists and profiles) and to
highlight @mentions of VIP users.  It is a golden badge with cursive
text so the tag is clearly distinguishable from the name itself."
  :group 'slack)

(defun slack-vip--field-value (user)
  "Return the value of the VIP field for USER plist, or nil."
  (and user
       (plist-get user slack-user-vip-field)))

(defun slack-vip--set (team user-id value)
  "Add USER-ID to TEAM's priority set when VALUE is non-nil, remove otherwise."
  (let ((table (slack-team-priority-users team)))
    (if value
        (puthash user-id t table)
      (remhash user-id table))))

(cl-defun slack-vip-sync-user (user team)
  "Sync TEAM's priority set from USER plist's VIP field.
When the field is present and truthy the user is added; when present
and explicitly nil it is removed; when absent the set is left
untouched (so explicit add/remove and list fetch are not clobbered)."
  (when (and user team (plist-get user :id))
    (let ((field (plist-get user slack-user-vip-field)))
      (when (plist-member user slack-user-vip-field)
        (slack-vip--set team (plist-get user :id) field)))))

(defun slack-vip-sync-users (team users)
  "Sync TEAM's priority set from a list of USER plists.
See `slack-vip-sync-user'."
  (dolist (user users)
    (slack-vip-sync-user user team)))

(defun slack-user-vip-p (user team)
  "Return non-nil if USER (a plist or id string) is a VIP in TEAM.
A user is VIP when its id is in TEAM's `priority-users' set or when its
plist has a truthy `slack-user-vip-field'.  When USER is an id string,
the cached user plist (if any) is consulted for the field too."
  (when (and user team)
    (if (stringp user)
        (slack-user-vip-p-id user team)
      (let ((id (plist-get user :id)))
        (or (and id (gethash id (slack-team-priority-users team)))
            (eq t (slack-vip--field-value user)))))))

(defun slack-user-vip-p-id (user-id team)
  "Return non-nil if USER-ID is a VIP in TEAM."
  (and user-id team
       (or (gethash user-id (slack-team-priority-users team))
           (slack-user-vip-p (gethash user-id (oref team users)) team))))

(defun slack-user-vip-label ()
  "Return the propertized ` [VIP]' badge label.
The leading space separates it from the preceding name."
  (propertize " [VIP]" 'face 'slack-user-vip-face))

(defun slack-user-vip-display-name (user team)
  "Return USER's name followed by the `[VIP]' badge when VIP.
USER is a plist.  When the user is not VIP, return the plain name."
  (let ((name (slack-user--name user team)))
    (if (slack-user-vip-p user team)
        (concat name (slack-user-vip-label))
      name)))

(defun slack-user-vip-propertize-name (name user-id team)
  "Return NAME followed by the `[VIP]' badge when USER-ID is a VIP in TEAM.
The name itself is left unchanged; only the badge is styled.  Use this
from render sites that already have the name string and an id."
  (if (slack-user-vip-p-id user-id team)
      (concat name (slack-user-vip-label))
    name))

(defun slack-user-vip-filter (users)
  "Filter USERS, keeping only VIP users.
Intended as the `:filter' argument of `slack-user-names' /
`slack-user-name-alist'.  The team is not available here, so membership
is decided from each user's `slack-user-vip-field' only; users added via
`users.priority.add' but whose plist lacks the field are not kept.  For
team-aware filtering use `slack-user-vip-names'."
  (cl-remove-if-not
   (lambda (user) (eq t (slack-vip--field-value user)))
   users))

(defun slack-user-vip-names (team)
  "Return VIP users of TEAM as an alist (\"name\" . user)."
  (let ((users (cl-remove-if #'slack-user-hidden-p
                             (slack-team-users team))))
    (mapcar (lambda (u) (cons (slack-user--name u team) u))
            (cl-remove-if-not
             (lambda (u) (slack-user-vip-p u team))
             users))))

(cl-defun slack-vip-add (user-id team &key after-success)
  "Add VIP/priority status to USER-ID in TEAM via `users.priority.add'."
  (slack-vip--priority-request
   slack-user-priority-add-url user-id team
   (lambda ()
     (slack-vip--set team user-id t)
     (slack-vip--sync-cached-user-field team user-id t)
     (slack-user--refresh-visible-buffers user-id team)
     (when (functionp after-success)
       (funcall after-success)))))

(cl-defun slack-vip-remove (user-id team &key after-success)
  "Remove VIP/priority status from USER-ID in TEAM via `users.priority.remove'."
  (slack-vip--priority-request
   slack-user-priority-remove-url user-id team
   (lambda ()
     (slack-vip--set team user-id nil)
     (slack-vip--sync-cached-user-field team user-id nil)
     (slack-user--refresh-visible-buffers user-id team)
     (when (functionp after-success)
       (funcall after-success)))))

(defun slack-vip--sync-cached-user-field (team user-id value)
  "Set the cached USER-ID plist's VIP field to VALUE (t or nil) in TEAM.
No-op when the user is not cached."
  (when-let* ((user (gethash user-id (oref team users))))
    (let ((updated (plist-put (copy-sequence user) slack-user-vip-field value)))
      (puthash user-id updated (oref team users)))))

(defun slack-vip--priority-request (url user-id team on-success)
  "POST to priority URL with USER-ID for TEAM, calling ON-SUCCESS on ok."
  (cl-labels
      ((success (&key data &allow-other-keys)
         (if (eq (plist-get data :ok) :json-false)
             (slack-log (format "slack-vip: %s failed: %s"
                                 url (plist-get data :error))
                        team :level 'warn)
           (when (functionp on-success)
             (funcall on-success)))))
    (slack-request
     (slack-request-create
      url
      team
      :type "POST"
      :params (list (cons "user" user-id))
      :success #'success
      :no-retry t))))

(defun slack-vip-list-update (&optional team)
  "Fetch TEAM's priority user list (`users.priority.list') and populate
the `priority-users' set.  The endpoint is undocumented and best-effort:
on failure the set is left untouched and a warning is logged."
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (&key data &allow-other-keys)
           (if (eq (plist-get data :ok) :json-false)
               (slack-log (format "slack-vip-list-update failed: %s"
                                  (plist-get data :error))
                          team :level 'warn)
             (let ((ids (or (plist-get data :priority_users)
                            (plist-get data :users)
                            (plist-get data :user_ids))))
               (dolist (id ids)
                 (when (stringp id)
                   (slack-vip--set team id t)))
               (slack-log (format "VIP list updated: %s user(s)"
                                  (length ids))
                          team :level 'info)))))
      (slack-request
       (slack-request-create
        slack-user-priority-list-url
        team
        :type "GET"
        :success #'success
        :no-retry t)))))

;;;###autoload
(defun slack-user-vip-add ()
  "Interactively add VIP/priority status to a user."
  (interactive)
  (let* ((team (slack-team-select))
         (user (slack-vip--select-user team "Add VIP to user: ")))
    (when user
      (slack-vip-add (plist-get user :id) team
                     :after-success
                     (lambda ()
                       (slack-log (format "Added VIP: %s"
                                          (slack-user--name user team))
                                  team :level 'info))))))

;;;###autoload
(defun slack-user-vip-remove ()
  "Interactively remove VIP/priority status from a known VIP user.
Completion is restricted to users that are currently VIP."
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-user-vip-names team)))
    (if (null alist)
        (message "No VIP users to remove.")
      (let ((selected (cdr (assoc (funcall slack-completing-read-function
                                          "Remove VIP from user: "
                                          (mapcar #'car alist))
                                  alist))))
        (when selected
          (slack-vip-remove (plist-get selected :id) team
                            :after-success
                            (lambda ()
                              (slack-log (format "Removed VIP: %s"
                                                 (slack-user--name selected team))
                                         team :level 'info))))))))

;;;###autoload
(defun slack-user-select-vip ()
  "Select a VIP user and display their profile."
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-user-vip-names team)))
    (if (null alist)
        (message "No VIP users.  Run `slack-vip-list-update' or add one.")
      (let ((selected (cdr (assoc (funcall slack-completing-read-function
                                            "Select VIP User: "
                                            (mapcar #'car alist))
                                  alist))))
        (when selected
          (slack-buffer-display
           (slack-create-user-profile-buffer team (plist-get selected :id))))))))

(defun slack-vip--select-user (team prompt)
  "Read a user from TEAM with PROMPT, returning the user plist."
  (let* ((alist (slack-user-names team))
         (name (funcall slack-completing-read-function prompt
                        (mapcar #'car alist)))
         (user (cdr (assoc name alist))))
    (when (string= name "")
      (setq user nil))
    user))

(provide 'slack-vip)
;;; slack-vip.el ends here
