;;; slack-org-link.el --- Org links to emacs-slack messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; Keywords: slack, org

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

;; Org links to emacs-slack messages and rooms.
;;
;; The canonical link address is a Slack permalink:
;;
;;   [[emacs-slack:https://myteam.slack.com/archives/C099W16KZ/p1730182493679269?cid=C099W16KZ][Slack message in #general]]
;;
;; Because the address is a permalink, following the link opens the
;; message in emacs-slack and, when that is not possible (e.g. the team
;; is not connected), falls back to the system browser.
;;
;; Links stored by the ol-emacs-slack package are still understood: the
;; TEAMID[&|]ROOMID[&|]ts:TS format is converted to permalink info by
;; `slack-org-link-to-info'.  See `slack-org-alert.el' for capturing
;; message alerts as Org TODO headings.

;;; Code:

(require 'ol)
(require 'browse-url)
(require 'dash)
(require 's)
(require 'eieio)
(require 'cl-lib)
(require 'slack-util)
(require 'slack-team)
(require 'slack-room)
(require 'slack-buffer)
(require 'slack-message)
(require 'slack-message-buffer)
(require 'slack-room-buffer)

(defvar slack-completing-read-function)

(defgroup slack-org nil
  "Org mode integration for emacs-slack."
  :prefix "slack-org-"
  :group 'slack)

(defcustom slack-org-open-in-browser-fallback t
  "If non-nil, open the permalink in a browser when emacs-slack cannot.
emacs-slack cannot open a link when its team is not connected or its
room is not loaded.  The browser always can."
  :type 'boolean
  :group 'slack-org)

(org-link-set-parameters "emacs-slack"
                         :follow #'slack-org-follow-link
                         :export #'slack-org-export
                         :store #'slack-org-store-link
                         :complete #'slack-org-complete-link)

(defconst slack-org--permalink-regexp
  "^https://\\([^./]+\\)\\.slack\\.com/archives/\\([^/?]+\\)"
  "Regexp matching team domain and room id in a Slack permalink.")

(defun slack-org--permalink-to-info (permalink)
  "Like `slack-permalink-to-info', but room-level permalinks work too.
A room-level permalink has no /p<timestamp> part and produces info
with a nil :ts."
  (if (string-match-p "/p[0-9]" permalink)
      (slack-permalink-to-info permalink)
    (when (string-match slack-org--permalink-regexp permalink)
      (list :team-domain (match-string 1 permalink)
            :room-id (match-string 2 permalink)
            :ts nil
            :thread-ts nil))))

(defun slack-org--old-format-to-info (link)
  "Convert LINK of the form TEAMID[&|]ROOMID[&|]ts:TS to permalink info.
This is the format the ol-emacs-slack package used to store."
  (let* ((parts (s-split "[&|]" link))
         (team (slack-team-find (cl-first parts)))
         (room-id (cl-second parts))
         (ts (when (cl-third parts)
               (cl-second (s-split ":" (cl-third parts)))))
         (thread-ts
          (if (and team room-id ts)
              (or (-some-> (slack-room-find room-id team)
                           (slack-room-find-message ts)
                           slack-thread-ts)
                  ts)
            ts)))
    (when team
      (list :team-domain (slack-team-domain team)
            :room-id room-id
            :ts ts
            :thread-ts thread-ts))))

(defun slack-org-link-to-info (link)
  "Convert LINK of any supported format to permalink info.
The result is a plist like (:team-domain ... :room-id ... :ts ...
:thread-ts ...), or nil when LINK cannot be interpreted.
Supported formats are permalinks (including room-level ones) and
the old TEAMID[&|]ROOMID[&|]ts:TS format."
  (cond
   ((string-match-p "^https://" link) (slack-org--permalink-to-info link))
   ((string-match-p "[&|]" link) (slack-org--old-format-to-info link))))

(defun slack-org-info-to-permalink (info)
  "Convert INFO to a permalink; room-level when INFO has no :ts."
  (if (plist-get info :ts)
      (slack-info-to-permalink info)
    (format "https://%s.slack.com/archives/%s/"
            (plist-get info :team-domain)
            (plist-get info :room-id))))

(defun slack-org--open-message (permalink)
  "Open PERMALINK in emacs-slack, or in the browser as a fallback."
  (condition-case err
      (slack-open-url permalink)
    (error
     (if slack-org-open-in-browser-fallback
         (progn
           (message "slack-org: could not open in emacs-slack (%s); opening %s in browser"
                    (error-message-string err) permalink)
           (browse-url permalink))
       (signal (car err) (cdr err))))))

(defun slack-org-follow-link (link)
  "Follow LINK to its Slack message.
Supports permalinks and the old TEAMID[&|]ROOMID[&|]ts:TS format;
the latter is converted to a permalink first because alerts only
arrive from connected teams.  When emacs-slack cannot open the
link (room not loaded, or a room-level permalink), fall back to
the browser when `slack-org-open-in-browser-fallback' is non-nil."
  (if (string-match-p "^https://" link)
      ;; permalink: open it directly, no round-trip through info
      (slack-org--open-message link)
    ;; old internal format: resolve to a permalink first
    (-if-let (info (slack-org-link-to-info link))
        (slack-org--open-message (slack-org-info-to-permalink info))
      (user-error "slack-org: cannot interpret link %s (is the team connected?)"
                  link))))

(defun slack-org--store-message-link ()
  "Store a permalink link for the message at point."
  (let* ((buf slack-current-buffer)
         (team (slack-buffer-team buf))
         (room (slack-buffer-room buf))
         (room-name (slack-room-name room team))
         (ts (org-get-at-bol 'ts))
         (formatted-ts (org-get-at-bol 'lui-formatted-time-stamp))
         (thread-ts (-when-let (message (and room ts
                                             (slack-room-find-message room ts)))
                      (slack-thread-ts message)))
         (info (list :team-domain (slack-team-domain team)
                     :room-id (oref room id)
                     :ts ts
                     :thread-ts (or thread-ts ts)))
         (link-path (if (plist-get info :team-domain)
                        (slack-org-info-to-permalink info)
                      ;; graceful degradation: the adapter understands
                      ;; this format too
                      (format "%s|%s|%s" (slack-team-id team) (oref room id)
                              (if ts (concat "ts:" ts) ""))))
         (description (concat
                       "Slack message in #" room-name
                       (if formatted-ts (format " at %s" formatted-ts) "")
                       (if ts
                           (concat ": " (s-trim (buffer-substring-no-properties
                                                 (line-beginning-position)
                                                 (line-end-position))))
                         ""))))
    (org-link-store-props
     :type "emacs-slack"
     :link (concat "emacs-slack:" link-path)
     :description description)))

(defun slack-org--store-permalink-property ()
  "Store a link from the `permalink' text property at point.
Works in buffers like search results and pinned items, where each
line carries the permalink of its message."
  (-when-let (permalink (get-text-property (point) 'permalink))
    (org-link-store-props
     :type "emacs-slack"
     :link (concat "emacs-slack:" permalink)
     :description (concat "Slack message: "
                          (s-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))))

(defun slack-org-store-link ()
  "Store a link to the Slack message at point."
  (cond
   ((memq major-mode '(slack-message-buffer-mode
                       slack-thread-message-buffer-mode))
    (slack-org--store-message-link))
   ((memq major-mode '(slack-search-result-buffer-mode
                       slack-pinned-items-buffer-mode))
    (slack-org--store-permalink-property))))

(defun slack-org-link-at-point ()
  "Return the `emacs-slack:' Org link for the message at point.
Useful in `org-capture-templates': insert a function entry that
calls this and the captured heading links back to the message."
  (let ((org-store-link-plist nil))
    (unless (slack-org-store-link)
      (user-error "No Slack message at point"))
    (plist-get org-store-link-plist :link)))

(defun slack-org-export (link description backend &optional _info)
  "Export an emacs-slack LINK as a plain https permalink."
  (-if-let (info (ignore-errors (slack-org-link-to-info link)))
      (let ((permalink (slack-org-info-to-permalink info)))
        (pcase backend
          (`html (format "<a href=\"%s\">%s</a>"
                         permalink (or description permalink)))
          (`md (format "[%s](%s)" (or description permalink) permalink))
          (_ (or description permalink))))
    (or description link)))

(defun slack-org--room-completions ()
  "Return an alist of (label . permalink) for connected teams' rooms."
  (let ((candidates '()))
    (dolist (team (slack-team-connected-list))
      (dolist (room (append (slack-team-channels team)
                            (slack-team-groups team)
                            (slack-team-ims team)))
        (unless (slack-room-hidden-p room)
          (push (cons (format "%s - %s"
                              (slack-team-name team)
                              (slack-room-name room team))
                      (format "https://%s.slack.com/archives/%s/"
                              (slack-team-domain team)
                              (oref room id)))
                candidates))))
    (nreverse candidates)))

(defun slack-org-complete-link ()
  "Complete an emacs-slack room link over the connected teams' rooms."
  (-if-let* ((candidates (slack-org--room-completions))
             (selected (funcall slack-completing-read-function
                                "Slack room: "
                                (mapcar #'car candidates))))
      (concat "emacs-slack:"
              (cdr (cl-assoc selected candidates :test #'string=)))
    (user-error "slack-org: no connected Slack team")))

;;; Obsolete names from the ol-emacs-slack package

(define-obsolete-function-alias 'ol/slack-follow-link 'slack-org-follow-link "0.0.4")
(define-obsolete-function-alias 'ol/slack-store-link 'slack-org-store-link "0.0.4")
(define-obsolete-function-alias 'ol/slack-export 'slack-org-export "0.0.4")

(provide 'slack-org-link)
;;; slack-org-link.el ends here
