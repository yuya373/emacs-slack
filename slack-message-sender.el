;;; slack-message-sender.el --- slack message concern message sending  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
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
(require 'json)
(require 'slack-util)
(require 'slack-room)
(require 'slack-im)
(require 'slack-group)
(require 'slack-message)
(require 'slack-channel)
(require 'slack-conversations)
(require 'slack-buffer)
(require 'slack-usergroup)
(require 'slack-mrkdwn)

(defvar slack-completing-read-function)
(defvar slack-buffer-function)

(defconst slack-channel-mention-regex "\\(<#\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-user-mention-regex "\\(<@\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-usergroup-mention-regex "\\(<!subteam^\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-special-mention-regex "\\(<!\\(here\\|channel\\|everyone\\)>\\)")

(cl-defun slack-message-send-internal (message room team &key (on-success nil) (on-error nil) (payload nil))
  (if (and (slack-channel-p room)
           (not (oref room is-member)))
      (slack-conversations-join
       room team
       #'(lambda (_data) (slack-message-send-internal message
                                                      room
                                                      team)))
    (with-slots (message-id self-id) team
      (let* ((channel-id (oref room id))
             (m (apply #'list
                       (cons "type" "message")
                       (cons "channel" channel-id)
                       (with-temp-buffer
                         (insert message)
                         (slack-create-blocks-from-buffer)))))
        (slack-chat-post-message team (append m payload)
                                 :on-success on-success
                                 :on-error on-error)))))

(cl-defun slack-chat-post-message (team message &key (on-success nil) (on-error nil))
  (cl-labels
      ((success (&key data &allow-other-keys)
                (if (eq t (plist-get data :ok))
                    (and on-success (funcall on-success))
                  (if on-error
                      (funcall on-error data)
                    (error "Failed to post message.  %s"
                           (plist-get data :message))))))
    (slack-request
     (slack-request-create
      "https://slack.com/api/chat.postMessage"
      team
      :type "POST"
      :data (json-encode message)
      :headers (list (cons "Content-Type"
                           "application/json;charset=utf-8"))
      :success #'success))))

(defun slack-message-room-list (team)
  (append (slack-group-names team)
          (slack-im-names team)
          (slack-channel-names team)))

(defun slack-message-embed-channel ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (with-slots (team) buf
        (slack-select-from-list
            ((slack-channel-names team) "Select Channel: ")
            (insert (concat (propertize (format "<#%s>" (oref selected id))
                                        'rear-nonsticky t
                                        'display (format "@%s" (slack-room-name selected team))
                                        'face 'slack-message-mention-face)
                            " "))))))

(defun slack-message-embed-mention ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (with-slots (team) buf
        (let* ((keyworkds (list (list "here" :name "here" :type 'keyword)
                                (list "channel" :name "channel" :type 'keyword)
                                (list "everyone" :name "everyone" :type 'keyword)))
               (usergroups (mapcar #'(lambda (e) (list (oref e handle)
                                                       :name (oref e handle)
                                                       :type 'usergroup))
                                   (cl-remove-if #'slack-usergroup-deleted-p
                                                 (oref team usergroups))))
               (alist (append keyworkds (slack-user-names team) usergroups)))
          (slack-select-from-list
              (alist "Select User: ")
              (cl-case (plist-get selected :type)
                (keyword
                 (insert (concat (propertize (format "<!%s>" (plist-get selected :name))
                                             'rear-nonsticky t
                                             'display (concat "@" (plist-get selected :name))
                                             'face 'slack-message-mention-keyword-face)
                                 " ")))
                (usergroup
                 (insert (concat (propertize (format "<!subteam^%s>" (plist-get selected :id))
                                             'rear-nonsticky t
                                             'display (concat "@" (plist-get selected :name))
                                             'face 'slack-message-mention-keyword-face)
                                 " ")))
                (t
                 (insert (concat (propertize (format "<@%s>" (plist-get selected :id))
                                             'rear-nonsticky t
                                             'display (concat "@" (slack-user--name selected team))
                                             'face 'slack-message-mention-face)
                                 " ")))))))))

(defun slack-mark-mentions ()
  (goto-char (point-min))
  (while (re-search-forward slack-user-mention-regex (point-max) t)
    (slack-mrkdwn-put-block-props (match-beginning 1)
                                  (match-end 1)
                                  (list :type 'user
                                        :user-id (match-string 2))))
  (goto-char (point-min))
  (while (re-search-forward slack-usergroup-mention-regex (point-max) t)
    (slack-mrkdwn-put-block-props (match-beginning 1)
                                  (match-end 1)
                                  (list :type 'usergroup
                                        :usergroup-id (match-string 2))))
  (goto-char (point-min))
  (while (re-search-forward slack-channel-mention-regex (point-max) t)
    (slack-mrkdwn-put-block-props (match-beginning 1)
                                  (match-end 1)
                                  (list :type 'channel
                                        :channel-id (match-string 2))))
  (goto-char (point-min))
  (while (re-search-forward slack-special-mention-regex (point-max) t)
    (slack-mrkdwn-put-block-props (match-beginning 1)
                                  (match-end 1)
                                  (list :type 'broadcast
                                        :range (match-string 2)))))

(defun slack-mark-emojis ()
  (goto-char (point-min))
  (while (re-search-forward ":\\([a-z0-9_-]+\\):" (point-max) t)
    (slack-mrkdwn-put-block-props (match-beginning 0)
                                  (match-end 0)
                                  (list :type 'emoji
                                        :name (match-string 1)))))

(defun slack-mark-links ()
  (goto-char (point-min))
  (let ((regex (regexp-opt thing-at-point-uri-schemes)))
    (while (re-search-forward regex (point-max) t)
      (let ((bounds (bounds-of-thing-at-point 'url)))
        (when bounds
          (slack-mrkdwn-put-block-props (car bounds)
                                        (cdr bounds)
                                        (list :type 'link
                                              :url (buffer-substring-no-properties (car bounds)
                                                                                   (cdr bounds)))))))))

(defun slack-create-blocks-from-buffer ()
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-mrkdwn-add-face)
    (slack-mark-mentions)
    (slack-mark-emojis)
    (slack-mark-links)
    (cl-labels ((with-ranges (ranges cb)
                             (let ((str (mapconcat #'(lambda (range)
                                                       (buffer-substring-no-properties
                                                        (car range)
                                                        (cdr range)))
                                                   (reverse ranges)
                                                   "\n")))
                               (with-temp-buffer
                                 (insert str)
                                 (slack-mrkdwn-add-face)
                                 (funcall cb))))
                (create-elements-from-ranges (ranges)

                                             (when (< 0 (length ranges))
                                               (with-ranges ranges #'(lambda ()
                                                                       (create-elements
                                                                        (point-min)
                                                                        (point-max))))))
                (create-section-elements-from-ranges (ranges)
                                                     (when (< 0 (length ranges))
                                                       (with-ranges ranges #'(lambda ()
                                                                               (create-section-elements
                                                                                (point-min)
                                                                                (point-max))))))
                (create-section-elements (start end)
                                         (let* ((cur-point start)
                                                (elements nil)
                                                (section-elements nil)
                                                (preformatted-ranges nil)
                                                (blockquote-ranges nil)
                                                (list-style nil)
                                                (list-indent nil)
                                                (list-ranges nil))
                                           (cl-labels ((commit-block (type block-elements &rest props)
                                                                     (when (< 0 (length block-elements))
                                                                       (let ((e (list (cons "type" type)
                                                                                      (cons "elements" block-elements))))
                                                                         (dolist (prop props)
                                                                           (push prop e))
                                                                         (push e elements))))
                                                       (commit-section-block ()
                                                                             (when (commit-block "rich_text_section"
                                                                                                 (reverse section-elements))
                                                                               (setq section-elements nil)))
                                                       (commit-preformatted-block ()
                                                                                  (when (commit-block "rich_text_preformatted"
                                                                                                      (create-elements-from-ranges
                                                                                                       preformatted-ranges))
                                                                                    (setq preformatted-ranges nil)))
                                                       (commit-blockquote-block ()

                                                                                (when (commit-block "rich_text_quote"
                                                                                                    (create-elements-from-ranges
                                                                                                     blockquote-ranges))
                                                                                  (setq blockquote-ranges nil)))
                                                       (commit-list-block ()
                                                                          (when (commit-block "rich_text_list"
                                                                                              (mapcan #'(lambda (range)
                                                                                                          (create-section-elements-from-ranges
                                                                                                           (list range)))
                                                                                                      (reverse list-ranges))
                                                                                              (cons "style" list-style)
                                                                                              (cons "indent" list-indent))
                                                                            (setq list-ranges nil)
                                                                            (setq list-style nil)
                                                                            (setq list-indent nil))))
                                             (while (and cur-point (< cur-point end))
                                               (let* ((block-props (get-text-property cur-point 'slack-section-block-props))
                                                      (section-type (and block-props (plist-get block-props :section-type)))
                                                      (end (or (next-single-property-change cur-point 'slack-section-block-props)
                                                               end)))
                                                 (cl-case section-type
                                                   (code-block (progn
                                                                 (commit-section-block)
                                                                 (commit-blockquote-block)
                                                                 (commit-list-block)
                                                                 (push (cons (plist-get block-props :element-beg)
                                                                             (plist-get block-props :element-end))
                                                                       preformatted-ranges)))
                                                   (blockquote (progn
                                                                 (commit-section-block)
                                                                 (commit-preformatted-block)
                                                                 (commit-list-block)
                                                                 (push (cons (plist-get block-props :element-beg)
                                                                             (plist-get block-props :element-end))
                                                                       blockquote-ranges)
                                                                 ;; Skip newline
                                                                 (setq end (1+ end))
                                                                 ))
                                                   (list (progn
                                                           (commit-section-block)
                                                           (commit-preformatted-block)
                                                           (commit-blockquote-block)
                                                           (push (cons (plist-get block-props :element-beg)
                                                                       (plist-get block-props :element-end))
                                                                 list-ranges)
                                                           (setq list-style (plist-get block-props :style))
                                                           (setq list-indent (plist-get block-props :indent)))
                                                         ;; Skip newline
                                                         (setq end (1+ end)))
                                                   (t (progn
                                                        (commit-preformatted-block)
                                                        (commit-blockquote-block)
                                                        (commit-list-block)
                                                        (dolist (e (create-elements cur-point end))
                                                          (push e section-elements)))))
                                                 (setq cur-point end)))
                                             (commit-section-block)
                                             (commit-preformatted-block)
                                             (commit-blockquote-block)
                                             (commit-list-block))
                                           (reverse elements)))
                (create-elements (start end)
                                 (save-excursion
                                   (save-restriction
                                     (narrow-to-region start end)
                                     (let* ((cur-point (point-min))
                                            (elements nil))
                                       (cl-labels ((create-text-element (text &optional style)
                                                                        (cl-remove-if #'null
                                                                                      (list (cons "type" "text")
                                                                                            (cons "text" text)
                                                                                            (when style
                                                                                              (cons "style" style))))))
                                         (while (and cur-point (< cur-point (point-max)))
                                           (let* ((block-props (get-text-property cur-point 'slack-block-props))
                                                  (block-type (and block-props (plist-get block-props :type)))
                                                  (block-text (and block-props (plist-get block-props :text)))
                                                  (next-change-point (or (next-single-property-change cur-point 'slack-block-props)
                                                                         (point-max)))
                                                  (element (progn
                                                             (cl-case block-type
                                                               (bold (create-text-element block-text (list (cons "bold" t))))
                                                               (italic (create-text-element block-text (list (cons "italic" t))))
                                                               (strike (create-text-element block-text (list (cons "strike" t))))
                                                               (code (create-text-element block-text (list (cons "code" t))))
                                                               (text (create-text-element block-text))
                                                               (user (list (cons "type" "user")
                                                                           (cons "user_id" (plist-get block-props :user-id))))
                                                               (usergroup (list (cons "type" "usergroup")
                                                                                (cons "usergroup_id" (plist-get block-props :usergroup-id))))
                                                               (channel (list (cons "type" "channel")
                                                                              (cons "channel_id" (plist-get block-props :channel-id))))
                                                               (broadcast (list (cons "type" "broadcast")
                                                                                (cons "range" (plist-get block-props :range))))
                                                               (emoji (list (cons "type" "emoji")
                                                                            (cons "name" (plist-get block-props :name))))
                                                               (link (list (cons "type" "link")
                                                                           (cons "url" (plist-get block-props :url))))
                                                               (t (create-text-element
                                                                   (buffer-substring-no-properties cur-point
                                                                                                   next-change-point)))))))
                                             (when element
                                               (push element elements))
                                             (let* ((n (min (or next-change-point end))))
                                               (setq cur-point n)))))

                                       (reverse elements))))))
      (let ((elements (create-section-elements (point-min) (point-max))))
        (let ((blocks (list (cons "blocks" (list (list (cons "type" "rich_text")
                                                       (cons "elements" elements)))))))
          ;; (let ((buf (get-buffer-create "emacs-slack blocks")))
          ;;   (with-current-buffer buf
          ;;     (delete-region (point-min) (point-max))
          ;;     (insert (json-encode-list blocks))
          ;;     (json-mode)
          ;;     (json-pretty-print-buffer))
          ;;   (switch-to-buffer-other-window buf))
          blocks)))))

(provide 'slack-message-sender)
;;; slack-message-sender.el ends here
