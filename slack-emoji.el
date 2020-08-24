;;; slack-emoji.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-request)
(require 'slack-image)

(declare-function emojify-get-emoji "emojify")
(declare-function emojify-image-dir "emojify")
(declare-function emojify-create-emojify-emojis "emojify")
(defvar emojify-user-emojis)
(defvar slack-current-buffer)

(defconst slack-emoji-list "https://slack.com/api/emoji.list")

;; [How can I get the FULL list of slack emoji through API? - Stack Overflow](https://stackoverflow.com/a/39654939)
(defconst slack-emoji-master-data-url
  "https://raw.githubusercontent.com/iamcal/emoji-data/master/emoji.json")
(defvar slack-emoji-master (make-hash-table :test 'equal :size 1600))

(defun slack-download-emoji (team after-success)
  (if (require 'emojify nil t)
      (cl-labels
          ((handle-alias (name emojis)
                         (let* ((raw-url (plist-get emojis name))
                                (alias (if (string-prefix-p "alias:" raw-url)
                                           (intern (format ":%s" (cadr (split-string raw-url ":")))))))
                           (or
                            (and (not raw-url) (handle-alias (intern ":slack") emojis)) ;some aliases are b0rked
                            (and (string-prefix-p "alias:" raw-url) ;recursive alias
                                 (handle-alias (intern (replace-regexp-in-string "alias" "" raw-url)) emojis))
                            (and alias (or (plist-get emojis alias)
                                           (let ((emoji (emojify-get-emoji (format "%s:" alias))))
                                             (if emoji
                                                 (concat (emojify-image-dir) "/" (gethash "image" emoji))))))
                            raw-url)))
           (push-new-emoji (emoji)
                           (puthash (car emoji) t (oref team emoji-master))
                           (cl-pushnew emoji emojify-user-emojis
                                       :test #'string=
                                       :key #'car))
           (on-success
            (&key data &allow-other-keys)
            (slack-request-handle-error
             (data "slack-download-emoji")
             (emojify-create-emojify-emojis)
             (let* ((emojis (plist-get data :emoji))
                    (paths nil))
               (cl-loop for (name _) on emojis by #'cddr
                        do (let* ((url (handle-alias name emojis))
                                  (path (if (file-exists-p url) url
                                          (slack-image-path url)))
                                  (emoji (cons (format "%s:" name)
                                               (list (cons "name" (substring (symbol-name name) 1))
                                                     (cons "image" path)
                                                     (cons "style" "github")))))
                             (if (file-exists-p path)
                                 (push-new-emoji emoji)
                               (slack-url-copy-file
                                url
                                path
                                :success #'(lambda () (push-new-emoji emoji))))

                             (push path paths)))
               (funcall after-success paths)))))
        (slack-request
         (slack-request-create
          slack-emoji-list
          team
          :success #'on-success)))))

(defun slack-select-emoji (team)
  (if (and (fboundp 'emojify-completing-read)
           (fboundp 'emojify-download-emoji-maybe))
      (progn (emojify-download-emoji-maybe)
             (cl-labels
                 ((select ()
                          (emojify-completing-read "Select Emoji: "
                                                   #'(lambda (data &rest args)
                                                       (unless (null args)
                                                         (slack-log (format "Invalid completing argments: %s, %s" data args)
                                                                    team :level 'debug))
                                                       (let ((emoji (car (split-string data " "))))
                                                         (or (gethash emoji
                                                                      slack-emoji-master
                                                                      nil)
                                                             (gethash emoji
                                                                      (oref team emoji-master)
                                                                      nil)))))))
               (if (< 0 (hash-table-count slack-emoji-master))
                   (select)
                 (slack-emoji-fetch-master-data (car (hash-table-values slack-teams-by-token)))
                 (select))))
    (read-from-minibuffer "Emoji: ")))

(defun slack-emoji-fetch-master-data (team)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-emoji-fetch-master-data")
                 (cl-loop for emoji in data
                          do (let ((short-names (plist-get emoji :short_names)))
                               (when short-names
                                 (cl-loop for name in short-names
                                          do (puthash (format ":%s:" name)
                                                      t
                                                      slack-emoji-master))))))))
    (slack-request
     (slack-request-create
      slack-emoji-master-data-url
      team
      :type "GET"
      :success #'success
      :without-auth t
      :sync t
      ))))

(defun slack-insert-emoji ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (insert (slack-select-emoji team))))

(provide 'slack-emoji)
;;; slack-emoji.el ends here
