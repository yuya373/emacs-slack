;;; slack-util.el ---utility functions               -*- lexical-binding: t; -*-

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
(require 'timer)
(require 'diary-lib)

(defcustom slack-profile-image-file-directory temporary-file-directory
  "Default directory for slack profile images."
  :group 'slack)

(defcustom slack-image-file-directory temporary-file-directory
  "Default directory for slack images."
  :group 'slack)

(defcustom slack-image-max-height 300
  "Max Height of image.  nil is unlimited.  integer."
  :group 'slack)

(defconst slack-log-levels
  '(;; debugging
    (trace . 40) (debug . 30)
    ;; information
    (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defcustom slack-log-level 'info
  "Used in `slack-message-logger'.
One of 'info, 'debug"
  :group 'slack)

(defcustom slack-log-time-format
  "[%Y-%m-%d %H:%M:%S]"
  "Time format for log."
  :group 'slack)

(defalias 'slack-if-let*
  (if (fboundp 'if-let*)
      'if-let*
    'if-let))

(defun slack-seq-to-list (seq)
  (if (listp seq) seq (append seq nil)))

(defun slack-decode (seq)
  (cl-loop for e in (slack-seq-to-list seq)
           collect (if (stringp e)
                       (decode-coding-string e 'utf-8)
                     (if (listp e)
                         (slack-decode e)
                       e))))

(defun slack-class-have-slot-p (class slot)
  (and (symbolp slot)
       (let* ((stripped (substring (symbol-name slot) 1))
              (replaced (replace-regexp-in-string "_" "-"
                                                  stripped))
              (symbolized (intern replaced)))
         (slot-exists-p class symbolized))))

(defun slack-collect-slots (class seq)
  (let ((plist (slack-seq-to-list seq)))
    (cl-loop for p in plist
             if (and (slack-class-have-slot-p class p)
                     (plist-member plist p))
             nconc (let ((value (plist-get plist p)))
                     (list p (if (stringp value)
                                 (decode-coding-string value 'utf-8)
                               (if (eq :json-false value)
                                   nil
                                 value)))))))
(defun slack-log-level-to-int (level)
  (slack-if-let* ((cell (cl-assoc level slack-log-levels)))
      (cdr cell)
    20))


(defun slack-message-logger (message level team)
  "Display message using `message'."
  (let ((user-level (slack-log-level-to-int slack-log-level))
        (current-level (slack-log-level-to-int level)))
    (when (<= current-level user-level)
      (message (format "%s [%s] [%s] %s"
                       (format-time-string slack-log-time-format)
                       level
                       (oref team name)
                       message)))))

(cl-defun slack-log (msg team &key
                         (logger #'slack-message-logger)
                         (level 'debug))
  "LEVEL is one of 'trace, 'debug, 'info, 'warn, 'error"
  (let ((log (format "%s [%s] %s - %s"
                     (format-time-string slack-log-time-format)
                     level
                     msg
                     (oref team name)))
        (buf (get-buffer-create (slack-log-buffer-name team))))
    (when (functionp logger)
      (funcall logger msg level team))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (save-excursion
        (goto-char (point-max))
        (insert log)
        (insert "\n"))
      (setq buffer-read-only t))))

(defun company-slack-backend (command &optional arg &rest ignored)
  "Completion backend for slack chats.  It currently understands
@USER; adding #CHANNEL should be a simple matter of programming."
  (interactive (list 'interactive))
  (cl-labels
      ((start-from-line-beginning (str)
                                  (let ((prompt-length (length lui-prompt-string)))
                                    (>= 0 (- (current-column) prompt-length (length str)))))
       (prefix-type (str) (cond
                           ((string-prefix-p "@" str) 'user)
                           ((string-prefix-p "#" str) 'channel)
                           ((and (string-prefix-p "/" str)
                                 (start-from-line-beginning str))
                            'slash)))
       (content (str) (substring str 1 nil)))
    (cl-case command
      (interactive (company-begin-backend 'company-slack-backend))
      (prefix (when (string= "slack" (car (split-string (format "%s" major-mode) "-")))
                  ;; (cl-find major-mode '(slack-mode
                  ;;                         slack-edit-message-mode
                  ;;                         slack-thread-mode))
                (company-grab-line "\\(\\W\\|^\\)\\(@\\w*\\|#\\w*\\|/\\w*\\)"
                                   2)))
      (candidates (let ((content (content arg)))
                    (cl-case (prefix-type arg)
                      (user
                       (cl-loop for user in (oref slack-current-team users)
                                if (and (not (eq (plist-get user :deleted) t))
                                        (string-prefix-p content
                                                         (plist-get user :name)))
                                collect (concat "@" (plist-get user :name))))
                      (channel
                       (cl-loop for team in (oref slack-current-team channels)
                                if (string-prefix-p content
                                                    (oref team name))
                                collect (concat "#" (oref team name))))
                      (slash
                       (cl-loop for command in (oref slack-current-team commands)
                                if (string-prefix-p (concat "/" content)
                                                    (oref command name))
                                collect (oref command name))
                       ))))
      (doc-buffer
       (cl-case (prefix-type arg)
         (slash
          (company-doc-buffer
           (let* ((team slack-current-team)
                  (command (slack-command-find arg team)))
             (when command
               (slack-command-company-doc-string command team))))))))))

(defun slack-get-ts ()
  (get-text-property 0 'ts (thing-at-point 'line)))

(defun slack-linkfy (text link)
  (if (not (slack-string-blankp link))
      (format "<%s|%s>" link text)
    text))

(defun slack-string-blankp (str)
  (if str
      (> 1 (length str))
    t))

(defun slack-log-buffer-name (team)
  (format "*Slack Log - %s*" (slack-team-name team)))

(defun slack-log-open-buffer ()
  (interactive)
  (let ((team (slack-team-select)))
    (funcall slack-buffer-function (get-buffer-create (slack-log-buffer-name team)))))

(defun slack-event-log-buffer-name (team)
  (format "*Slack Event Log - %s*" (slack-team-name team)))

(defun slack-log-websocket-payload (payload team)
  (let* ((bufname (slack-event-log-buffer-name team))
         (buf (get-buffer-create bufname)))
    (when buf
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (save-excursion
          (goto-char (point-max))
          (insert (format "[%s] %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S")
                          payload)))
        (setq buffer-read-only t)))))

(defun slack-log-open-websocket-buffer ()
  (interactive)
  (if websocket-debug
      (progn
        (let* ((team (slack-team-select))
               (websocket (oref team ws-conn)))
          (if websocket
              (funcall slack-buffer-function
                       (websocket-get-debug-buffer-create websocket))
            (error "Websocket is not connected"))))
    (error "`websocket-debug` is not t")))

(defun slack-log-open-event-buffer ()
  (interactive)
  (let* ((team (slack-team-select))
         (bufname (slack-event-log-buffer-name team))
         (buf (get-buffer bufname)))
    (if buf
        (funcall slack-buffer-function buf)
      (error "No Event Log Buffer"))))

(defun slack-profile-image-path (image-url team)
  (expand-file-name
   (concat (md5 (concat (slack-team-name team) "-" image-url))
           "."
           (file-name-extension image-url))
   slack-profile-image-file-directory))

(cl-defun slack-image--create (path &key (width nil) (height nil) (max-height nil) (max-width nil))
  (let* ((imagemagick-available-p (image-type-available-p 'imagemagick))
         (image (apply #'create-image (append (list path (and imagemagick-available-p 'imagemagick) nil)
                                              (if height (list :height height))
                                              (if width (list :width width))
                                              (if max-height
                                                  (list :max-height max-height))
                                              (if max-width
                                                  (list :max-width max-width))))))
    (if (and (display-graphic-p) imagemagick-available-p)
        (slack-image-shrink image max-height)
      image)))

(defun slack-image-exists-p (image-spec)
  (file-exists-p (slack-image-path (car image-spec))))

(defun slack-image-string (spec &optional pad no-token)
  "SPEC: (list URL WIDTH HEIGHT MAX-HEIGHT MAX-WIDTH)"
  (if spec
      (slack-if-let* ((path (slack-image-path (car spec))))
          (if (file-exists-p path)
              (slack-mapconcat-images
               (slack-image-slice
                (slack-image--create path
                                     :width (cadr spec)
                                     :height (caddr spec)
                                     :max-height (cadddr spec)
                                     :max-width (cadr (cdddr spec))))
               pad)
            (propertize "[Image]"
                        'slack-image-spec spec
                        'no-token no-token))
        "")
    ""))

(defun slack-image-path (image-url)
  (and image-url
       (expand-file-name
        (concat (md5 image-url)
                "."
                (file-name-extension image-url))
        slack-image-file-directory)))

(defun slack-image-slice (image)
  (when image
    (let* ((line-height 50.0)
           (height (or (plist-get (cdr image) :height)
                       (cdr (image-size image t))))
           (line-count (/ height line-height))
           (line (/ 1.0 line-count)))
      (if (< line-height height)
          (cl-loop for i from 0 to (- line-count 1)
                   collect (list (list 'slice 0 (* line i) 1.0 line)
                                 image))
        (list image)))))

(defun slack-image-shrink (image &optional max-height)
  (unless (image-type-available-p 'imagemagick)
    (error "Need Imagemagick"))
  (if max-height
      (let* ((data (plist-get (cdr image) :data))
             (file (plist-get (cdr image) :file))
             (size (image-size image t))
             (height (cdr size))
             (width (car size))
             (h (min height max-height))
             (w (if (< max-height height)
                    (ceiling
                     (* (/ (float max-height) height)
                        width))
                  width)))
        (create-image (or file data) 'imagemagick data :height h :width w))
    image))

(defun slack-mapconcat-images (images &optional pad)
  (when images
    (cl-labels
        ((sort-images (images)
                      (let ((compare (if (or (and (eq system-type 'darwin)
                                                  (< emacs-major-version 26))
                                             (< emacs-major-version 25))
                                         #'>
                                       #'<)))
                        (cl-sort images compare :key
                                 #'(lambda (image) (caddr (car image))))))
         (propertize-image (image)
                           (propertize "image"
                                       'display image
                                       'face 'slack-profile-image-face)))
      (mapconcat #'propertize-image
                 (sort-images images)
                 (format "\n%s" (or pad ""))))))

(cl-defun slack-url-copy-file (url newname &key (success nil) (error nil) (sync nil) (token nil))
  (if (executable-find "curl")
      (slack-curl-downloader url newname
                             :success success
                             :error error
                             :token token)
    (cl-labels
        ((on-success (&key data &allow-other-keys)
                     (when (functionp success) (funcall success)))
         (on-error (&key error-thrown symbol-status response data)
                   (message "Error Fetching Image: %s %s %s, url: %s"
                            (request-response-status-code response)
                            error-thrown symbol-status url)
                   (if (file-exists-p newname)
                       (delete-file newname))
                   (case (request-response-status-code response)
                     (403 nil)
                     (404 nil)
                     (t (when (functionp error)
                          (funcall error
                                   (request-response-status-code response)
                                   error-thrown
                                   symbol-status
                                   url)))))
         (parser () (mm-write-region (point-min) (point-max)
                                     newname nil nil nil 'binary t)))
      (let* ((url-obj (url-generic-parse-url url))
             (need-token-p (and url-obj
                                (string-match-p "slack"
                                                (url-host url-obj))))
             (use-https-p (and url-obj
                               (string= "https" (url-type url-obj)))))
        (request
         url
         :success #'on-success
         :error #'on-error
         :parser #'parser
         :sync sync
         :headers (if (and token use-https-p need-token-p)
                      (list (cons "Authorization" (format "Bearer %s" token)))))))))

(defun slack-render-image (image team)
  (let ((buf (get-buffer-create
              (format "*Slack - %s Image*" (slack-team-name team)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (if image
          (insert (slack-mapconcat-images (slack-image-slice image)))
        (insert "Loading Image..."))
      (setq buffer-read-only t)
      (goto-char (point-min)))

    buf))

(defun slack-parse-time-string (time)
  "TIME should be one of:
- a string giving today’s time like \"11:23pm\"
  (the acceptable formats are HHMM, H:MM, HH:MM, HHam, HHAM,
  HHpm, HHPM, HH:MMam, HH:MMAM, HH:MMpm, or HH:MMPM;
  a period ‘.’ can be used instead of a colon ‘:’ to separate
  the hour and minute parts);
- a string giving specific date and time like \"1991/03/23 03:00\";
- a string giving a relative time like \"90\" or \"2 hours 35 minutes\"
  (the acceptable forms are a number of seconds without units
  or some combination of values using units in ‘timer-duration-words’);
- a number of seconds from now;"
  (if (numberp time)
      (setq time (timer-relative-time nil time)))
  (if (stringp time)
      (let ((secs (timer-duration time)))
        (if secs
            (setq time (timer-relative-time nil secs)))))
  (if (stringp time)
      (progn
        (let* ((date-and-time (split-string time " "))
               (date (and (eq (length date-and-time) 2) (split-string (car date-and-time) "/")))
               (time-str (or (and (eq (length date-and-time) 2) (cadr date-and-time))
                             (car date-and-time)))
               (hhmm (diary-entry-time time-str))
               (now (or (and date (decode-time
                                   (encode-time 0 0 0
                                                (string-to-number (nth 2 date))
                                                (string-to-number (nth 1 date))
                                                (string-to-number (nth 0 date))
                                                )))
                        (decode-time))))
          (if (>= hhmm 0)
              (setq time
                    (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
                                 (nth 4 now) (nth 5 now) (nth 8 now)))))))
  time)

(defmacro slack-merge-list (old-list new-list)
  `(cl-loop for n in ,new-list
            do (let ((o (cl-find-if #'(lambda (e) (slack-equalp n e))
                                    ,old-list)))
                 (if o (slack-merge o n)
                   (push n ,old-list)))))

(cl-defun slack-curl-downloader (url name &key (success nil) (error nil) (token nil))
  (cl-labels
      ((sentinel (proc event)
                 (cond
                  ((string-equal "finished\n" event)
                   (when (functionp success) (funcall success)))
                  (t
                   (let ((status (process-status proc))
                         (output (with-current-buffer (process-buffer proc)
                                   (buffer-substring-no-properties (point-min)
                                                                   (point-max)))))
                     (if (functionp error)
                         (funcall error status output url name)
                       (message "Download Failed. STATUS: %s, EVENT: %s, URL: %s, NAME: %s, OUTPUT: %s"
                                status
                                event
                                url
                                name
                                output))
                     (if (file-exists-p name)
                         (delete-file name))
                     (delete-process proc))))))
    (let* ((url-obj (url-generic-parse-url url))
           (need-token-p (and url-obj
                              (string-match-p "slack" (url-host url-obj))))
           (header (or (and token
                            need-token-p
                            (string-prefix-p "https" url)
                            (format "-H 'Authorization: Bearer %s'" token))
                       ""))
           (output (format "--output '%s'" name))
           (command (format "curl --silent --show-error --fail --location %s %s '%s'" output header url))
           (proc (start-process-shell-command "slack-curl-downloader"
                                              "slack-curl-downloader"
                                              command)))
      (set-process-sentinel proc #'sentinel))))

(provide 'slack-util)
;;; slack-util.el ends here
