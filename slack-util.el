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

(require 'subr-x)
(require 'eieio)
(require 'cl-lib)
(require 'timer)
(require 'diary-lib)
(require 'websocket)

(defvar slack-completing-read-function)
(defvar slack-buffer-function)
(defvar slack-next-page-token "[Next page]")
(defvar slack-current-buffer)

(defalias 'slack-if-let*
  (if (fboundp 'if-let*)
      'if-let*
    'if-let))

(cl-defmacro slack-select-from-list ((alist prompt &key initial) &body body)
  "Bind candidates from selected."
  (declare (indent 2) (debug t))
  (let ((key (cl-gensym)))
    `(let* ((,key (let ((completion-ignore-case t))
                    (funcall slack-completing-read-function (format "%s" ,prompt)
                             ,alist nil t ,initial)))
            (selected (cdr (cl-assoc ,key ,alist :test #'string=))))
       ,@body
       selected)))

(defmacro slack-merge-list (old-list new-list)
  `(cl-loop for n in ,new-list
            do (let ((o (cl-find-if #'(lambda (e) (slack-equalp n e))
                                    ,old-list)))
                 (if o (slack-merge o n)
                   (push n ,old-list)))))

(defmacro slack-plist-each (plist &rest body)
  (declare (indent 2) (debug t))
  (let ((dup (cl-gensym)))
    `(let* ((,dup (copy-sequence ,plist))
            (key  (pop ,dup))
            (value (pop ,dup)))
       ,@body)))

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

(defun slack-get-ts ()
  (let ((bol (point-at-bol))
        (eol (point-at-eol)))
    (when (and bol eol)
      (cl-loop for i from bol to eol
               for ts = (get-text-property i 'ts)
               if ts
               return ts))))

(defun slack-linkfy (text link)
  (if (not (slack-string-blankp link))
      (format "<%s|%s>" link text)
    text))

(defun slack-string-blankp (str)
  (if str
      (not (null (string-match-p "\\`[ \t\n\r]*\\'" str)))
    t))

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

(cl-defun slack-select-multiple (prompt-fn collection &optional initial-input-fn)
  (let ((result '())
        (loop-count 0)
        (do-loop t))
    (while do-loop
      (let ((selected (apply slack-completing-read-function
                             (list
                              (funcall prompt-fn loop-count)
                              (if result (cons "" collection) collection)
                              nil       ; predicate
                              t         ; require-match
                              (when (functionp initial-input-fn)
                                (funcall initial-input-fn loop-count))
                              nil       ; history
                              ""        ; def
                              ))))
        (if (and selected (< 0 (length selected)))
            (progn
              (push (cdr (cl-assoc selected collection :test #'equal))
                    result)
              (setq collection (cl-remove-if #'(lambda (e)
                                                 (equal selected (car-safe e)))
                                             collection))
              (cl-incf loop-count))

          (setq do-loop nil))))
    (cl-delete-if #'null result)))

;; org-combine-plists
(defun slack-merge-plist (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls)
              v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(cl-defmethod slack-ts ((ts string))
  ts)

(defun slack-propertize-mention-text (face display text)
  (let ((props (list 'rear-nonsticky t
                     'display display
                     'face face))
        (head (substring text 0 1))
        (rest (substring text 1)))
    (concat (apply #'propertize (format "%s%s"
                                        (propertize head 'slack-mention-props (list :props props))
                                        rest)
                   props)
            " ")))

(cl-defun slack-format-ts (ts &optional (format "%Y-%m-%d %H:%M:%S"))
  (when ts
    (when (stringp ts)
      (setq ts (string-to-number ts)))
    (format-time-string format (seconds-to-time ts))))

(defun slack-format-message (&rest args)
  (let ((messages args))
    (mapconcat #'identity
               (cl-remove-if #'(lambda (e) (or (null e)
                                               (< (length e) 1)))
                             messages)
               "\n")))

(provide 'slack-util)
;;; slack-util.el ends here
