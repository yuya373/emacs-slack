;;; slack-bot.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <yuya373@archlinux>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'slack-util)
(require 'slack-request)
(require 'slack-team)

(defconst slack-bot-info-url "https://slack.com/api/bots.info")

(defun slack-find-bot (id team)
  (gethash id (oref team bots)))

(defun slack-bot-info-request (bot-id team &optional after-success)
  (cl-labels
      ((on-success (&key data &allow-other-keys)
                   (slack-request-handle-error
                    (data "slack-bot-info-request")
                    (let ((bot (plist-get data :bot)))
                      (slack-team-set-bots team (list bot))))
                   (if after-success
                       (funcall after-success))))
    (slack-request
     (slack-request-create
      slack-bot-info-url
      team
      :params (list (cons "bot" bot-id))
      :success #'on-success))))

(defun slack-bots-info-request (bot-ids team &optional after-success)
  (cl-labels
      ((success (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-bots-info-request")
                 (let ((bots (plist-get data :bots)))
                   (slack-team-set-bots team bots)))
                (if after-success
                    (funcall after-success))))
    (slack-request
     (slack-request-create
      slack-bot-info-url
      team
      :params (list (cons "bots" (mapconcat #'identity bot-ids ",")))
      :success #'success))))

(provide 'slack-bot)
;;; slack-bot.el ends here
