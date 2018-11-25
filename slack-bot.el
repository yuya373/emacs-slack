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

(require 'slack-team)

(defun slack-find-bot (id team)
  (with-slots (bots) team
    (cl-find-if (lambda (bot)
                  (string= id (plist-get bot :id)))
                bots)))

(defun slack-find-bot-by-name (name team)
  (with-slots (bots) team
    (cl-find-if #'(lambda (bot)
                    (string= name (plist-get bot :name)))
                bots)))



(provide 'slack-bot)
;;; slack-bot.el ends here
