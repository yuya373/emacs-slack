;;; slack-oauth2.el ---                              -*- lexical-binding: t; -*-

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
(require 'oauth2)
(require 'slack-team)

(defconst slack-oauth2-authorize "https://slack.com/oauth/authorize")
(defconst slack-oauth2-access "https://slack.com/api/oauth.access")

(defcustom slack-redirect-url "http://localhost:8080"
  "Redirect url registered for Slack."
  :type 'string
  :group 'slack)

(defun slack-oauth2-get-token (team)
  (oauth2-token-access-token (with-slots (client-id client-secret) team
                               (oauth2-auth
                                slack-oauth2-authorize
                                slack-oauth2-access
                                client-id
                                client-secret
                                "client"
                                nil
                                slack-redirect-url))))

(provide 'slack-oauth2)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; slack-oauth2.el ends here
