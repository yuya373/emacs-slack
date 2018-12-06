;;; slack-authorize.el ---                           -*- lexical-binding: t; -*-

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
(require 'slack-team)
(require 'slack-request)
(require 'slack-channel)
(require 'slack-group)
(require 'slack-im)
(require 'slack-user)
(require 'slack-emoji)
(require 'slack-slash-commands)
(require 'slack-usergroup)
(require 'slack-message-notification)

(defconst slack-authorize-url "https://slack.com/api/rtm.start")
(defconst slack-rtm-connect-url "https://slack.com/api/rtm.connect")

(defun slack-authorize (team &optional error-callback success-callback)
  (let ((authorize-request (oref team authorize-request)))
    (if (and authorize-request (not (request-response-done-p authorize-request)))
        (slack-log "Authorize Already Requested" team)
      (cl-labels
          ((on-error (&key error-thrown symbol-status response data)
                     (oset team authorize-request nil)
                     (slack-log (format "Authorize Failed: %s" error-thrown)
                                team)
                     (when (functionp error-callback)
                       (funcall error-callback
                                :error-thrown error-thrown
                                :symbol-status symbol-status
                                :response response
                                :data data)))
           (on-success (&key data &allow-other-keys)
                       (oset team authorize-request nil)
                       (slack-request-handle-error
                        (data "slack-authorize")
                        (slack-log "Authorization Finished" team)
                        (if success-callback
                            (funcall success-callback data)
                          (cl-labels
                              ((on-emoji-download (paths)
                                                  (oset team
                                                        emoji-download-watch-timer
                                                        (run-with-idle-timer 5 t
                                                                             #'slack-team-watch-emoji-download-complete
                                                                             team paths)))
                               (on-open ()
                                        (slack-channel-list-update team)
                                        (slack-group-list-update team)
                                        (slack-im-list-update team)
                                        (slack-user-list-update team)
                                        (slack-bot-list-update team)
                                        (slack-download-emoji team #'on-emoji-download)
                                        (slack-command-list-update team)
                                        (slack-usergroup-list-update team)
                                        (slack-update-modeline)))
                            (let ((self (plist-get data :self))
                                  (team-data (plist-get data :team)))
                              (oset team id (plist-get team-data :id))
                              (oset team name (plist-get team-data :name))
                              (oset team self self)
                              (oset team self-id (plist-get self :id))
                              (oset team self-name (plist-get self :name))
                              (slack-team-set-ws-url team (plist-get data :url))
                              (oset team domain (plist-get team-data :domain)))
                            (slack-team-open-ws team :on-open #'on-open))))))
        (let ((request (slack-request
                        (slack-request-create
                         slack-rtm-connect-url
                         team
                         :params (list (cons "mpim_aware" "1"))
                         :success #'on-success
                         :error #'on-error))))
          (oset team authorize-request request))))))

(provide 'slack-authorize)
;;; slack-authorize.el ends here
