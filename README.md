<p>
  <a href="https://melpa.org/#/slack"><img alt="MELPA" src="https://melpa.org/packages/slack-badge.svg"/></a>
  <a href="https://travis-ci.com/yuya373/emacs-slack"><img src="https://travis-ci.com/yuya373/emacs-slack.svg?branch=master" alt="Build Status"></a>
  <a href="https://patreon.com/yuya373"><img src="https://img.shields.io/badge/patreon-Become a patron-052D49.svg?logo=patreon&labelColor=E85B46&logoColor=white" alt="Become a patron" /></a>
</p>
<p align="center"><img src="https://raw.githubusercontent.com/yuya373/emacs-slack/assets/assets/slack-logo.svg?sanitize=true" width=300 height=126/></p>
<p align="center"><b>Emacs Slack</b></p>
<p align="center">GNU Emacs client for <a href="https://slack.com/">Slack</a>.</p>

---

## Preview

You can see some gifs on the [wiki](https://github.com/yuya373/emacs-slack/wiki/ScreenShots).

## Dependencies

- [Alert](https://github.com/jwiegley/alert)
- [circe](https://github.com/jorgenschaefer/circe) (for the Linewise User
  Interface library).
- [Emojify](https://github.com/iqbalansari/emacs-emojify) (optional)
- [Oauth2](https://github.com/emacsmirror/oauth2/blob/master/oauth2.el)
  - do `package install`
- [request](https://github.com/tkf/emacs-request)
- [websocket](https://github.com/ahyatt/emacs-websocket)

## Extensions

- [helm-slack](https://github.com/yuya373/helm-slack)

## Configuration

[How to get token and cookie](#how-to-get-token-and-cookie)

```elisp
;; I'm using use-package and el-get and evil

(el-get-bundle slack)
(el-get-bundle yuya373/helm-slack) ;; optional
(use-package helm-slack :after (slack)) ;; optional
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
   :subscribed-channels '(test-rename rrrrr)
   :full-and-display-names t)

  (slack-register-team
   :name "test"
   :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
   :subscribed-channels '(hoge fuga))

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
   (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

```

## How to get token and cookie

1. Using Chrome, open and sign into the slack customization page,
   e.g. https://my.slack.com/customize
2. Right click anywhere on the page and choose "inspect" from the
   context menu. This will open the Chrome developer tools.
3. Find the console (it's one of the tabs in the developer tools window)
4. At the prompt ("> ") type the following:
   `window.prompt("your api token is: ", TS.boot_data.api_token)`
5. Copy the displayed token elsewhere.
6. If your token starts with `xoxc` then keep following the other steps below, otherwise you are done and can close the window.
7. Now switch to the Applications tab in the Chrome developer tools (or Storage tab in Firefox developer tools).
8. Expand Cookies in the left-hand sidebar.
9. Click the cookie entry named `d` and copy its value. Note, use the default encoded version, so *don't click* the Show URL decoded checkbox.
10. Now you're done and can close the window.

For further explanation, see the documentation for the emojme project:
[(github.com/jackellenberger/emojme)](https://github.com/jackellenberger/emojme#slack-for-web)

Note that it is only possible to obtain the cookie manually, not through
client-side javascript, due to it being set as `HttpOnly` and `Secure`. See
[OWASP HttpOnly](https://owasp.org/www-community/HttpOnly#Browsers_Supporting_HttpOnly).

## How to secure your token

If someone steals your token they can use the token to impersonate
you, reading and posting to Slack as if they were you.  It's important
to take reasonable precautions to secure your token.

One way to do this is by using the Emacs `auth-source` library. Read
the [auth-source
documentation](https://www.gnu.org/software/emacs/manual/html_node/auth/index.html)
to learn how to use it to store login information for remote services.

Then configure the `auth-sources` variable to select a "backend"
store. The default backend is `~/.authinfo` file, which is simple but
also un-encrypted. A more complex option is to encrypt that
`.~/authinfo` file with `gnupg` and configure `auth-sources` to use
`~/.authinfo.gpg` as the source for all passwords and secrets. Other
backends exist beyond these; read the documentation for details.

How to store your slack tokens in your `auth-source` backend will vary
depending which backend you chose. See documentation for details. The
"host" and "user" fields can be whatever you like as long as they are
unique; as a suggestion use "myslackteam.slack.com" for host, and use
your email address for user. The "secret" or "password" field should
contain the token you obtained earlier ([How to get
token and cookie](#how-to-get-token-and-cookie)).

Do the same for the cookie, however for the "user" field append `^cookie`, so if
for the token you picked `user@email.com` then for the cookie use
`user@email.com^cookie`.

Then finally, in your Emacs init read the token from your
`auth-source`:

``` elisp
(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :subscribed-channels '((channel1 channel2)))
```

If your token starts with `xoxc` you'll also need to manually obtain the cookie
as described in [How to get token and cookie](#how-to-get-token-and-cookie) and
make sure the "user" has `^cookie` in it as described above in [How to secure
your token](#how-to-secure-your-token):

``` elisp
(slack-register-team
 :name "myslackteam"
 :token (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com")
 :cookie (auth-source-pick-first-password
         :host "myslackteam.slack.com"
         :user "me@example.com^cookie")
 :subscribed-channels '((channel1 channel2)))
```

If you do not specify `:cookie` then you'll automatically be prompted for one if
you are using an `xoxc` token.

## How to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

Some terminology in the `slack-` functions:
- `im`: An IM (instant message) is a direct message between you and exactly one other Slack user.
- `channel`: A channel is a Slack channel which you are a member of
- `group`. Any chat (direct message or channel) which isn't an IM is a group.

- `slack-register-team`
  - set team configuration and create team.
  - :name and :token are required
- `slack-change-current-team`
  - change `slack-current-team` var
- `slack-start`
  - do authorize and initialize
- `slack-ws-close`
  - turn off websocket connection
- `slack-group-select`
  - select group from list
- `slack-im-select`
  - select direct message from list
- `slack-channel-select`
  - select channel from list
- `slack-group-list-update`
  - update group list
- `slack-im-list-update`
  - update direct message list
- `slack-channel-list-update`
  - update channel list
- `slack-message-embed-mention`
  - use to mention to user
- `slack-message-embed-channel`
  - use to mention to channel
- `slack-file-upload`
  - uploads a file
  - the command allows to choose many channels via select loop. In order to finish the loop input an empty string. For helm that's <kbd>C+RET</kbd> or <kbd>M+TET</kbd>. In case of Ivy it's <kbd>C+M+j</kbd>.

## Notification

See [alert](https://github.com/jwiegley/alert).
