# emacs-slack

Emacs client for [Slack](https://slack.com/)
### **[ScreenShot](https://github.com/yuya373/emacs-slack/wiki/ScreenShots)**

## Dependencies
- [websocket](https://github.com/ahyatt/emacs-websocket)
- [request](https://github.com/tkf/emacs-request)
- [Oauth2](https://github.com/emacsmirror/oauth2/blob/master/oauth2.el)
  - do `package install`
- [circe](https://github.com/jorgenschaefer/circe) (for the Linewise User Interface library).
- [Alert](https://github.com/jwiegley/alert)
- [Emojify](https://github.com/iqbalansari/emacs-emojify) (optional)
  - Required if you want to show emoji.

## Configure
[How to get token](#how-to-get-token)

```elisp
;; I'm using use-package and el-get and evil

(el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "emacs-slack"
   :default t
   :client-id "aaaaaaaaaaa.00000000000"
   :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   :token "aaaa-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
   :subscribed-channels '(test-rename rrrrr))

  (slack-register-team
   :name "test"
   :client-id "3333333333.77777777777"
   :client-secret "cccccccccccccccccccccccccccccccc"
   :token "xxxx-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
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

## How to get token (the easy way)

1. Log into the Slack team you're interested in with a web browser
1. Open DevTools
1. Ctrl-F to find, "xoxs-", copy it

## How to get token (the harder yet officially sanctioned way)

### 1. Get client-id and client-secret

1. Go to "https://api.slack.com/apps".
2. Click "Create New App".
3. Fill "App Name" and "Development Slack Team" and click "Create App".
4. "Client ID" and "Client Secret" are listed in the "App Credentials" section.

### 2. Configure Redirect URL

1. Click "Permissions" in the "Add features and functionality" of "Building Apps for Slack" section.
2. Fill in the "Redirect URLs" section (slack-emacs defaults to: "http://localhost:8080").
3. click "Save URLs".

### 3. Get token

1. Call `slack-register-team` with above "Client ID" and "Client Secret" (ignore the "Token" prompt [i.e., just hit enter.]).
2. Emacs' prompt will display: "Enter the code your browser displayed: ".
3. Code appears in the browser's address bar like "http://localhost:8080?code=181818181818.1819919191&state=", enter this code in the previous Emacs' prompt.
4. Once you activate `emacs-slack`, token appears in the URL like: "https://api.slack.com/apps/{APPID}/oauth".
5. Save your token and pass it to the `slack-register-team` function along with the "Client ID" and "Client Secret".

#### Note

emacs-slack uses websocket to communicate with Slack, and need request to [rtm.start method](https://api.slack.com/methods/rtm.start) (you can also test your settings in that page's "Tester" tab to make sure things have been configured correctly).
this request needs "client" scope when authorize and Slack does not yet have "client" scope in new OAuth scope.
make sure `slack-oauth2-auth` requesting with "client" scope and prevent token migration.
![token_migration](https://github.com/yuya373/emacs-slack/wiki/images/token_migration.png)


## How to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

Some terminology in the `slack-` functions:
- `im`: An IM (instant message) is a direct message between you and exactly one other Slack user.
- `channel`: A channel is a Slack channel which you are a member of
- `group`. Any chat (direct message or channel) which isn't an IM is a group.

- `slack-register-team`
  - set team configuration and create team.
  - :name, :client-id, and :client-secret are needed for arguments
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
