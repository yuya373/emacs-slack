# emacs-slack

emacs client for Slack
### **[ScreenShot](https://github.com/yuya373/emacs-slack/wiki/ScreenShots)**

## dependent
- [websocket](https://github.com/ahyatt/emacs-websocket)
- [request](https://github.com/tkf/emacs-request)
- oauth2
  - do `package install`
- [circe](https://github.com/jorgenschaefer/circe) (for the Linewise
  User Interface library).
- [alert](https://github.com/jwiegley/alert)
- [emojify](https://github.com/iqbalansari/emacs-emojify) (optional)
  - required if you want to show emoji


## configure
[How To Get Token](#how-to-get-token)


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


## How To Get Token

### 1. Get client-id and client-secret

1. go to https://api.slack.com/apps
2. click "Create New App".
3. fill "App Name" and "Development Slack Team" and click "Create App".
4. "Client ID" and "Client Secret" is listed in "App Credentials" section.

### 2. Configure Redurect URL

1. click "Permissions" in the "Add features and functionality" of "Building Apps for Slack" section.
2. fill "Redirect URLs" like "http://localhost:8080".
3. click "Save URLs".

### 3. Get token

1. call `slack-register-team` with above "Client ID" and "Client Secret".
2. Emacs prompt "Enter the code your browser displayed: ".
3. code appears in the browser's address bar like "http://localhost:8080?code=181818181818.1819919191&state=", enter this code to Emacs.
4. once you activate emacs-slack, token appears in the https://api.slack.com/apps/{APPID}/oauth.
5. save your token and pass `slack-register-team` function, to save 2, 3 steps.

#### Note

emacs-slack use websocket to communicate with Slack, and need request to [rtm.start method](https://api.slack.com/methods/rtm.start).
this request needs "client" scope when authorize and Slack does not yet have "client" scope in new OAuth scope.
make sure `slack-oauth2-auth` requesting with "client" scope and prevent token migration.
![token_migration](https://github.com/yuya373/emacs-slack/wiki/images/token_migration.png)


## how to use

I recommend to chat with slackbot for tutorial using `slack-im-select`.

- `slack-register-team`
  - set team configuration and create team.
  - :name, :client-id, :client-secret is needed for argumens
- `slack-change-current-team`
  - change `slack-current-team` var
- `slack-start`
  - do authorize and initialize
- `slack-ws-close`
  - turn off websoeket connection
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
  - update chennel list
- `slack-message-embed-mention`
  - use to mention to user
- `slack-message-embed-channel`
  - use to mention to channel



## notification

see [alert](https://github.com/jwiegley/alert).
