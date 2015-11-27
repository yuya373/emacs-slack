# emacs-slack

emacs client for Slack

## dependent
- [websocket](https://github.com/ahyatt/emacs-websocket)
- oauth2
  - do `package install`


## configure

- get client-id and client-secret from https://api.slack.com/applications/new
- get slack-token from https://api.slack.com/web

```elisp
;; I'm using use-package and el-get

(el-get-bundle websocket)
(el-get-bundle oauth2)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-function
        #'(lambda (buffer) (popwin:close-popup-window)
            (switch-to-buffer-other-window buffer)))
  (setq slack-room-subscription '(test-group slackbot))
  (setq slack-client-id "hoge")
  (setq slack-client-secret "fuga")
  (setq slack-token "hogehogehoge"))
  (setq slack-user-name "hogehoge")

```

