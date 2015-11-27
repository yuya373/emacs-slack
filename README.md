# emacs-slack

emacs client for Slack

## dependent
- [websocket](https://github.com/ahyatt/emacs-websocket)
- oauth2
  - do `package install`


## configure

```elisp
;; I'm using use-package

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
  (setq slack-user-name "hogehoge")
  (setq slack-token
        "hogehogehoge"))

```

