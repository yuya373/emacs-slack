(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-byte-compile nil)
(setq el-get-bundle-byte-compile nil)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle websocket)
(el-get-bundle tkf/emacs-request :name request)
(el-get-bundle oauth2)
(el-get-bundle jorgenschaefer/circe)
(el-get-bundle alert)
(el-get-bundle company)
