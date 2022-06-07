;;; package --- Private settings -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

;;; Telegram

(use-package telega
  :config
  (setq telega-proxies
        (list
         '(:server "127.0.0.1" :port 7890 :enable t
                   :type (:@type "proxyTypeSocks5"))
         )))

;;; Proxy

(use-package url
  :config
  (defun chunyang-toggle-url-proxy ()
    "Toggle proxy for the url.el library."
    (interactive)
    (cond
     (url-proxy-services
      (message "Turn off URL proxy")
      (setq url-proxy-services nil))
     (t
      (message "Turn on URL proxy")
      (setq url-proxy-services
            '(("http" . "127.0.0.1:7890")
              ("https" . "127.0.0.1:7890")
              ("no_proxy" . "0.0.0.0"))))))
  (chunyang-toggle-url-proxy))

(provide 'private)
;;; private.el ends here
