;; -*- lexical-binding: t; -*-

(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :init
  (setq hledger-jfile "~/OneDrive - The Hong Kong Polytechnic University/accounts.journal")
  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  (setq hledger-currency-string "$"))

(provide 'init-hledger)
;;; init-hledger.el ends here
