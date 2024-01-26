;; -*- lexical-binding: t; -*-

(use-package wakatime-mode
  :config
  (if *is-a-win*
      (setq wakatime-cli-path "c:/Users/fywan/scoop/apps/wakatime-cli/current/wakatime-cli.exe"))
  (global-wakatime-mode))

(provide 'init-waka)
;; init-waka.el ends here
