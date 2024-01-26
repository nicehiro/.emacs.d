;; -*- lexical-binding: t; -*-

(use-package evil
  :config
  (evil-mode 1)
  (evil-set-initial-state 'ledger-report-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'info-mode 'emacs)
  ;; (evil-set-initial-state 'corfu-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'pass-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'telega-root-mode 'emacs)
  (evil-set-initial-state 'telega-chat-mode 'emacs)
  (evil-set-initial-state 'gptel-mode 'emacs)
  (setq evil-default-state 'normal)
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil))
  (with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (setq evil-mode-line-format nil)
  (setq evil-default-cursor t))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
