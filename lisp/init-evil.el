;; -*- lexical-binding: t; -*-

(use-package evil
  :config
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  (add-to-list 'evil-emacs-state-modes 'info-mode)
  (add-to-list 'evil-emacs-state-modes 'corfu-mode)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'pass-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'telega-root-mode)
  (add-to-list 'evil-emacs-state-modes 'telega-chat-mode)
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
