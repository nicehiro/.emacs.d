;; -*- lexical-binding: t; -*-

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode))

(use-package google-translate
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist '(("en" . "zh") ("zh" . "en")))
  (global-set-key (kbd "C-c t") 'google-translate-smooth-translate))

(use-package which-key
  :config
  (which-key-mode t)
  (which-key-setup-side-window-bottom))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package super-save
  :diminish
  :defer 0.5
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (setq super-save-max-buffer-size 200000)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 10)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)
  (setq auto-save-default nil)
  (super-save-mode 1))

(use-package posframe)

(use-package clipetty
  :hook (after-init . global-clipetty-mode)
  :bind ("M-w" . clipetty-kill-ring-save))

;; (use-package pass)

(provide 'init-utils)
;;; init-utils.el ends here
