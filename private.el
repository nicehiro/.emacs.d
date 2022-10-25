;;; package --- Private settings -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

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
  ;; (chunyang-toggle-url-proxy)
  )

;;; Theme

(use-package ef-themes
  :load-path "lib/ef-themes/"
  :config
  ;; Disable all other themes to avoid awkward blending:
  ;; (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  ;; (load-theme 'ef-spring :no-confirm)

  ;; The themes we provide:
  ;;
  ;; Light: `ef-day', `ef-light', `ef-spring', `ef-summer'.
  ;; Dark:  `ef-autumn', `ef-dark', `ef-night', `ef-winter'.
  ;;
  ;; Also those which are optimized for deuteranopia (red-green color
  ;; deficiency): `ef-deuteranopia-dark', `ef-deuteranopia-light'.
  )

(use-package carbon-theme
  :load-path "lib/carbon-theme/")

(defun hiro/disable-themes ()
  "Disable all enabled themes."
  (mapc #'disable-theme custom-enabled-themes))

(defadvice load-theme (before disable-themes-first activate)
  "Load theme after disbale all other loaded themes."
  (hiro/disable-themes))

(load-theme 'ef-autumn t t)

;;; evil

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
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil))
  (with-eval-after-load 'evil-maps
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (setq evil-mode-line-format nil))

(use-package hledger-mode
  :pin manual
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init
  (setq hledger-jfile
        (expand-file-name "~/OneDrive - The Hong Kong Polytechnic University/accounts.journal"))
  ;; Expanded account balances in the overall monthly report are
  ;; mostly noise for me and do not convey any meaningful information.
  (setq hledger-show-expanded-report nil)
  (setq hledger-currency-string "$")

  (when (boundp 'my-hledger-service-fetch-url)
    (setq hledger-service-fetch-url
          my-hledger-service-fetch-url))

  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)

  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (when (equal hledger-last-run-command
                                             "balancesheet")
                                  ;; highlight frequently changing accounts
                                  (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                                  (highlight-regexp "^.*credit-card.*$"
                                                    'hledger-warning-face)))))))

(use-package hledger-input
  :pin manual
  :load-path "lib/hledger-mode/"
  :bind (("C-c e" . hledger-capture)
         :map hledger-input-mode-map
         ("C-c C-b" . popup-balance-at-point))
  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))

  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
  (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
  (add-hook 'hledger-input-mode-hook
            (lambda ()
              (make-local-variable 'company-idle-delay)
              (setq-local company-idle-delay 0.1))))

(provide 'private)
;;; private.el ends here
