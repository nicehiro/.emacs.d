;; -*- lexical-binding: t; -*-

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-max-width 100)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  :hook (eshell-mode . (lambda () (setq-local corfu-auto nil)))
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :load-path "lib/corfu/extensions/"
  :config
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay (cons nil 1.0)))

(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ;; ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p \\" . cape-tex)
         ("C-c p w" . cape-dict))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-complete)
;;; init-complete.el ends here
