;; -*- lexical-binding: t; -*-

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(use-package consult-eglot)

(provide 'init-lsp)
;;; init-lsp.el ends here
