;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; (use-package yasnippet-snippets)

;; add my yasnippets
(setq yas-snippet-dirs
      (list
       (expand-file-name "lib/my-yasnippets" user-emacs-directory)))

(provide 'init-yas)
;;; provide init-yas.el ends here
