;; -*- lexical-binding: t; -*-

(use-package devdocs)
(use-package eldoc
  :custom (eldoc-echo-area-use-multiline-p nil))

(use-package help
  :defer t
  :custom (help-window-select t)
  :config (temp-buffer-resize-mode))

(provide 'init-doc)
;;; init-doc.el ends here
