;; -*- lexical-binding: t; -*-

(use-package python
  :defer t
  :custom (python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" ".")
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (setq python-shell-interpreter
                    (concat pyvenv-virtual-env
                            (if (eq system-type 'windows-nt)
                                "scripts/python"
                              "bin/python"))))))

(provide 'init-python)
;;; init-python.el ends here
