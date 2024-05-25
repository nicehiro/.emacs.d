;; -*- lexical-binding: t; -*-

(use-package rime
  :bind (("C-`" . rime-send-keybinding)
         ("C-\\" . toggle-input-method))
  :hook (post-command . rime-change-cursor-color)
  :custom
  (default-input-method "rime")
  (rime-title "")
  :init
  (setq rime-share-data-dir "/usr/share/rime-data")
  (when *is-a-mac*
    (setq rime-librime-root "~/.config/emacs/librime/dist")
    ;; (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")
    ;; Mac rime schame store path now is ~/Library/Rime/
    (setq rime-share-data-dir "~/Library/Rime/")
    )
  :config
  (defvar rime-default-cursor-color (frame-parameter nil 'cursor-color)
    "The default cursor color.")
  (defun rime-change-cursor-color ()
    "Set cursor color depending on input method."
    (set-cursor-color (if (and (rime--should-enable-p)
                               (not (rime--should-inline-ascii-p))
                               current-input-method)
                          "Orange"
                        rime-default-cursor-color)))
  (setq rime-disable-predicates '(;; rime-predicate-evil-mode-p
                                  rime-predicate-after-alphabet-char-p
                                  rime-predicate-prog-in-code-p
                                  rime-predicate-tex-math-or-command-p
                                  rime-predicate-current-uppercase-letter-p
                                  rime-predicate-ace-window-p
                                  rime-predicate-after-ascii-char-p))
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-style 'vertical))

(provide 'init-rime)
;;; init-rime.el ends here
