;; -*- lexical-binding: t; Modeline Configuration -*-

;;; Configure uniquification of buffer name
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffer-re "^\\*"))

(progn ; `modeline'
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-position-column-line-format '(" %l,%c"))
  (setq mode-line-compact nil)
  ;; evil
  (defun mode-line-evil ()
    "Evil state info for modeline."
    (cond
     ((eq evil-state 'normal) "Ⓝ")
     ((eq evil-state 'insert) "Ⓘ")
     ((eq evil-state 'visual) "Ⓥ")
     ((eq evil-state 'emacs) "Ⓔ")
     (t "＃")))

  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  " "
                  (:eval (mode-line-evil))
                  "  "
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  ;; mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  mode-line-modes
                  "  "
                  (vc-mode vc-mode)
                  "  "
                  mode-line-misc-info
                  mode-line-end-spaces)))

(use-package minions
  :config
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-direct (list 'defining-kbd-macro
                             'flymake-mode))
  (setq minions-mode-line-delimiters '("(" . "-)"))
  (minions-mode 1))

(use-package recursion-indicator
  :config
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))


(provide 'init-modeline)
;;; init-modeline.el ends here
