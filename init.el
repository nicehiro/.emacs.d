;;; init.el --- user-init-file              -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(progn ; `startup'
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq ring-bell-function #'ignore)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-window-scroll-bars
               (minibuffer-window frame) 0 nil 0 nil t)))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 1)
  (setq x-underline-at-descent-line t)
  (toggle-debug-on-error))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn ; `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (require 'use-package))

(use-package epkg)

(use-package no-littering)

(use-package custom
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))
  (setq enable-local-variables :all))

(progn ; `ns-win'
  (when *is-a-mac*
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)))

;; set up exec-path to help Emacs find programs
(use-package exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
	    (unless (memq system-type '(ms-dos window-nt))
	      (daemonp)))
  :custom (exec-path-from-shell-arguments '("-l"))
  :config
  (dolist (var '("GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(progn ; `fontset' ;; copied from meowmacs
  (defvar meomacs-font-size 16
    "Current font size.")

  ;; default fonts: Lation Modern Mono
  (defvar meomacs-fonts '((default . "Monaco")
                          (cjk . "LXGW WenKai")
                          (symbol . "Symbola")
                          (fixed . "Source Code Pro")
                          (fixed-serif . "Latin Modern Mono")
                          (variable . "Georgia"))
    "Fonts to use.")

  (defun meomacs--get-font-family (key)
    (alist-get key meomacs-fonts))

  ;; Set default font before frame creation
  ;; to make sure the first frame have the correct size
  ;; (add-to-list 'default-frame-alist (cons 'font (format "%s-%s"
  ;;                                                       (meomacs--get-font-family 'default)
  ;;                                                       meomacs-font-size)))
  ;; (set-frame-font (format "%s-%s" (meomacs--get-font-family 'default) meomacs-font-size))

  (defun meomacs-load-font ()
    "Load font configuration."
    (let ((default-font (format "%s-%s"
                                (meomacs--get-font-family 'default)
                                meomacs-font-size))
          (cjk-font (meomacs--get-font-family 'cjk))
          (symbol-font (meomacs--get-font-family 'symbol))
          (variable-font (meomacs--get-font-family 'variable))
          (fixed-font (meomacs--get-font-family 'fixed))
          (fixed-serif-font (meomacs--get-font-family 'fixed-serif)))
      (set-frame-font default-font)
      (let ((fontset (create-fontset-from-ascii-font default-font)))
        ;; Fonts for charsets
        (dolist (charset '(kana han hangul cjk-misc bopomofo))
          (set-fontset-font fontset charset cjk-font))
        (set-fontset-font fontset 'symbol symbol-font)
        ;; Apply fontset
        (set-frame-parameter nil 'font fontset)
        (add-to-list 'default-frame-alist (cons 'font fontset))
        ;; Fonts for faces
        (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0)
        (set-face-attribute 'fixed-pitch nil :family fixed-font :height 1.0)
        (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font :height 1.0))))

  (when window-system
    (meomacs-load-font))

  (defvar hiro/font-size-list '(12 14 16 18 20 22))

  (defun hiro/larger-font ()
    "Larger font."
    (interactive)
    (if-let ((size (--find (> it meomacs-font-size) hiro/font-size-list)))
        (progn (setq meomacs-font-size size)
               (meomacs-load-font)
               (message "Font size: %s" meomacs-font-size))
      (message "Using largest font")))

  (defun hiro/smaller-font ()
    "Smaller font."
    (interactive)
    (if-let ((size (--find (< it meomacs-font-size) (reverse hiro/font-size-list))))
        (progn (setq meomacs-font-size size)
               (message "Font size: %s" meomacs-font-size)
               (meomacs-load-font))
      (message "Using smallest font")))

  ;; Run after startup
  (add-hook 'after-init-hook
            (lambda ()
              (when window-system
                (meomacs-load-font))))
  )

;; Change global font size easily

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset)))

;;; Dired mode

(use-package dired
  :bind (:map dired-mode-map
	      ("e" . dired-open-externally))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alGh")
  (dired-recursive-copies 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (defun dired-open-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-filename))
     arg)))

(use-package diredfl
  :after dired
  :config (diredfl-global-mode))

;;; Configure uniquification of buffer name

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffer-re "^\\*"))

;;; A universal on-the-fly syntax checker

(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

;;; Modeline

(progn ; `modeline'
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-position-column-line-format '(" %l,%c"))
  (setq mode-line-compact nil)

  ;; (let ((active-bg (face-attribute 'mode-line :background))
  ;;       (inactive-bg (face-attribute 'mode-line-inactive :background)))
  ;;   (custom-set-faces
  ;;    `(mode-line ((t :inherit variable-pitch :box (:line-width 6 :color ,active-bg) :background ,active-bg)))
  ;;    `(mode-line-inactive ((t :inherit variable-pitch :box (:line-width 6 :color ,inactive-bg) :background ,inactive-bg)))))

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
                  (:eval (mode-line-evil))
                  "  "
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
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

;;; Minibuffer and completion

(use-package minibuffer
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (enable-recursive-minibuffers t)
  :init
  ;; Make sure vertico commands are hidden in M-x
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :demand t
  :config
  (defmacro dispatch: (regexp style)
    (cl-flet ((symcat (a b) (intern (concat a (symbol-name b)))))
      `(defun ,(symcat "dispatch:" style) (pattern _index _total)
         (when (string-match ,regexp pattern)
           (cons ',(symcat "orderless-" style) (match-string 1 pattern))))))
  (cl-flet ((pre/post (str) (format "^%s\\(.*\\)$\\|^\\(?1:.*\\)%s$" str str)))
    (dispatch: (pre/post "=") literal)
    (dispatch: (pre/post "`") regexp)
    (dispatch: (pre/post (if (or minibuffer-completing-file-name
                                 (derived-mode-p 'eshell-mode))
                             "%" "[%.]"))
               initialism))
  (dispatch: "^{\\(.*\\)}$" flex)
  (dispatch: "^\\([^][^\\+*]*[./-][^][\\+*$]*\\)$" prefixes)
  (dispatch: "^!\\(.+\\)$" without-literal)
  :custom
  (orderless-matching-styles 'orderless-regexp)
  (orderless-style-dispatchers
   '(dispatch:literal dispatch:regexp dispatch:without-literal
                      dispatch:initialism dispatch:flex dispatch:prefixes))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :demand t
  :custom (vertico-cycle t)
  :config (vertico-mode))

(use-package vertico-directory
  :load-path "lib/vertico/extensions"
  :after vertico
  :demand t
  :bind
  (:map
   vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . 'vertico-directory-delete-char)
   ("M-DEL" . 'vertico-directory-delete-word)))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :defer 0.5
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history))
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-after-jump-hook '(recenter-on-top reveal-entry))
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (advice-add #'register-preview :override #'consult-register-window)
  :preface
  (defun recenter-on-top ()
    "`recenter' on top"
    (interactive)
    (recenter 0))
  (defun reveal-entry ()
    "Reveal Org or Outline entry and recenter on top."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry))
     ((and (or (eq major-mode 'outline-mode)
               (bound-and-true-p outline-minor-mode))
           (outline-on-heading-p))
      (outline-show-entry)))))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("M-n" . embark-next-symbol)
  ("M-p" . embark-previous-symbol)
  ("C-h E" . embark-on-last-message)
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")
  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg))))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t))

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

(use-package corfu-doc
  :hook ((corfu-mode . corfu-doc-mode))
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-up)
        ("M-n" . corfu-doc-scroll-down))
  :config
  (setq corfu-doc-display-within-parent-frame nil))

(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package tempel
  :custom (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("M-]" . tempel-next)
         ("M-[" . tempel-previous))
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :init
  (defun tempel-setup-capf ()
    "Setup completion at point."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))

;;; Working with Windows within frames

(use-package window
  :bind (([f7] . sanityinc/split-window)
         ("C-c <down>". sanityinc/toggle-current-window-dedication)
         :map ctl-x-4-map
         ("s" . toggle-window-split))
  :config
  (bind-key "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
  (bind-key "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))
  :preface
  ;; When splitting window, show (other-buffer) in the new window
  (defun split-window-func-with-other-buffer (split-function)
    "Use SPLIT-FUNCTION to split window."
    (lambda (&optional arg)
      "Split this window and switch to the new window unless ARG is provided."
      (interactive "P")
      (funcall split-function)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window)))))

  (defun toggle-window-split ()
    "Toggle window split from vertical to horizontal."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows")
      (let ((was-full-height (window-full-height-p)))
        (delete-other-windows)
        (if was-full-height
            (split-window-vertically)
          (split-window-horizontally))
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

  ;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
  (defun sanityinc/split-window()
    "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
    (interactive)
    (if (eq last-command 'sanityinc/split-window)
        (progn
          (jump-to-register :sanityinc/split-window)
          (setq this-command 'sanityinc/unsplit-window))
      (window-configuration-to-register :sanityinc/split-window)
      (switch-to-buffer-other-window nil)))

  ;; Toggle to dedicated window
  (defun sanityinc/toggle-current-window-dedication ()
    "Toggle whether the current window is dedicated to its current buffer."
    (interactive)
    (let* ((window (selected-window))
           (was-dedicated (window-dedicated-p window)))
      (set-window-dedicated-p window (not was-dedicated))
      (message "Window %sdedicated to %s"
               (if was-dedicated "no longer " "")
               (buffer-name)))))

;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :bind (("C-x o" . switch-window)
         :map ctl-x-4-map
         ("t" . switch-window-then-swap-buffer))
  :custom
  (switch-window-shortcut-style 'alphabet)
  (switch-window-timeout nil))

;;; Settings for tracking recent files

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat user-emacs-directory "lib/.*-autoloads\\.el\\'")))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode))

;;; Editing utils

(progn ; favorite default
  (setq-default
   use-short-answers t
   blink-cursor-interval 0.4
   column-number-mode t
   indent-tabs-mode nil
   create-lockfiles nil
   auto-save-default nil
   make-backup-files nil
   mouse-yank-at-point t
   save-interprogram-paste-before-kill t
   scroll-preserve-screen-position 'always
   set-mark-command-repeat-pop t
   truncate-partial-width-windows nil
   tooltip-delay 1.5))

(progn ; `pixel-scroll'
  (if (boundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)))

(use-package delsel
  :config (delete-selection-mode))

(use-package elec-pair
  :config (electric-pair-mode))

(use-package electric
  :config (electric-indent-mode))

(use-package autorevert
  :diminish
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode))

(use-package beacon
  :custom
  (beacon-lighter "")
  (beacon-size 20)
  (beacon-blink-when-window-scrolls nil)
  :config (beacon-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package move-dup
  :bind
  ("C-c d" . move-dup-duplicate-down)
  ("C-c u" . move-dup-duplicate-up)
  ([M-up] . move-dup-move-lines-up)
  ([M-down] . move-dup-move-lines-down))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(progn ; `buffer'
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left)
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))

(bind-keys
 ("C-x k" . kill-current-buffer)
 ("C-x x p" . pop-to-mark-command)
 ("C-x C-." . pop-global-mark)
 ;; M-^ is inconvenient, so also bind M-j
 ("M-j" . join-line)
 ;; Zap *up* to char is a handy pair for zap-to-char
 ("M-Z" . zap-up-to-char))

;;; Whitespace

(use-package ws-butler
  :diminish
  :hook (after-init . ws-butler-global-mode))

;;; Version control

(use-package diff-hl
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package git-link
  :bind (("C-c g l" . git-link)
         ("C-c g h" . git-link-homepage)
         ("C-c g c" . git-link-commit)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-module-sections-nested nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (put 'magit-clean 'disabled nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (with-eval-after-load "magit-submodule"
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
    (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote)))

;;; Text editing

(use-package org
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c i a" . org-id-get-create)
         ("C-c e d" . org-export-docx)
         :map sanityinc/org-global-prefix-map
         ("j" . org-clock-goto)
         ("l" . org-clock-in-last)
         ("i" . org-clock-in)
         ("o" . org-clock-out)
         ("b" . org-mark-ring-goto)
         :map org-src-mode-map
         ;; I prefer C-c C-c over C-c ' (more consistent)
         ("C-c C-c" . org-edit-src-exit))
  :bind-keymap ("C-c o" . sanityinc/org-global-prefix-map)
  :hook ((org-agenda-mode . hl-line-mode)
         (org-mode . variable-pitch-mode))
  :custom
  (org-modules nil) ; Faster loading
  (org-log-done 'time)
  (org-fontify-done-headline nil)
  (org-edit-timestamp-down-means-later t)
  (org-catch-invisible-edits 'show)
  (org-export-coding-system 'utf-8)
  (org-fast-tag-selection-single-key 'expert)
  (org-html-validation-link nil)
  (org-export-kill-product-buffer-when-displayed t)
  (org-tags-column 80)
  (org-hide-emphasis-markers t)
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-indent-mode t)
  (org-image-actual-width nil) ;; set this first for #+attr_org :width works
  :config
  ;; org face
  (with-eval-after-load "org"
    ;; Use fixed pitch for table and code
    (custom-set-faces
     '(org-table ((t :inherit 'fixed-pitch-serif)))
     '(org-code ((t :inherit 'fixed-pitch-serif)))
     '(org-block ((t :inherit 'fixed-pitch-serif)))
     '(org-checkbox ((t :inherit 'fixed-pitch :background nil :box nil)))
     '(org-latex-and-related ((t (:inherit 'fixed-pitch-serif))))))
  ;; org latex code render size
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; make svg latex preview image
  (setq org-preview-latex-default-process 'dvisvgm)
  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (add-hook
                 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languags
   `((emacs-lisp . t)
     (haskell . nil)))
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block )
  :preface
  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")
  ;; Export to docx
  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (expand-file-name "template/template.docx"
                                           user-emacs-directory)))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file))
      (message "Convert finish: %s" docx-file))))

;;; Org agenda

(use-package org-agenda
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  ;; Directories settings
  (when (file-directory-p "~/Documents/gtd/")
    (setq org-agenda-files (list "~/Documents/gtd/")))
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-deadline-warning-days 16)
  (setq org-use-speed-commands t)
  (setq org-agenda-start-day "+0d")
  (setq org-todo-keywords
        '(
          (sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
          (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
          (sequence "|" "READ(r)")
          (sequence "|" "BLOG(b)")
          (sequence "|" "IDEA(i)")
          ))

  ;; my agenda view contains follow items:
  ;;; Ideas
  ;;; Day views
  ;;; Reading list
  ;;; Blogs
  (defvar org-agenda-custom-commands nil
    "Custom commands for org agenda.")
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and Ideas"
           ((todo "IDEA"
                  ((org-agenda-overriding-header "Stupid but maybe interesting IDEAs:")))
            (agenda ""
                    ((org-agenda-overriding-header "Today's agenda")
                     (org-agenda-span 'day)
                     (org-agenda-toggle-deadlines)))
            (agenda ""
                    ((org-agenda-overriding-header "Future's agenda(One week)")
                     (org-agenda-start-day "+1d")
                     (org-agenda-span 7)
                     (org-agenda-toggle-deadlines)))
            (todo "READ"
                  ((org-agenda-overriding-header "Reading List:")))
            (todo "BLOG"
                  ((org-agenda-overriding-header "Blogs:")))))))
  ;; org capture
  (defvar org-capture-templates nil
    "Org capture templates.")
  (setq org-capture-templates '(("i" "Idea"
                                 entry (file+headline "~/Documents/gtd/ideas.org" "Someday/Maybe")
                                 "* IDEA %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t)
                                ("t" "Todo"
                                 entry (file+headline "~/Documents/gtd/inbox.org" "TODOs")
                                 "* TODO %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t)
                                ("r" "Read"
                                 entry (file+headline "~/Documents/gtd/read.org" "Reading List")
                                 "* READ %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t)
                                ("b" "Blog"
                                 entry (file+headline "~/Documents/gtd/blog.org" "Blogs")
                                 "* BLOG %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t))))

;; https://christiantietze.de/posts/2019/12/emacs-notifications/
(use-package appt ;; appointment for org agenda
  :config
  (defun hiro/notify (title msg)
    "Send notification with `msg' and `title'."
    (ns-do-applescript (format "display notification \"%s\" with title \"%s\" sound name \"Submarine\""
                               msg title)))

  (setq appt-time-msg-list nil    ;; clear existing appt list
        appt-display-interval '5  ;; warn every 5 minutes from t - appt-message-warning-time
        appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
        appt-display-mode-line t     ;; don't show in the modeline
        appt-display-format 'window)   ;; pass warnings to the designated window function

  (defun hiro/appt-display-native (min-to-app new-time msg)
    (hiro/notify
     (format "Appointment in %s minutes" min-to-app) ; Title
     (format "%s" msg)))

  (setq appt-disp-window-function (function hiro/appt-display-native))
  (appt-activate 1)                ;; activate appointment notification

  ;; Agenda-to-appointent hooks
  (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  )

(use-package org-download
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-image-dir "~/Pictures/org-download/")
  (setq org-download-image-org-width 400)
  (setq org-download-image-latex-width 400)
  (setq org-download-image-attr-list
        '("#+attr_html: scale=0.8 :align center")))

(use-package markdown-mode
  :mode (("\\.md\\.html\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package org-bars
  :hook (org-mode . org-bars-mode)
  :config
  (setq org-bars-stars '(:empty "◉"
                                :invisible "▶"
                                :visible "▼")))

(use-package writeroom-mode
  :hook (org-mode . prose-mode)
  :custom
  (writeroom-fullscreen-effect 'maximized)
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
    This enables or modifies a number of settings so that the
    experience of editing prose is a little more like that of a
    typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))

;;; Blog writing
(use-package org-static-blog
  :load-path "lib/org-static-blog"
  :config
  (require 'oc-csl)
  (setq org-static-blog-publish-title "Fangyuan's Blog")
  (setq org-static-blog-publish-url "https://nicehiro.github.io")
  (setq org-static-blog-publish-directory "~/Projects/blog/")
  (setq org-static-blog-posts-directory "~/Projects/blog/posts/")
  (setq org-static-blog-drafts-directory "~/Projects/blog/drafts/")
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers t)
  (setq org-static-blog-index-length 5)
  (setq org-static-blog-enable-tags t)
  (setq org-static-blog-use-preview t)

  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"Fangyuan\">
    <meta name=\"referrer\" content=\"no-referrer\">
    <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
    <script src=\"https://kit.fontawesome.com/4afcf67bd2.js\" crossorigin=\"anonymous\"></script>
    <link rel=\"icon\" href=\"static/favicon.ico\">")

  (setq org-static-blog-page-preamble
        "<div>
    <a href=\"https://nicehiro.github.io\">Fangyuan's Blog</a>
    ◌
    <a href=\"archive.html\"><i class=\"fa-solid fa-box-open\"></i>Archive</a>
    ◌
    <a href=\"about.html\"><i class=\"fa-solid fa-address-card\"></i> About</a>
    ◌
    <a href=\"rss.xml\"><i class=\"fa-solid fa-rss\"></i> RSS</a>
    </div>")

  (setq org-static-blog-page-postamble
        "<div id=blog-tail><center><span>Powered by Org-mode and Emacs</center></div>"))

;;; Note taking

(use-package org-roam
  :diminish
  :bind (("C-c n a" . org-id-get-create)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n R" . org-roam-ref-add)
         ("C-c n s" . org-roam-db-sync))
  :custom
  ;; (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory (file-truename "~/Documents/roam/org-roam/"))
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-enable)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package org-roam-dailies
  :load-path "lib/org-roam/extensions/")

(use-package org-roam-ui)

;;; LaTeX
;; read README.GIT && ./configure to generate .el files

(use-package latex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

(use-package preview
  :after latex
  :hook ((LaTeX-mode . preview-larger-previews))
  :config
  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25
                        (funcall (preview-scale-from-face)))))))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

;; temporary fix for this
;; check here https://github.com/tom-tan/auctex-latexmk/issues/44
(provide 'tex-buf)
(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup))

;;; Terminal config

(use-package vterm)

;;; Programming languages support

(use-package elisp-mode
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "ELisp")))
         (emacs-lisp-mode . sanityinc/maybe-set-bundled-elisp-readonly))
  :config
  (setq initial-scratch-message
        (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n"
                ";; stay hungry, stay foolish\n"
                ";; write more, but shorter\n"
                ";; no day but toady\n\n"))
  :preface
  (defun sanityinc/maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1))))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package haskell-mode
  :defer t
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template)))

(use-package python
  :defer t
  :custom (python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" "/usr/local/Caskroom/miniconda/base/envs/")
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (setq python-shell-interpreter
                    (concat pyvenv-virtual-env
                            (if (eq system-type 'windows-nt)
                                "scripts/python"
                              "bin/python"))))))

(use-package devdocs)

;;; Scientific research config

(progn ; `basic-variables'
  (defconst hiro/bib-libraries '("~/Documents/research/references.bib"))
  (defconst hiro/main-bib-library (nth 0 hiro/bib-libraries))
  (defconst hiro/pdf-libraries '("~/Documents/research/pdfs/"))
  (defconst hiro/main-pdf-library (nth 0 hiro/pdf-libraries))
  (defconst hiro/note-libraries '("~/Documents/roam/org-roam/"))
  (defconst hiro/main-note-library (nth 0 hiro/note-libraries)))

(use-package biblio
  :load-path "lib/biblio.el"
  :config
  (setq biblio-download-directory hiro/main-pdf-library)
  ;; extend biblio actions
  (defun hiro/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
    "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
    (with-current-buffer (find-file-noselect hiro/main-bib-library)
      (goto-char (point-max))
      (insert bibtex))
    (message "Inserted bibtex entry for %S."
	     (biblio--prepare-title (biblio-alist-get 'title entry))))
  (defun hiro/biblio-selection-insert-end-of-bibfile (record)
    "Insert BibTeX of current entry `RECORD' at the end of user-specified bibtex file."
    (interactive)
    (biblio--selection-forward-bibtex #'hiro/biblio--selection-insert-at-end-of-bibfile-callback))
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Add bib to the end of bib file" .
                 hiro/biblio-selection-insert-end-of-bibfile))
  (defun hiro/biblio-selection-insert-and-download (record)
    "Insert BibTex of current entry `RECORD' at the end of user-specified bibtex file,
and download pdf to user-specified directory."
    (interactive)
    (progn
      (biblio--selection-forward-bibtex #'hiro/biblio-selection-insert-end-of-bibfile)
      (biblio-download--action record)))
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Add bib and download pdf" .
                 hiro/biblio-selection-insert-and-download)))

(use-package citar
  :bind (("C-c b" . citar-open))
  :config
  (setq citar-bibliography hiro/bib-libraries)
  (setq citar-library-paths hiro/pdf-libraries)
  (setq citar-notes-paths hiro/note-libraries)
  (setq citar-library-file-extensions '("pdf" "org" "md"))
  (setq citar-file-open-function (lambda (fpath)
                                   (call-process "open" nil 0 nil "-a" "Skim" fpath)))
  (defun hiro/citar-full-names (names)
    "Transform names like LastName, FirstName to FirstName LastName."
    (when (stringp names)
      (mapconcat
       (lambda (name)
         (if (eq 1 (length name))
             (split-string name " ")
           (let ((split-name (split-string name ", ")))
             (cl-concatenate 'string (nth 1 split-name) " " (nth 0 split-name)))))
       (split-string names " and ") ", ")))

  (defun hiro/citar-open-roam-note (citekey csl-entry)
    "Citar open or create org-roam node with citekey and csl-entry."
    (if-let* ((csl-title (cdr (assoc "title" csl-entry)))
              (node (org-roam-node-from-title-or-alias (format "Notes on %s" csl-title))))
        (org-roam-node-visit node)
      (let* ((node (org-roam-node-create :title csl-title :refs citekey :tags "papernote")))
        (org-roam-capture-
         :node node
         :keys "r"
         :info (list :ref citekey)
         :props '(:finalize find-file)))))
  (setq citar-open-note-function 'hiro/citar-open-roam-note)
  (setq citar-display-transform-functions
        '((t . citar-clean-string)
          (("author" "editor") . hiro/citar-full-names)))
  (setq citar-templates
        '((main . "${author editor:55}     ${date year issued:4}     ${title:55}")
          (suffix . "  ${tags keywords keywords:40}")
          (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
          (note . "#+title: Notes on ${author editor}, ${title}")))
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  ;; use consult-completing-read for enhanced interface
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package oc
  :config
  (setq org-cite-global-bibliography hiro/bib-libraries
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((latex biblatex)
                                     (t csl))))

(use-package bibtex
  :config
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5
        bibtex-files hiro/bib-libraries
        org-ref-bibtex-hydra-key-binding (kbd "H-b"))
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body))

(use-package org-zotxt
  :hook
  (org-mode . org-zotxt-mode))

;;; Anki

(progn ; anki
  (require 'request)
  ;;; anki-connect default settings
  (defvar anki-connect-host "127.0.0.1"
    "Anki connect server host.")

  (defvar anki-connect-port "8765"
    "Anki connect server port.")

  (defvar anki-deck-name "Words"
    "Shengci in anki deck name.")

;;; copied from youdao-dictionary
  (defun hiro-region-or-word ()
    "Return word in region or word at point."
    (if (derived-mode-p 'pdf-view-mode)
        (if (pdf-view-active-region-p)
            (mapconcat 'identity (pdf-view-active-region-text) "\n"))
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning)
                                          (region-end))
        (thing-at-point 'word t))))


  (defun hiro-htmlize-ul (elements)
    "Htmlize ELEMENTS to <ul> element."
    (concat "<ul>"
            (mapconcat
             (lambda (element) (concat "<li>" element "</li>"))
             elements
             "<br>")
            "</ul>"))

  (defun hiro-htmlize-anki-back (translation basic-explains web-refs)
    (format "<h3>Translation</h3>%s<h3>Basic Explainations</h3>%s<h3>Web References</h3>%s"
            translation
            basic-explains
            web-refs))

;;; most of this copied from youdao-dictionary
  (defun hiro-format-youdao-result (json)
    "Format result in youdao-dictionary JSON."
    (let* ((query (assoc-default 'query json))
           (translation (assoc-default 'translation json))
           (_errorCode (assoc-default 'errorCode json))
           (web (assoc-default 'web json))
           (basic (assoc-default 'basic json))
           (phonetic (assoc-default 'phonetic basic))
           (translation-str (hiro-htmlize-ul translation))
           (basic-explains-str (hiro-htmlize-ul (assoc-default 'explains basic)))
           (web-str (concat "<ul>"
                            (mapconcat
                             (lambda (k-v)
                               (format "<li> %s :: %s </li>"
                                       (assoc-default 'key k-v)
                                       (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                             web
                             "")
                            "</ul>"))
           ;; (back (concat translation-str basic-explains-str web-str)))
           (back (hiro-htmlize-anki-back translation-str basic-explains-str web-str)))
      (list query back)))

  (defun anki-add-card (deck front back)
    "Add anki basic card which contains FRONT and BACK elements to the DECK."
    (let* ((req-params (list `("note" . ,(list `("deckName" . ,deck)
                                               '("modelName" . "Basic")
                                               `("fields" . ,(list `("Front" . ,front)
                                                                   `("Back" . ,back)))
                                               `("options" . ,(list '("closeAfterAdding" . t)))
                                               `("tags" . ,(list "Emacs")))))))
      (request (format "%s:%s" anki-connect-host anki-connect-port)
        :type "POST"
        :data (json-encode (list '("action" . "guiAddCards")
                                 '("version" . 6)
                                 `("params" . ,req-params)))
        :headers '(("Content-Type" . "text/json"))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (message "result: %S" (assoc-default 'result data)))))))

  ;; saved for test
  (defun anki-test (word)
    (let* ((json (youdao-dictionary--request word))
           (res (hiro-format-youdao-result json))
           (front (car res))
           (back (car (cdr res))))
      (progn
        (anki-add-card anki-deck-name front back)
        )))

  ;; (anki-test "anki")

  (defun anki-add-current-word-card ()
    "Add current word to anki card."
    (interactive)
    (let* ((word (hiro-region-or-word))
           (json (youdao-dictionary--request word))
           (res (hiro-format-youdao-result json))
           (front (car res))
           (back (car (cdr res))))
      (anki-add-card anki-deck-name front back)))

  (defun anki-add-yanked-word-card ()
    "Add yanked word to anki card."
    (interactive)
    (let* ((word (current-kill 0 t))
           (json (youdao-dictionary--request word))
           (res (hiro-format-youdao-result json))
           (front (car res))
           (back (car (cdr res))))
      (anki-add-card anki-deck-name front back)))

  ;; (global-set-key (kbd "C-c u") 'anki-add-current-word-card)
  ;; (global-set-key (kbd "C-c t") 'anki-add-yanked-word-card)
  )

;;; LSP config

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

;;; Treesitter

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs)

;;; Miscellaneous config

(use-package ledger-mode
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  :mode "\\.dat\\'")

(use-package which-key
  :config
  (which-key-mode t)
  (which-key-setup-side-window-bottom))

(use-package all-the-icons)

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
  (super-save-mode 1))

(use-package go-translate
  :commands (gts-buffer-render)
  :bind (("C-c t g" . gts-do-translate)
         ("C-c t p" . go-translate-at-point)
         ("C-c t s" . go-translate-save-kill-ring))
  :config
  ;; HACK: https://github.com/lorniu/go-translate/issues/31
  (cl-defmethod gts-out :after ((_ gts-buffer-render) _)
    (with-current-buffer gts-buffer-name
      (read-only-mode 1)
      (variable-pitch-mode 1)
      (if (featurep 'sis)
          (sis-set-english))))

  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine))
         :render (gts-buffer-render)))

  ;; Pick directly and use Google RPC API to translate
  (defun go-translate-at-point ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-rpc-engine)
                    :render (gts-buffer-render))))

  ;; Pick directly and add the results into kill-ring
  (defun go-translate-save-kill-ring ()
    (interactive)
    (gts-translate (gts-translator
                    :picker (gts-noprompt-picker)
                    :engines (gts-google-engine
                              :parser (gts-google-summary-parser))
                    :render (gts-kill-ring-render)))))

(use-package flyspell
  :diminish
  :if (and (executable-find "aspell") *spell-check-support-enabled*)
  :hook ((prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case"))
  (ispell-personal-dictionary (expand-file-name "en_US.personal" "~/.config/aspell/")))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-," . flyspell-correct-wrapper)))

(use-package posframe)

(use-package rime
  :bind (("C-`" . rime-send-keybinding)
         ("C-\\" . toggle-input-method))
  :hook (post-command . rime-change-cursor-color)
  :custom
  (default-input-method "rime")
  (rime-title "")
  :init
  (when *is-a-mac*
    (setq rime-librime-root "~/.emacs.d/librime/dist"))
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

;;; Built-in packages

(use-package eldoc
  :custom (eldoc-echo-area-use-multiline-p nil))

(use-package help
  :defer t
  :custom (help-window-select t)
  :config (temp-buffer-resize-mode))

;;; Pass for password storage

(use-package pass)

;;; Configure default locale

(progn ; `charset'
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq system-time-locale "C")
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8)))

;;; Tequila worms

(progn ; `startup'
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(setq-default line-spacing 0.2)

(progn ; personalize
  (let ((file (expand-file-name "private.el" user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; init.el ends here