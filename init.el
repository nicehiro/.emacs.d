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
  (setq-default line-spacing 0.2)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-window-scroll-bars
               (minibuffer-window frame) 0 nil 0 nil t)))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 1))
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
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

;; (use-package epkg)

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'init-theme)
(require 'init-font)
(require 'init-evil)
(require 'init-dired)
(require 'init-modeline)
;; (require 'init-flymake)
(require 'init-minibuffer)
(require 'init-complete)
(require 'init-window)
(require 'init-tab)
(require 'init-vc)
(require 'init-edit)
(require 'init-org)
(require 'init-roam)
(require 'init-blog)
(require 'init-tex)
(require 'init-yas)
(require 'init-elisp)
(require 'init-python)
(require 'init-lsp)
(require 'init-elfeed)
;; (require 'init-rime)
(require 'init-doc)
(require 'init-hledger)
(require 'init-utils)

;;; Configure default locale
(progn ; `charset'
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq system-time-locale "C")
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8)))

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

(progn ; personalize
  (let ((file (expand-file-name "private.el" user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; init.el ends here
(put 'upcase-region 'disabled nil)
