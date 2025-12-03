;;; -*- lexical-binding: t -*-

;; Early load org-mode
;; for better org-latex-preview
(use-package org
  :defer
  :straight `(org
              :fork (:host nil
                     :repo "https://git.tecosaur.net/tec/org-mode.git"
                     :branch "dev"
                     :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              :pin nil))

(straight-use-package 'diminish)
(require 'org)
(require 'diminish)

;; Define helper command for reloading configuration
(defun meomacs-refresh ()
  "Refresh and tangle configuration."
  (interactive)
  (meomacs-load-config "private" t)
  (meomacs-load-config "laf" t)
  (meomacs-load-config "editor" t)
  (meomacs-load-config "writing" t)
  (meomacs-load-config "programming" t)
  (meomacs-load-config "addons" t)
  (meomacs-load-config "research" t))

;; Define helper command for open configuration file.
(defun meomacs-open-configuration ()
  "Open meomacs.org under `user-emacs-directory'."
  (interactive)
  (let ((config (completing-read "Open configuration: "
				 '("private"
				   "laf"
				   "editor"
				   "writing"
				   "programming"
				   "addons"
                                   "research")
				 nil
				 t)))
    (find-file (expand-file-name (format "%s.org" config) user-emacs-directory))))

;; Define helper macro to parse key binding tables
(defmacro meomacs-keymap-table (keymap table)
  `(progn
     (unless (boundp (quote ,keymap))
       (defvar ,keymap (make-keymap)))
     (let ((parse-and-def (lambda (x)
                            (keymap-set ,keymap (car x) (intern (cadr x))))))
       (mapcar parse-and-def ,table))
     (defalias (quote ,keymap) ,keymap)))

(global-set-key (kbd "<f9>") 'meomacs-open-configuration)
(global-set-key (kbd "<f12>") 'meomacs-refresh)

;; Load main configuration
(meomacs-load-config "laf")
(meomacs-load-config "editor")
(meomacs-load-config "writing")
(meomacs-load-config "programming")
(meomacs-load-config "addons" t)
(meomacs-load-config "research" t)
(meomacs-load-config "private" t)
