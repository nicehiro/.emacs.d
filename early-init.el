;;; early-init.el --- earliest birds  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives
	       (cons "melpa" "https://melpa.org/packages/")
	       t))

(setq frame-inhibit-implied-resize t)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;;; End:
;;; early-init.el ends here
