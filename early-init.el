(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil))
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;;; early-init.el ends here
