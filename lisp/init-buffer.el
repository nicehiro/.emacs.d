;; -*- lexical-binding: t; -*-

(defun indicate-buffer-boundaries-left ()
  (setq indicate-buffer-boundaries 'left))
(add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left)
(add-hook 'text-mode-hook 'indicate-buffer-boundaries-left)

(bind-keys
 ("C-x k" . kill-current-buffer)
 ("C-x x p" . pop-to-mark-command)
 ("C-x C-." . pop-global-mark)
 ;; M-^ is inconvenient, so also bind M-j
 ("M-j" . join-line)
 ;; Zap *up* to char is a handy pair for zap-to-char
 ("M-Z" . zap-up-to-char))

(provide 'init-buffer)
;;; init-buffer.el ends here
