;; -*- lexical-binding: t; -*-

(use-package vterm
  :init
  ;; Granted, I seldom pop out to the shell except during code demonstrations,
  ;; but I like how C-p/C-n jumps up to each prompt entry using this setting
  ;; that works with my prompt:
  (setq vterm-use-vterm-prompt-detection-method nil
        term-prompt-regexp "^.* $ ")
  :config
  (dolist (k '("<C-backspace>" "<M-backspace>"))
    (define-key vterm-mode-map (kbd k)
                (lambda () (interactive) (vterm-send-key (kbd "C-w")))))

  ;; Enter copy mode? Go to Evil's normal state to move around:
  (advice-add 'vterm-copy-mode :after 'evil-normal-state)

  ;; I don't know if I need any of these ... yet. Because when I am in a shell,
  ;; I default to Emacs keybindings...
  ;; (setq vterm-keymap-exceptions nil)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)
  :hook
  (vterm-mode . (lambda ()
                  (setq-local evil-insert-state-cursor 'box)
                  (evil-insert-state))))

(use-package multi-vterm)

(provide 'init-vterm)
;;; init-vterm.el ends here
