;; -*- lexical-binding: t; -*-

(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-select-tab-modifiers "super")
  (setq tab-bar-tab-hints t)

  (global-set-key (kbd "C-c g g") 'tab-bar-switch-to-recent-tab)
  (global-set-key (kbd "C-c g t") 'tab-bar-switch-to-next-tab)
  (global-set-key (kbd "C-c g T") 'tab-bar-switch-to-prev-tab)

  (defvar ct/circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")

  (defun ct/tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 10))
                       (alist-get i ct/circle-numbers-alist) "")))
      (propertize
       (concat tab-num
               " "
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")
               " ")
       'face (funcall tab-bar-tab-face-function tab))))
  (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)

  ;; Copied from https://mmk2410.org/2022/02/11/using-emacs-tab-bar-mode/
  (defun mmk2410/tab-bar-tab-exists (name)
    "Check if tab-bar `name' exist."
    (member name
	    (mapcar #'(lambda (tab) (alist-get 'name tab))
		    (tab-bar-tabs))))

  (defun mmk2410/tab-bar-new-tab (name func)
    "Create new tab-bar `name' with buffer created by `func'."
    (when (eq nil tab-bar-mode)
      (tab-bar-mode))
    (tab-bar-new-tab)
    (tab-bar-rename-tab name)
    (funcall func))

  (defun mmk2410/tab-bar-switch-or-create (name func)
    "Create new tab-bar if `name' not exist, otherwise switch to it."
    (if (mmk2410/tab-bar-tab-exists name)
        (tab-bar-switch-to-tab name)
      (mmk2410/tab-bar-new-tab name func)))

  (defun hiro/tab-bar-run-agenda ()
    "Create or switch to Agenda tab-bar."
    (interactive)
    (mmk2410/tab-bar-switch-or-create
     "Agenda"
     #'(lambda ()
         (org-agenda nil "d"))))

  (defun hiro/tab-bar-run-elfeed ()
    "Create or switch to Elfeed tab-bar."
    (interactive)
    (mmk2410/tab-bar-switch-or-create "RSS" #'elfeed))

  (defun hiro/tab-bar-run-write ()
    "Create or switch to Write tab-bar."
    (interactive)
    (mmk2410/tab-bar-switch-or-create "Write" #'scratch-buffer)))

(provide 'init-tab)
;;; init-tab.el ends here
