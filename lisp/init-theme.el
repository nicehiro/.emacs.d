;; -*- lexical-binding: t; -*-

;; https://www.reddit.com/r/emacs/comments/1u5l17/always_black_cursor_in_emacsclient/
;; There's both a cursor-color frame parameter and a cursor face.
;; One of these settings may be overriding the other.
;; Most themes will set the cursor face appropriately for
;; dark/light frame background colours, so you might consider
;; explicitly setting the cursor-color to nil so that you can
;; rely on the theme's settings
(add-to-list 'default-frame-alist '(cursor-color . nil))

(use-package ef-themes
  :load-path "lib/ef-themes"
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-deuteranopia-light))
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))

  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  (setq ef-themes-region '(intense no-extend neutral))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':

  ;; modeline
  (defun hiro/ef-theme-mode-line ()
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c
                     :background ,bg-mode-line
                     :box (:line-width 10 :color ,bg-mode-line))))
       `(mode-line-inactive ((,c
                              :background ,bg-completion
                              :box (:line-width 10 :color ,bg-completion)))))))
  (add-hook 'ef-themes-post-load-hook #'hiro/ef-theme-mode-line)

  (ef-themes-select 'ef-rosa)
  )

(use-package doom-themes)

;; add custom themes
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(provide 'init-theme)
;;; init-theme.el ends here
