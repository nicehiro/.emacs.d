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
  ;; (ef-themes-select 'ef-cyprus)
  )

(use-package standard-themes
  :config
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t
        standard-themes-mode-line-accented nil

        ;; Accepts a symbol value:
        standard-themes-fringes 'subtle

        ;; The following accept lists of properties
        standard-themes-links '(neutral-underline)
        standard-themes-region '(no-extend neutral intense)
        standard-themes-prompts '(bold italic)

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified):
        standard-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch light 1.7))
          (3 . (variable-pitch semilight 1.6))
          (4 . (variable-pitch semilight 1.5))
          (5 . (variable-pitch 1.4))
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes)

;; add custom themes
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(load-theme 'nord :t)

(provide 'init-theme)
;;; init-theme.el ends here
