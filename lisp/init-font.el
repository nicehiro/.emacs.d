;; -*- lexical-binding: t; Font Configuration -*-

(defvar meomacs-font-size 20
  "Current font size.")

(defvar meomacs-fonts '((default . "Menlo")
                        (cjk . "LXGW WenKai Mono")
                        (symbol . "Noto Sans Symbols")
                        (fixed . "Hack")
                        (fixed-serif . "Hack")
                        (variable . "Hack")
                        (wide . "Menlo")
                        (tall . "Menlo"))
  "Fonts to use.")
(defun meomacs--get-font-family (key)
  (let ((font (alist-get key meomacs-fonts)))
    (if (string-empty-p font)
        (alist-get 'default meomacs-fonts)
      font)))

(defun meomacs-load-default-font ()
  "Load default font configuration."
  (let ((default-font (format "%s-%s"
                              (meomacs--get-font-family 'default)
                              meomacs-font-size)))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun meomacs-load-face-font ()
  "Load face font configuration."
  (let ((variable-font (meomacs--get-font-family 'variable))
        (fixed-font (meomacs--get-font-family 'fixed))
        (fixed-serif-font (meomacs--get-font-family 'fixed-serif)))
    (set-face-attribute 'variable-pitch nil :family variable-font)
    (set-face-attribute 'fixed-pitch nil :family fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font)))

(defun meomacs-load-charset-font (&optional font)
  "Load charset font configuration."
  (let ((default-font (or font (format "%s-%s"
                                       (meomacs--get-font-family 'default)
                                       meomacs-font-size)))
        (cjk-font (meomacs--get-font-family 'cjk))
        (symbol-font (meomacs--get-font-family 'symbol)))
    (set-frame-font default-font)
    (let ((fontset (create-fontset-from-ascii-font default-font)))
      ;; Fonts for charsets
      (dolist (charset '(kana han hangul cjk-misc bopomofo))
        (set-fontset-font fontset charset cjk-font))
      (set-fontset-font fontset 'symbol symbol-font)
      ;; Apply fontset
      (set-frame-parameter nil 'font fontset)
      (add-to-list 'default-frame-alist (cons 'font fontset)))))

(meomacs-load-default-font)
(meomacs-load-face-font)
(meomacs-load-charset-font)

;; Run after startup
(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (meomacs-load-charset-font))))

;; (add-to-list 'default-frame-alist '(font .  "Iosevka-16" ))
;; (set-face-attribute 'default t :font  "Iosevka-16" )

(provide 'init-font)
;;; init-font.el ends here
