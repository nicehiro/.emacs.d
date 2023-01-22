;; -*- lexical-binding: t; Font Configuration -*-

;; don't use anymore
(defun hiro/load-font (font-size)
  "Load font with `font-size'."
  (when (display-graphic-p)
    (set-face-attribute
     'default nil
     :font (font-spec :name "-*-Iosevka Slab-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                      :weight 'normal
                      :slant 'normal
                      :size font-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :name "-*-LXGW WenKai-regular-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                  :weight 'normal
                  :slant 'normal)))))

;; default fonts: Lation Modern Mono
;; 中英文测试
;; aaBBCcDDee

(defvar meomacs-font-size 20
  "Current font size.")

(defvar meomacs-fonts '((default . "Fantasque Sans Mono")
                        (cjk . "LXGW WenKai")
                        (symbol . "symbol")
                        (fixed . "Sarasa Fixed SC")
                        (fixed-serif . "Sarasa Fixed Slab SC")
                        (variable . "Georgia")
                        (wide . "Futura")
                        (tall . "Iosevka"))
  "Fonts to use.")

(defun meomacs--get-font-family (key)
  (alist-get key meomacs-fonts
             (alist-get 'default meomacs-fonts)))

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

;; Run after startup
(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (meomacs-load-charset-font))))

(provide 'init-font)
;;; init-font.el ends here
