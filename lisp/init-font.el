;; -*- lexical-binding: t; Font Configuration -*-

(defun font-installed-p (font-name)
  (member font-name (font-family-list)))

(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Iosevka Nerd Font" "Lilex" "Comic Mono" "Cascadia Code" "Hack")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 140))

    (cl-loop for font in '("Iosevka Nerd Font")
             when (font-installed-p font)
             return (set-face-attribute 'variable-pitch nil
                                        :family font
                                        :height 140))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
             when (font-installed-p font)
             return (if (>= emacs-major-version 28)
                        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW WenKai" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.1)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;  (set-fontset-font (frame-parameter nil 'font)
;		    charset (font-spec :family "LXGW WenKai" :size 28)))

;;; nerd icon font
(use-package nerd-icons)

(provide 'init-font)
;;; init-font.el ends here
