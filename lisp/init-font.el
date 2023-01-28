;; -*- lexical-binding: t; Font Configuration -*-

(defun hiro/load-font (font-size)
  "Load font with `font-size'."
  (when (display-graphic-p)
    (set-face-attribute
     'default nil
     :font (font-spec :name "-*-Iosevka-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                      :weight 'normal
                      :slant 'normal
                      :size font-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :name "-*-LXGW WenKai-regular-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                  :weight 'normal
                  :slant 'normal)))
    (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")))

;; default fonts: Lation Modern Mono
;; 中英文测试
;; aaBBCcDDee

(setq hiro/font-size 20)

(hiro/load-font hiro/font-size)

(provide 'init-font)
;;; init-font.el ends here
