;;; early-init.el --- earliest birds  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives
	       (cons "melpa" "https://melpa.org/packages/")
	       t))

(setq frame-inhibit-implied-resize t)


;;; Font setting
(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("Iosevka" . 16) ("Roboto Mono" . 16) ("Operator Mono SSm" . 16) ("SF Mono" . 16) ("Monaco" . 16) ("Menlo" . 16)))
   ((eq system-type 'windows-nt)
    '(("SF Mono" . 16) ("Consolas" . 16) ("Cascadia Mono" . 16)))
   (t
    '(("SF Mono" . 16) ("Consolas" . 16) ("Cascadia Mono" . 16))))
  "List of fonts and sizes.  The first one available will be used.")

;; Set default font before frame creation to make sure the first frame have the correct size
(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))

(defun font-installed-p (font)
  "Check if the FONT is available."
  (find-font (font-spec :name font)))

(defun change-font ()
  "Change the font of frame from an available `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size font-set)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (font-installed-p (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
                                       available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-set (format "%s-%d" font-name font-size))
      (set-frame-font font-set nil t)
      (add-to-list 'default-frame-alist (cons 'font font-set)))))

(defun change-unicode-font ()
  "Setup the Unicode font."
  (when window-system
    (cl-loop for font in '("Microsoft Yahei" "PingFang SC" "Noto Sans Mono CJK SC")
             when (font-installed-p font)
             return (dolist (charset '(kana han hangul cjk-misc bopomofo))
                      (set-fontset-font t charset font)))
    (cl-loop for font in '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'append))
    (dolist (font '("HanaMinA" "HanaMinB"))
      (when (font-installed-p font)
        (set-fontset-font t 'unicode font nil 'append)))))

;; Run after startup
(dolist (fn '(change-font change-unicode-font))
  (add-hook 'after-init-hook fn))

;;; End:
;;; early-init.el ends here
