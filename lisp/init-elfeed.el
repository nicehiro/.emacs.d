;; -*- lexical-binding: t; -*-

(use-package elfeed
  :config
  (setq-default elfeed-search-filter "@1-years-old +unread")
  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)))

(use-package elfeed-protocol
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-curl-extra-arguments '("--insecure"))
  (setq elfeed-protocol-fever-fetch-category-as-tag t)

  (defun hiro/get-fever-pwd ()
    (replace-regexp-in-string "[[:space:]\n]+"
                              ""
                              (with-temp-buffer
                                (insert-file-contents "~/Documents/keys/fever/pwd.txt")
                                (buffer-string))))

  (setq elfeed-protocol-feeds `(("fever+https://fy@freshrss.nicehiro.xyz"
                                 :api-url "https://freshrss.nicehiro.xyz/api/fever.php"
                                 :password ,(hiro/get-fever-pwd))))
  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-enable))

(use-package elfeed-score
  :config
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (elfeed-score-serde-load-score-file "~/.config/emacs/elfeed.score")

  (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
  )

(provide 'init-elfeed)
;;; init-elfeed.el ends here
