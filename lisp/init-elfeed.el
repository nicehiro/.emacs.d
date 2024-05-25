;; -*- lexical-binding: t; -*-

(use-package elfeed
  :config
  (setq elfeed-feeds
        '(
          ("https://rss.arxiv.org/rss/cs.RO" research)
          )
        )
  (setq-default elfeed-search-filter "@1-years-old "))

(use-package elfeed-score
  :config
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (elfeed-score-serde-load-score-file "~/.config/emacs/elfeed.score")

  (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)
  )

(provide 'init-elfeed)
;;; init-elfeed.el ends here
