;; -*- lexical-binding: t; -*-

(use-package telega
  :config
  (setq telega-server-libs-prefix "/usr")
  (setq telega-chat-fill-column 65)
  (setq telega-chat-input-markups '("org"))
  (telega-autoplay-mode 1)

  (defmacro lucius/telega-ins--aux-inline-reply (&rest body)
    `(telega-ins--aux-inline
         "󱞫" 'telega-msg-inline-reply
         ,@body))

  ;; shorten url
  (setq telega-url-shorten-regexps
        ;; telega-url-shorten
        (list `(too-long-link
                :regexp "^\\(https?://\\)\\(.\\{55\\}\\).*?$"
                :symbol ""
                :replace "\\1\\2...")))

  ;; symbols
  (setq telega-symbol-checkmark ""
        telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
        telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
        telega-symbol-forward (nerd-icons-mdicon "nf-md-comment_arrow_right_outline")
        telega-symbol-heavy-checkmark "󰄭"
        telega-symbol-right-arrow (nerd-icons-codicon "nf-cod-arrow_right")
        telega-symbol-reaction (nerd-icons-mdicon "nf-md-heart_circle"))
  (setq word-wrap-by-category t)

  )

(provide 'init-telega)
;;; init-telega.el ends here
