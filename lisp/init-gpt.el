;; -*- lexical-binding: t; -*-

(use-package gptel
  :load-path "lib/gptel"
  :config
  (defun hiro/get-gpt-api-key ()
    (replace-regexp-in-string "[[:space:]\n]+"
                              ""
                              (with-temp-buffer
                                (insert-file-contents "~/secrets/gpt-api.txt")
                                (buffer-string))))
  (setq gptel-api-key (hiro/get-gpt-api-key)))

(provide 'init-gpt)
;;; init-gpt.el ends here
