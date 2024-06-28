;; -*- lexical-binding: t; -*-

(use-package gptel
  :load-path "lib/gptel"
  :config
  (defun hiro/get-gpt-api-key ()
    (replace-regexp-in-string "[[:space:]\n]+"
                              ""
                              (with-temp-buffer
                                (insert-file-contents "~/Documents/keys/azure/gpt-api.txt")
                                (buffer-string))))
  (setq-default gptel-backend
                (gptel-make-azure "Azure GPT"             ;Name, whatever you'd like
                  :protocol "https"                     ;Optional -- https is the default
                  :host "fygpt.openai.azure.com"
                  :endpoint "/openai/deployments/gpt-4o/chat/completions?api-version=2023-09-01-preview" ;or equivalent
                  :stream t                             ;Enable streaming responses
                  :key #'hiro/get-gpt-api-key
                  :models '("gpt-4o"))
                gptel-model "gpt-4o"
                gptel-default-mode #'org-mode))

(provide 'init-gpt)
;;; init-gpt.el ends here
