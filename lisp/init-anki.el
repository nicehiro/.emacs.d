;; -*- lexical-binding: t; -*-

(progn ; anki
  (require 'request)
  ;;; anki-connect default settings
  (defvar anki-connect-host "127.0.0.1"
    "Anki connect server host.")

  (defvar anki-connect-port "8765"
    "Anki connect server port.")

  (defvar anki-deck-name "Words"
    "Shengci in anki deck name.")

;;; copied from youdao-dictionary
  (defun hiro-region-or-word ()
    "Return word in region or word at point."
    (if (derived-mode-p 'pdf-view-mode)
        (if (pdf-view-active-region-p)
            (mapconcat 'identity (pdf-view-active-region-text) "\n"))
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning)
                                          (region-end))
        (thing-at-point 'word t))))


  (defun hiro-htmlize-ul (elements)
    "Htmlize ELEMENTS to <ul> element."
    (concat "<ul>"
            (mapconcat
             (lambda (element) (concat "<li>" element "</li>"))
             elements
             "<br>")
            "</ul>"))

  (defun hiro-htmlize-anki-back (translation basic-explains web-refs)
    (format "<h3>Translation</h3>%s<h3>Basic Explainations</h3>%s<h3>Web References</h3>%s"
            translation
            basic-explains
            web-refs))

;;; most of this copied from youdao-dictionary
  (defun hiro-format-youdao-result (json)
    "Format result in youdao-dictionary JSON."
    (let* ((query (assoc-default 'query json))
           (translation (assoc-default 'translation json))
           (_errorCode (assoc-default 'errorCode json))
           (web (assoc-default 'web json))
           (basic (assoc-default 'basic json))
           (phonetic (assoc-default 'phonetic basic))
           (translation-str (hiro-htmlize-ul translation))
           (basic-explains-str (hiro-htmlize-ul (assoc-default 'explains basic)))
           (web-str (concat "<ul>"
                            (mapconcat
                             (lambda (k-v)
                               (format "<li> %s :: %s </li>"
                                       (assoc-default 'key k-v)
                                       (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                             web
                             "")
                            "</ul>"))
           ;; (back (concat translation-str basic-explains-str web-str)))
           (back (hiro-htmlize-anki-back translation-str basic-explains-str web-str)))
      (list query back)))

  (defun anki-add-card (deck front back)
    "Add anki basic card which contains FRONT and BACK elements to the DECK."
    (let* ((req-params (list `("note" . ,(list `("deckName" . ,deck)
                                               '("modelName" . "Basic")
                                               `("fields" . ,(list `("Front" . ,front)
                                                                   `("Back" . ,back)))
                                               `("options" . ,(list '("closeAfterAdding" . t)))
                                               `("tags" . ,(list "Emacs")))))))
      (request (format "%s:%s" anki-connect-host anki-connect-port)
        :type "POST"
        :data (json-encode (list '("action" . "guiAddCards")
                                 '("version" . 6)
                                 `("params" . ,req-params)))
        :headers '(("Content-Type" . "text/json"))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (message "result: %S" (assoc-default 'result data)))))))

  ;; saved for test
  (defun anki-test (word)
    (let* ((json (youdao-dictionary--request word))
           (res (hiro-format-youdao-result json))
           (front (car res))
           (back (car (cdr res))))
      (progn
        (anki-add-card anki-deck-name front back)
        )))

  ;; (anki-test "anki")

  (defun anki-add-current-word-card ()
    "Add current word to anki card."
    (interactive)
    (let* ((word (hiro-region-or-word))
           (json (youdao-dictionary--request word))
           (res (hiro-format-youdao-result json))
           (front (car res))
           (back (car (cdr res))))
      (anki-add-card anki-deck-name front back)))

  (defun anki-add-yanked-word-card ()
    "Add yanked word to anki card."
    (interactive)
    (let* ((word (current-kill 0 t))
           (json (youdao-dictionary--request word))
           (res (hiro-format-youdao-result json))
           (front (car res))
           (back (car (cdr res))))
      (anki-add-card anki-deck-name front back)))

  ;; (global-set-key (kbd "C-c u") 'anki-add-current-word-card)
  ;; (global-set-key (kbd "C-c t") 'anki-add-yanked-word-card)
  )

(provide init-anki)
;;; init-anki.el ends here
