;; -*- lexical-binding: t; -*-

(use-package ox-hugo
  :after org
  :config
  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "img"))

;; Heavily modified based on https://github.com/novoid/title-capitalization.el/blob/master/title-capitalization.el
(defun title-capitalization (str)
  "Convert str to title case"
  (interactive)
  (with-temp-buffer
    (insert str)
    (let* ((beg (point-min))
           (end (point-max))
	   ;; Basic list of words which don't get capitalized according to simplified rules
	   ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
                                            "from" "in" "into" "it" "next" "nor" "of" "off"
                                            "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"
                                            "n" "t" "es" "s"))
	   ;; If user has defined 'my-do-not-capitalize-words, append to basic list
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
                                      do-not-capitalize-basic-words)))
      ;; Go to begin of first word
      (goto-char beg)
      (setq continue t)

      ;; Go through the region, word by word
      (while continue
        (let ((last-point (point)))
          (let ((word (thing-at-point 'word)))
            (if (stringp word)
                ;; Capitalize current word except when it is list member
                (if (and (member (downcase word) do-not-capitalize-words)
                         ;; Always capitalize first word
                         (not (= (point) 1)))
                    (downcase-word 1)

                  ;; If it's an acronym, don't capitalize
                  (if (string= word (upcase word))
                      (progn
                        (goto-char (+ (point) (length word) 1)))
                    (capitalize-word 1)))))

          (skip-syntax-forward "^w" end)

          ;; Break if we are at the end of the buffer
          (when (= (point) last-point)
            (setq continue nil))))

      ;; Always capitalize the last word
      (backward-word 1)

      (let ((word (thing-at-point 'word)))
        (if (and (>= (point) 0)
                 (not (member (or word "s")
                              '("n" "t" "es" "s")))
                 (not (string= word (upcase word))))
            (capitalize-word 1))))

    (buffer-string)))

(eval-when-compile
  (require 'cl))

(use-package org-roam
  :diminish
  :bind (("C-c n a" . org-id-get-create)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n R" . org-roam-ref-add)
         ("C-c n s" . org-roam-db-sync))
  :custom
  ;; (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory (file-truename "~/Documents/roam/org-roam/"))
  (org-roam-db-location "~/Documents/roam/org-roam/roam.db")
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-enable)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  :init
  (setq org-id-extra-files (directory-files-recursively "~/Documents/roam/org-roam/" "org"))
  (setq org-roam-v2-ack t)
  (setq org-roam-publish-path "~/Documents/nicehiro.github.io/")
  ;; These functions need to be in :init otherwise they will not be
  ;; callable in an emacs --batch context which for some reason
  ;; can't be found in autoloads if it's under :config
  (defun my/org-roam--extract-note-body (file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (first (org-element-map (org-element-parse-buffer) 'paragraph
               (lambda (paragraph)
                 (let ((begin (plist-get (first (cdr paragraph)) :begin))
                       (end (plist-get (first (cdr paragraph)) :end)))
                   (buffer-substring begin end)))))))

  (defun extract-date-info (date-string)
    "Extract year, month, and day from DATE-STRING in the format YYYYMMDDHHMMSS."
    (let ((year (substring date-string 0 4))
          (month (substring date-string 4 6))
          (day (substring date-string 6 8)))
      (concat year "-" month "-" day)))

  ;; Include backlinks in org exported notes not tagged as private or
  ;; draft or section
  (defun my/org-roam--backlinks-list (id file)
    (--reduce-from
     (concat acc (format "- [[id:%s][%s]]\n  #+begin_quote\n  %s\n  #+end_quote\n"
                         (car it)
                         (title-capitalization (org-roam-node-title (org-roam-node-from-id (car it))))
                         (my/org-roam--extract-note-body (org-roam-node-file (org-roam-node-from-id (car it))))))
     ""
     (org-roam-db-query
      (format
       "SELECT id FROM (SELECT links.source AS id, group_concat(tags.tag) AS alltags FROM links LEFT OUTER JOIN tags ON links.source = tags.node_id WHERE links.type = '\"id\"' AND links.dest = '\"%s\"' GROUP BY links.source) Q WHERE alltags IS NULL OR (','||alltags||',' NOT LIKE '%%%%,\"private\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"draft\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"section\",%%%%')"
       id))
     ))

  (defun file-path-to-md-file-name (path)
    (let ((file-name (first (last (split-string path "/")))))
      (concat (first (split-string file-name "\\.")) ".md")))

  (defun file-path-to-slug (path)
    (let* ((file-name (file-name-nondirectory path))
           (note-name (car (last (split-string file-name "--"))))
           (title (first (split-string note-name "\\."))))
      (replace-regexp-in-string (regexp-quote "_") "-" title nil 'literal)))

  ;; Org export is very slow when processing org-id links. Override it
  ;; to skip opening the file and loading all modes.
  (defun my/org-export--collect-tree-properties (data info)
    "Extract tree properties from parse tree.

    DATA is the parse tree from which information is retrieved.  INFO
    is a list holding export options.

    Following tree properties are set or updated:

    `:headline-offset' Offset between true level of headlines and
                       local level.  An offset of -1 means a headline
                       of level 2 should be considered as a level
                       1 headline in the context.

    `:headline-numbering' Alist of all headlines as key and the
                          associated numbering as value.

    `:id-alist' Alist of all ID references as key and associated file
                as value.

    Return updated plist."
    ;; Install the parse tree in the communication channel.
    (setq info (plist-put info :parse-tree data))
    ;; Compute `:headline-offset' in order to be able to use
    ;; `org-export-get-relative-level'.
    (setq info
          (plist-put info
                     :headline-offset
                     (- 1 (org-export--get-min-level data info))))
    ;; From now on, properties order doesn't matter: get the rest of the
    ;; tree properties.
    (org-combine-plists
     info
     (list :headline-numbering (org-export--collect-headline-numbering data info)
           :id-alist
           (org-element-map data 'link
             (lambda (l)
               (and (string= (org-element-property :type l) "id")
                    (let* ((id (org-element-property :path l))
                           (file (org-id-find-id-file id)))
                      (and file (cons id (file-relative-name file))))))))))

  (advice-add 'org-export--collect-tree-properties :override #'my/org-export--collect-tree-properties)

  ;; No notes use anchor links so ignore this to speed it up
  (defun my/org-hugo-link--headline-anchor-maybe (link)
    "")
  (advice-add 'org-hugo-link--headline-anchor-maybe :override #'my/org-hugo-link--headline-anchor-maybe)

  ;; ox-hugo doesn't set the `relref` path correctly so we need to
  ;; tell it how to do it
  (defun my/org-id-path-fix (strlist)
    (file-name-nondirectory strlist))

  (advice-add 'org-export-resolve-id-link :filter-return #'my/org-id-path-fix)

  ;; Fetches all org-roam files and exports to hugo markdown
  ;; files. Adds in necessary hugo properties
  ;; e.g. HUGO_BASE_DIR. Ignores notes tagged as private or draft
  (defun org-roam-to-hugo-md ()
    (interactive)
    ;; Make sure the author is set
    (setq user-full-name "Fangyuan")

    ;; Don't include any files tagged as private or
    ;; draft. The way we filter tags doesn't work nicely
    ;; with emacsql's DSL so just use a raw SQL query
    ;; for clarity
    (let ((notes (org-roam-db-query "SELECT id, file FROM (SELECT nodes.id, nodes.file, group_concat(tags.tag) AS alltags FROM nodes LEFT OUTER JOIN tags ON nodes.id = tags.node_id GROUP BY nodes.file) WHERE alltags is null or (','||alltags||',' not like '%%,\"private\",%%' and ','||alltags||',' not like '%%,\"draft\",%%')")))
      (-map
       (-lambda ((id file))
         ;; Use temporary buffer to prevent a buffer being opened for
         ;; each note file.
         (with-temp-buffer
           (message file)
           (insert-file-contents file)

           ;; Change relative links that work within emacs to view an image to
           ;; absolute paths for use with exported markdown to html.
           (goto-char (point-min))

           (while (re-search-forward "\\(\\[\\[file:\\)\\(img/[^]]+\\)\\(\\]\\]\\)" nil t)
             (replace-match "\\1/\\2\\3" nil nil))

           ;; Adding these tags must go after file content because it
           ;; will include a :PROPERTIES: drawer as of org-roam v2
           ;; which must be the first item on the page

           ;; Add in hugo tags for export. This lets you write the
           ;; notes without littering HUGO_* tags everywhere
           ;; HACK:
           ;; org-export-output-file-name doesn't play nicely with
           ;; temp buffers since it attempts to get the file name from
           ;; the buffer. Instead we explicitely add the name of the
           ;; exported .md file otherwise you would get prompted for
           ;; the output file name on every note.


           (goto-char (point-min))
           (re-search-forward ":END:")
           (newline)
           (insert
            (format "#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: ./\n#+HUGO_SLUG: %s\n#+EXPORT_FILE_NAME: %s\n#+date: %s"
                    org-roam-publish-path
                    (file-path-to-slug file)
                    (file-path-to-md-file-name file)
                    (extract-date-info (file-path-to-slug file))))

           ;; If this is a placeholder note (no content in the
           ;; body) then add default text. This makes it look ok when
           ;; showing note previews in the index and avoids a headline
           ;; followed by a headline in the note detail page.
           (if (eq (my/org-roam--extract-note-body file) nil)
               (progn
                 (goto-char (point-max))
                 (insert "\n/This note does not have a description yet./\n")))

           ;; Add in backlinks (at the end of the file) because
           ;; org-export-before-processing-hook won't be useful the
           ;; way we are using a temp buffer
           (let ((links (my/org-roam--backlinks-list id file)))
             (if (not (string= links ""))
                 (progn
                   (goto-char (point-max))
                   (insert (concat "\n* Links to this note\n") links))))
           (org-hugo-export-to-md)
           ))
       notes)))
  )

(use-package org-roam-dailies
  :load-path "lib/org-roam/extensions/")

(use-package org-roam-ui
  :config
  (setq
   ;; org-roam-ui-sync-theme t
   org-roam-ui-follow t
   org-roam-ui-update-on-save t
   ;; org-roam-ui-open-on-start t
   ))

(use-package consult-org-roam
  :after org-roam consult
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(provide 'init-roam)
;;; init-roam.el ends here
