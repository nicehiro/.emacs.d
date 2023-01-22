;; -*- lexical-binding: t; -*-

(progn ; `basic-variables'
  (defconst hiro/bib-libraries '("~/Documents/research/references.bib"))
  (defconst hiro/main-bib-library (nth 0 hiro/bib-libraries))
  (defconst hiro/pdf-libraries '("~/Documents/research/pdfs/"))
  (defconst hiro/main-pdf-library (nth 0 hiro/pdf-libraries))
  (defconst hiro/note-libraries '("~/Documents/roam/org-roam/"))
  (defconst hiro/main-note-library (nth 0 hiro/note-libraries)))

(use-package biblio
  :load-path "lib/biblio.el"
  :config
  (setq biblio-download-directory hiro/main-pdf-library)
  ;; extend biblio actions
  (defun hiro/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
    "Add BIBTEX (from ENTRY) to end of a user-specified bibtex file."
    (with-current-buffer (find-file-noselect hiro/main-bib-library)
      (goto-char (point-max))
      (insert bibtex))
    (message "Inserted bibtex entry for %S."
	     (biblio--prepare-title (biblio-alist-get 'title entry))))
  (defun hiro/biblio-selection-insert-end-of-bibfile (record)
    "Insert BibTeX of current entry `RECORD' at the end of user-specified bibtex file."
    (interactive)
    (biblio--selection-forward-bibtex #'hiro/biblio--selection-insert-at-end-of-bibfile-callback))
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Add bib to the end of bib file" .
                 hiro/biblio-selection-insert-end-of-bibfile))
  (defun hiro/biblio-selection-insert-and-download (record)
    "Insert BibTex of current entry `RECORD' at the end of user-specified bibtex file,
and download pdf to user-specified directory."
    (interactive)
    (progn
      (biblio--selection-forward-bibtex #'hiro/biblio-selection-insert-end-of-bibfile)
      (biblio-download--action record)))
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Add bib and download pdf" .
                 hiro/biblio-selection-insert-and-download)))

(use-package citar
  :bind (("C-c b" . citar-open))
  :config
  (setq citar-bibliography hiro/bib-libraries)
  (setq citar-library-paths hiro/pdf-libraries)
  (setq citar-notes-paths hiro/note-libraries)
  (setq citar-library-file-extensions '("pdf" "org" "md"))
  (setq citar-file-open-function (lambda (fpath)
                                   (call-process "open" nil 0 nil "-a" "Skim" fpath)))
  (defun hiro/citar-full-names (names)
    "Transform names like LastName, FirstName to FirstName LastName."
    (when (stringp names)
      (mapconcat
       (lambda (name)
         (if (eq 1 (length name))
             (split-string name " ")
           (let ((split-name (split-string name ", ")))
             (cl-concatenate 'string (nth 1 split-name) " " (nth 0 split-name)))))
       (split-string names " and ") ", ")))

  (defun hiro/citar-open-roam-note (citekey csl-entry)
    "Citar open or create org-roam node with citekey and csl-entry."
    (if-let* ((csl-title (cdr (assoc "title" csl-entry)))
              (node (org-roam-node-from-title-or-alias (format "Notes on %s" csl-title))))
        (org-roam-node-visit node)
      (let* ((node (org-roam-node-create :title csl-title :refs citekey :tags "papernote")))
        (org-roam-capture-
         :node node
         :keys "r"
         :info (list :ref citekey)
         :props '(:finalize find-file)))))
  (setq citar-open-note-function 'hiro/citar-open-roam-note)
  (setq citar-display-transform-functions
        '((t . citar-clean-string)
          (("author" "editor") . hiro/citar-full-names)))
  (setq citar-templates
        '((main . "${author editor:55}     ${date year issued:4}     ${title:55}")
          (suffix . "  ${tags keywords keywords:40}")
          (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
          (note . "#+title: Notes on ${author editor}, ${title}")))
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  ;; use consult-completing-read for enhanced interface
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package oc
  :config
  (setq org-cite-global-bibliography hiro/bib-libraries
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((latex biblatex)
                                     (t csl))))

(use-package bibtex
  :config
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5
        bibtex-files hiro/bib-libraries
        org-ref-bibtex-hydra-key-binding (kbd "H-b"))
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body))

(use-package org-zotxt
  :hook
  (org-mode . org-zotxt-mode))

(provide 'init-research)
;;; init-research.el ends here
