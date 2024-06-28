;; -*- lexical-binding: t; -*-

(use-package org
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c i a" . org-id-get-create)
         ("C-c e d" . org-export-docx)
         :map sanityinc/org-global-prefix-map
         ("j" . org-clock-goto)
         ("l" . org-clock-in-last)
         ("i" . org-clock-in)
         ("o" . org-clock-out)
         ("b" . org-mark-ring-goto)
         :map org-src-mode-map
         ;; I prefer C-c C-c over C-c ' (more consistent)
         ("C-c C-c" . org-edit-src-exit))
  :bind-keymap ("C-c o" . sanityinc/org-global-prefix-map)
  :hook ((org-agenda-mode . hl-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :custom
  (org-modules nil) ; Faster loading
  (org-log-done 'time)
  (org-fontify-done-headline nil)
  (org-edit-timestamp-down-means-later t)
  (org-catch-invisible-edits 'show)
  (org-export-coding-system 'utf-8)
  (org-fast-tag-selection-single-key 'expert)
  (org-html-validation-link nil)
  (org-export-kill-product-buffer-when-displayed t)
  (org-tags-column 80)
  (org-hide-emphasis-markers t)
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-src-fontify-natively t)
  ;; (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-indent-mode nil)
  (org-image-actual-width nil) ;; set this first for #+attr_org :width works
  :config
  ;; org face
  (with-eval-after-load "org"
    ;; Use fixed pitch for table and code
    (custom-set-faces
     '(org-table ((t :inherit 'fixed-pitch-serif)))
     '(org-code ((t :inherit 'fixed-pitch-serif)))
     '(org-block ((t :inherit 'fixed-pitch-serif)))
     '(org-checkbox ((t :inherit 'fixed-pitch :background nil :box nil)))
     '(org-latex-and-related ((t (:inherit 'fixed-pitch-serif))))))
  ;; org latex code render size
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))
  ;; make svg latex preview image
  (setq org-preview-latex-default-process 'dvisvgm)
  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (add-hook
                 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languags
   `((emacs-lisp . t)
     (haskell . nil)))
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block )
  :preface
  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")
  ;; Export to docx
  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (expand-file-name "template/template.docx"
                                           user-emacs-directory)))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file))
      (message "Convert finish: %s" docx-file))))

;;; Org agenda

(use-package org-agenda
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  ;; Directories settings
  (when (file-directory-p "~/Documents/gtd/")
    (setq org-agenda-files (list "~/Documents/gtd/")))
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-deadline-warning-days 16)
  (setq org-use-speed-commands t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-start-day "+0d")
  (setq org-todo-keywords
        '(
          (sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
          (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
          (sequence "|" "READ(r)")
          (sequence "|" "BLOG(b)")
          (sequence "|" "IDEA(i)")
          ))

  ;; my agenda view contains follow items:
  ;;; Ideas
  ;;; Day views
  ;;; Reading list
  ;;; Blogs
  (defvar org-agenda-custom-commands nil
    "Custom commands for org agenda.")
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and Ideas"
           ((todo "IDEA"
                  ((org-agenda-overriding-header "Stupid but maybe interesting IDEAs:")))
            (agenda ""
                    ((org-agenda-overriding-header "Today's agenda")
                     (org-agenda-span 'day)
                     (org-agenda-toggle-deadlines)))
            (agenda ""
                    ((org-agenda-overriding-header "Future's agenda(One week)")
                     (org-agenda-start-day "+1d")
                     (org-agenda-span 7)
                     (org-agenda-toggle-deadlines)))
            (todo "READ"
                  ((org-agenda-overriding-header "Reading List:")))
            (todo "BLOG"
                  ((org-agenda-overriding-header "Blogs:")))))))
  ;; org capture
  (defvar org-capture-templates nil
    "Org capture templates.")
  (setq org-capture-templates '(("i" "Idea"
                                 entry (file+headline "~/Documents/gtd/ideas.org" "Someday/Maybe")
                                 "* IDEA %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t)
                                ("t" "Todo"
                                 entry (file+headline "~/Documents/gtd/inbox.org" "TODOs")
                                 "* TODO %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t)
                                ("r" "Read"
                                 entry (file+headline "~/Documents/gtd/read.org" "Reading List")
                                 "* READ %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t)
                                ("b" "Blog"
                                 entry (file+headline "~/Documents/gtd/blog.org" "Blogs")
                                 "* BLOG %?\nAdded: %U\n"
                                 :prepend t
                                 :kill-buffer t))))

;; https://christiantietze.de/posts/2019/12/emacs-notifications/
(use-package appt ;; appointment for org agenda
  :config
  (require 'notifications)
  (defun hiro/notify (title msg)
    "Send notification with `msg' and `title'."
    (if (eq system-type 'darwin)
        (ns-do-applescript (format
                            "display notification \"%s\" with title \"%s\" sound name \"Submarine\""
                            msg title))
      (if (eq system-type 'gnu/linux)
          (notifications-notify
           :title title
           :body msg))))

  (setq appt-time-msg-list nil    ;; clear existing appt list
        appt-display-interval '5  ;; warn every 5 minutes from t - appt-message-warning-time
        appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
        appt-display-mode-line t     ;; don't show in the modeline
        appt-display-format 'window)   ;; pass warnings to the designated window function

  (defun hiro/appt-display-native (min-to-app new-time msg)
    (hiro/notify
     (format "Appointment in %s minutes" min-to-app) ; Title
     (format "%s" msg)))

  (setq appt-disp-window-function (function hiro/appt-display-native))
  (appt-activate 1)                ;; activate appointment notification

  ;; Agenda-to-appointent hooks
  (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  )

(use-package org-download
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-image-dir "~/Pictures/org-download/")
  (setq org-download-image-org-width 400)
  (setq org-download-image-latex-width 400)
  (setq org-download-image-attr-list
        '("#+attr_html: scale=0.8 :align center")))

(use-package markdown-mode
  :mode (("\\.md\\.html\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package org-bars
  :hook (org-mode . org-bars-mode)
  :config
  (setq org-bars-stars '(:empty "◉"
                                :invisible "▶"
                                :visible "▼")))

(use-package writeroom-mode
  ;; :hook ((org-mode . prose-mode)
  ;;        (LaTeX-mode . prose-mode))
  :config
  (setq writeroom-width 120)
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
    This enables or modifies a number of settings so that the
    experience of editing prose is a little more like that of a
    typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.6)
          (setq-local electric-pair-mode nil)
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))

(use-package org-margin
  :load-path "~/.config/emacs/lib/org-margin/"
  :hook ((org-mode . org-margin-mode)))

(provide 'init-org)
;;; init-org.el ends here
