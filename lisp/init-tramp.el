;; -*- lexical-binding: t; Remote SSH Configuration -*-

(use-package ssh-config-mode)

(use-package tramp
  :config
  ;; Make sure version control system doesn't slow tramp:
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

;; make sure tramp ssh option comes from personal `.ssh/config'
(use-package tramp-sh
  :after tramp
  :custom (tramp-use-ssh-controlmaster-options nil))

;; Copied from https://github.com/howardabrams/hamacs/blob/main/ha-remoting.org
(defvar ha-latest-ssh-window-name nil
  "The window-name of the latest ssh session. Most commands default to the last session.")

(defvar ha-ssh-host-history '() "List of hostnames we've previously connected.")

(defvar ha-ssh-favorite-hostnames '()
  "A list of tuples (associate list) containing a hostname and its IP address.
See =ha-ssh-add-favorite-host= for easily adding to this list.")

(defvar ha-ssh-shell (shell-command-to-string "type -p zsh")
  "The executable to the shell I want to use locally.")

(defun ha-ssh (hostname &optional window-name)
  "Start a SSH session to a given HOSTNAME (with an optionally specified WINDOW-NAME).
If called interactively, it presents the user with a list
returned by =ha-ssh-choose-host=."
  (interactive (list (ha-ssh-choose-host)))
  (unless window-name
    (setq window-name (format "ssh: %s" hostname)))
  (setq ha-latest-ssh-window-name (format "*%s*" window-name))

  ;; I really like this =vterm= interface, so if I've got it loaded, let's use it:
  (if (not (fboundp 'vterm))
      ;; Should we assume the =ssh= we want is on the PATH that started Emacs?
      (make-term window-name "ssh" nil hostname)
    (vterm ha-latest-ssh-window-name)
    (vterm-send-string (format "ssh %s" hostname))
    (vterm-send-return))

  (pop-to-buffer ha-latest-ssh-window-name))

(defun ha-ssh-choose-host ()
  "Prompts the user for a host, and if it is in the cache, return
its IP address, otherwise, return the input given.
This is used in calls to =interactive= to select a host."
  (let ((hostname
         ;; We call Helm directly if installed, only so that we can get better
         ;; labels in the window, otherwise, the =completing-read= call would be fine.
         (if (fboundp 'helm-comp-read)
             (helm-comp-read "Hostname: " ha-ssh-favorite-hostnames
                             :name "Hosts"
                             :fuzzy t :history ha-ssh-host-history)
           (completing-read "Hostname: " ha-ssh-favorite-hostnames nil 'confirm nil 'ha-ssh-host-history))))
    (alist-get hostname ha-ssh-favorite-hostnames hostname nil 'equal)))

(defun ha-shell (&optional directory)
  "Creates and tidies up a =vterm= terminal shell in side window."
  (interactive (list (read-directory-name "Starting Directory: " (projectile-project-root))))
  (let* ((win-name (ha--terminal-name-from-dir directory))
         (buf-name (format "*%s*" win-name))
         (default-directory (or directory default-directory)))
    (setq ha-latest-ssh-window-name buf-name)
    (if (not (fboundp 'vterm))
        (make-term win-name ha-ssh-shell)
      (vterm buf-name))))

(defun ha-ssh-add-favorite-host (hostname ip-address)
  "Add a favorite host to your list for easy pickin's."
  (interactive "sHostname: \nsIP Address: ")
  (add-to-list 'ha-ssh-favorite-hostnames (cons hostname ip-address)))

(defun ha-shell-send (command &optional directory)
  "Send COMMAND to existing shell based on DIRECTORY.
If the shell doesn't already exist, start on up by calling
the `ha-shell' function."
  (let* ((win-name (ha--terminal-name-from-dir directory))
         (win-rx   (rx "*" (literal win-name) "*"))
         (bufs     (seq-filter (lambda (b) (when (string-match win-rx (buffer-name b)) b))
                               (buffer-list)))
         (buf (first bufs)))
    (unless buf
      (setq buf (ha-shell directory)))
    (ha-ssh-send command buf)))

(defun ha--terminal-name-from-dir (&optional directory)
  "Return an appropriate title for a terminal based on DIRECTORY.
If DIRECTORY is nil, use the `projectile-project-name'."
  (unless directory
    (setq directory (projectile-project-name)))
  (format "Terminal: %s" (file-name-base (directory-file-name directory))))

(defun ha-ssh-send (phrase &optional window-name)
  "Send command PHRASE to the currently running SSH instance.
If you want to refer to another session, specify the correct WINDOW-NAME.
This is really useful for scripts and demonstrations."
  (unless window-name
    (setq window-name ha-latest-ssh-window-name))
  (save-window-excursion
    (pop-to-buffer window-name)

    (if (fboundp 'vterm)
        (progn
          (vterm-send-string phrase)
          (vterm-send-return))
      (progn
        (term-send-raw-string phrase)
        (term-send-input)))))

(defun ha-ssh-send-line ()
  "Copy the contents of the current line in the current buffer,
and call =ha-ssh-send= with it. After sending the contents, it
returns to the current line."
  (interactive)
  ;; The function =save-excursion= doesn't seem to work...
  (let* ((buf (current-buffer))
         (cmd-line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
         (trim-cmd (s-trim cmd-line)))
    (ha-ssh-send trim-cmd)
    ;; (sit-for 0.25)
    (pop-to-buffer buf)))

(defun ha-ssh-exit (&optional window-name)
  "End the SSH session specified by WINDOW-NAME (or if not, the latest session)."
  (interactive)
  (unless (string-match-p "v?term" (buffer-name))
    (unless window-name
      (setq window-name ha-latest-ssh-window-name))
    (pop-to-buffer window-name))

  (ignore-errors
    (term-send-eof))
  (kill-buffer window-name)
  (delete-window))

;;; Tramp
(defun ha-ssh-find-file (hostname)
  "Constructs a ssh-based, tramp-focus, file reference, and then calls =find-file=."
  (interactive (list (ha-ssh-choose-host)))
  (let ((tramp-ssh-ref (format "/ssh:%s:" hostname))
        (other-window (when (equal current-prefix-arg '(4)) t)))
    (ha-ssh--find-file tramp-ssh-ref other-window)))

(defun ha-ssh--find-file (tramp-ssh-ref &optional other-window)
  "Calls =find-file= after internally completing a file reference based on TRAMP-SSH-REF."
  (let ((tramp-file (read-file-name "Find file: " tramp-ssh-ref)))
    (if other-window
        (find-file-other-window tramp-file)
      (find-file tramp-file))))

(defun ha-ssh-find-root (hostname)
  "Constructs a ssh-based, tramp-focus, file reference, and then calls =find-file=."
  (interactive (list (ha-ssh-choose-host)))
  (let ((tramp-ssh-ref (format "/ssh:%s|sudo:%s:" hostname hostname))
        (other-window (when (equal current-prefix-arg '(4)) t)))
    (ha-ssh--find-file tramp-ssh-ref other-window)))

(provide 'init-tramp)
;;; init.tramp.el ends here
