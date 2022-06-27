;;; -*- lexical-binding: t; -*-
(load! "doom-source-dir.el")
;; doom does this automatically for doomdirs, but home-manager has a different
;; directory
(cl-pushnew lh/doom-source-dir +emacs-lisp-disable-flycheck-in-dirs :test #'string=)

;; first element is "not" which negates the list
(pushnew! (cdr +format-on-save-enabled-modes)
          'sh-mode ; weird bash formatting
          'web-mode) ; noisy htmltidy output buffers for emails

;; opacity for current frame and new frames
(setf (alist-get 'alpha default-frame-alist) '(88 . 82)
      ;; frames start maximized
      (alist-get 'fullscreen default-frame-alist) 'maximized)

;; show weekend at week's end
(setq calendar-week-start-day 1)

(setq which-key-idle-delay 3.0)

(after! orderless
  (setq orderless-component-separator #'orderless-escapable-split-on-space))

(after! magit-section
  (map! :map magit-section-mode-map
        "C-n" #'magit-section-forward
        "C-p" #'magit-section-backward))

(defun lh/load-random-theme ()
  "Load a random theme, light during the day"
  (interactive)
  (let* ((doom-dark-themes
          '(doom-tomorrow-night doom-nord doom-one doom-city-lights doom-gruvbox
                                doom-henna doom-material doom-oceanic-next
                                doom-spacegrey doom-moonlight doom-challenger-deep
                                doom-opera doom-peacock doom-rouge doom-molokai
                                doom-monokai-classic doom-palenight
                                ;; chopping block
                                doom-snazzy doom-horizon doom-dark+ doom-solarized-dark doom-old-hope doom-dracula
                                doom-sourcerer doom-wilmersdorf
                                ;; ;; no bueno
                                ;; doom-laserwave doom-ephemeral doom-nova doom-fairy-floss doom-manegarm
                                ;; doom-acario-light
                                ))
         (doom-light-themes
          '(doom-gruvbox-light doom-solarized-light
                               doom-tomorrow-day doom-opera-light doom-one-light
                               ;; doom-nord-light doom-acario-light
                               ))
         (current-hour (string-to-number (format-time-string "%H")))
         (current-themes (if (or (>= current-hour 19) (< current-hour 7))
                             doom-dark-themes doom-light-themes)))
    (load-theme (nth (random (length current-themes)) current-themes) t)))

(lh/load-random-theme)
;; load a new random theme at 7:00 and 19:00
;; run-at-time runs immediately if the current time of day is past the specified
;; time, so we need some logic to avoid this wastefully loading themes
;; repeatedly on startup
(let ((current-hour (string-to-number (format-time-string "%H"))))
  (if (< current-hour 7)
      (run-at-time "07:00" 43200 #'lh/load-random-theme)
    (if (< current-hour 19)
        (run-at-time "19:00" 43200 #'lh/load-random-theme)
      ;; when it's past 19:00, run the run-at-time once we're into the next day
      ;; before 07:00 to avoid it running immediately
      (run-at-time "6 hour" nil (lambda () (run-at-time "07:00" 43200 #'lh/load-random-theme))))))

;; not necessary with avy
(setq display-line-numbers-type nil)

;;; fill column
(after! visual-fill-column
  ;; account for fringe, margin, and some org-indent
  (setq-default visual-fill-column-width 90
                visual-fill-column-center-text t
                ;; git gutter with text
                visual-fill-column-fringes-outside-margins nil))

(after! display-fill-column-indicator
  (setq global-display-fill-column-indicator-modes
        '((not special-mode text-mode) t))
  ;; HACK: overwrite the definition because they used a hard-coded predicate
  ;; instead of the variable
  (define-globalized-minor-mode global-display-fill-column-indicator-mode
    display-fill-column-indicator-mode display-fill-column-indicator--turn-on
    :predicate global-display-fill-column-indicator-modes)

  (global-display-fill-column-indicator-mode))

;; display form feed characters as horizontal rules
(global-form-feed-mode)

;;; elastic tabstops
(use-package! spaceship-mode
  :commands spaceship-mode)
(use-package! tabble-mode
  :commands tabble-mode)

;;; undo
(map! :leader
      (:prefix ("b" . "buffer")
       :desc "Undo tree" "u" #'vundo
       :desc "Save buffer as root" "U" #'doom/sudo-save-buffer))
(after! vundo
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;; shrface
(add-hook 'eww-after-render-hook #'shrface-mode)
(after! eww (require 'shrface))
(after! shrface
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

;; integrate with freedesktop secret service
(add-load-path! "backport")
(require 'secrets)
;; default to saving secrets to the system keyring, but able to move them to
;; cross-system Main database if they aren't system-specific
(setq auth-sources `(,(concat "secrets:" (system-name) "-keyring")
                     "secrets:Main"
                     ;; for keepassxc, this is whatever database is active (focused)
                     default))

;;; tramp
;; TODO: tramp hangs after I approve from Duo for CS lab machines
(after! tramp
  ;; tramp sets tramp-default-remote-path via `getconf PATH` which doesn't seem
  ;; to work on NixOS, only returning /run/current-system/sw/bin:/bin:/usr/bin
  ;; this means magit, rg, etc don't work. this fixes the issue
  ;; https://www.gnu.org/software/tramp/#Remote-programs
  (cl-pushnew 'tramp-own-remote-path tramp-remote-path)

  ;; tramp stores .tramp_history in the home directory by default
  (when-let ((data-home (getenv "XDG_DATA_HOME")))
    (setq tramp-histfile-override (expand-file-name "tramp_history" data-home))))

;; with fish as my login shell, this might be necessary if people use
;; non-fish-supported POSIX or bash features in commands called by
;; `shell-command`
(setq shell-file-name "/run/current-system/sw/bin/bash")
;; but override back to fish in an interactive context
(after! vterm
  (setq vterm-shell "/etc/profiles/per-user/lh/bin/fish"))

;;; workspaces
;; I like keeping the main workspace around for miscellaneous files even when I
;; immediately open a project
(setq +workspaces-on-switch-project-behavior t)
;; force Doom project name to "doom" so "emacs" is available for upstream source
;; TODO: dir-locals aren't loaded before workspace is created
(put 'projectile-project-name 'safe-local-variable 'stringp)
(after! projectile
  (setq projectile-project-name-function
        (lambda (project-root)
          (let* ((full-root (expand-file-name project-root))
                 (root-filename (directory-file-name project-root))
                 (root-basename (file-name-nondirectory root-filename)))
            (concat (cond ((string= full-root doom-emacs-dir) "doom")
                          ((string= root-basename ".git-annex")
                           (let ((parent-basename
                                  (file-name-nondirectory
                                   (directory-file-name
                                    (file-name-directory root-filename)))))
                             (concat parent-basename "/annex")))
                          (t root-basename))
                    (when-let ((remote-host (file-remote-p full-root 'host)))
                      (concat "@" remote-host))) ))))

;;; dired
(after! all-the-icons-dired
  (setq all-the-icons-dired-monochrome nil))
(after! (dired-aux dired-x)
  ;; use bsdtar for common archival and compression tasks in dired
  (let* ((bsdtar-rw-exts
          (rx "." (or (seq (? "t") (? "ar.")
                           (or "gz" "bz2" "xz" "zst" "lz" "lz4" "lzo" "Z" "lrz"))
                      "tar" "cpio" "iso" "zip" "ar" "xar" "lha" "lzh" "7z" "warc") eos))
         (bsdtar-ro-exts (rx "." (or "rar" "cab") eos))
         (bsdtar-r-exts (rx (or (regexp bsdtar-rw-exts) (regexp bsdtar-ro-exts)))))
    ;; `dired-do-shell-command' suggestions
    (setf (alist-get bsdtar-r-exts dired-guess-shell-alist-user nil nil #'string=)
          '("bsdtar -tvf"
            ;; extract into new directory
            (let ((name (string-remove-suffix ".tar" (file-name-sans-extension file))))
              (concat "mkdir " name "; bsdtar -C " name " -xvf"))
            ;; convert other archives to .tar.zst
            (concat "bsdtar -acvf "
                    (string-remove-suffix ".tar" (file-name-sans-extension file)) ".tar.zst"
                    " @`?`")
            "bsdtar -xvf"))
    ;; decompression with `dired-do-compress'
    (setq dired-compress-file-suffixes `((,bsdtar-r-exts "" "bsdtar -xf")))
    ;; compression with `dired-do-compress-to'
    (setq dired-compress-files-alist
          `((,(rx (or (regexp bsdtar-rw-exts) (seq ".shar" eos))) . "bsdtar -acf %o -- %i"))))

  ;; TODO: get feedback on ergonomics of using file at point vs using it for
  ;; default in read-file-name
  (defun my/dired-ediff-dwim ()
    "Ediff files or directories in dired.

If two files are marked in the current directory, they will be diffed. If only one
is, the marked file in the other dired window's directory or the file at point
will be used. If only one file is marked and it's in the other directory, it
will be diffed with the file at point. If no files are marked, the file at point
will be diffed with a file input interactively.

To minimize confusion, the files will be diffed initially with the newer file on
the right. Refer to `ediff-swap-buffers' to swap them."
    (interactive)
    (let* ((files (dired-get-marked-files))
           (other-win (get-window-with-predicate
                       (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (and (not (eq window (selected-window)))
                                (eq major-mode 'dired-mode))))))
           (other-buffer (and other-win (window-buffer other-win)))
           (other-files (and other-buffer
                             (with-current-buffer other-buffer
                               (dired-get-marked-files))))
           ;; if no files are marked, dired-get-marked-files falls back to the
           ;; current line, which we want to ignore for the other window
           (other-marked-files
            (if (and (= (length other-files) 1)
                     (with-current-buffer other-buffer
                       (save-excursion
                         (goto-char (point-min))
                         (not (re-search-forward (dired-marker-regexp) nil t)))))
                nil other-files)))
      (if (> (length files) 2)
          (error "No more than 2 files should be marked.")
        (let* ((file1
                (or (car files)
                    (dired-get-filename)
                    (read-file-name
                     "File: "
                     (dired-dwim-target-directory))))
               (file1-dir (file-directory-p file1))
               (file2
                (or (cadr files)
                    (if (> (length other-marked-files) 1)
                        (error "No more than one file in another window should be marked.")
                      (or (car other-marked-files)
                          ;; use filename at point unless there were no
                          ;; marks so that was already set to file1
                          (if (string= file1 (dired-get-filename))
                              ;; prompt based on the type of file1
                              (let ((read-file-function
                                     (if file1-dir
                                         #'read-directory-name
                                       #'read-file-name)))
                                (funcall read-file-function
                                         "File: "
                                         (dired-dwim-target-directory)))
                            (dired-get-filename))))))
               (file2-dir (file-directory-p file2))
               (ediff-function
                (if (and file1-dir file2-dir)
                    (lambda (dir1 dir2)
                      (let ((default-regexp (eval ediff-default-filtering-regexp)))
                        (ediff-directories
                         dir1 dir2
                         (read-string
                          (if (stringp default-regexp)
                              (format "Filter filenames through regular expression (default %s): "
                                      default-regexp)
                            "Filter filenames through regular expression: ")
                          nil
                          'ediff-filtering-regexp-history
                          (eval ediff-default-filtering-regexp)))))
                  #'ediff-files)))
          (if (not (eq file1-dir file2-dir))
              (error "Cannot compare a file with a directory.")
            (apply ediff-function (if (file-newer-than-file-p file1 file2)
                                      (list file2 file1)
                                    (list file1 file2))))))))
  (map! :map dired-mode-map
        :desc "Ediff files"
        :ng "e" #'my/dired-ediff-dwim))

(after! tar
  (setq tar-mode-show-date t))

;;; dashboard
;; shortcuts inspired by tecosaur
(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "s" (cmd! (doom-project-browse (expand-file-name "~/repos/system/")))
      :ne "e" (cmd! (doom-project-browse (expand-file-name "~/repos/system/profiles/user/emacs/doom/")))
      ;; :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      ;; :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)
;; hide unnecessary widgets and cursor, and set a gnu splash
(remove-hook! '+doom-dashboard-functions
  #'doom-dashboard-widget-shortmenu
  #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor '(nil))
(setq fancy-splash-image "~/nc/config/heckert-gnu.svg")

;; view HEIC files from Apple devices
(after! files
  (setf (alist-get (rx "." (or "heic" "HEIC") string-end)
                   auto-mode-alist nil nil #'string=) 'image-mode))
(after! image
  (setq image-use-external-converter t))
(after! image-file
  (cl-pushnew "heic" image-file-name-extensions :test #'string=))

;; use pdf-tools for latex rather than zathura, etc
(setq +latex-viewers '(pdf-tools))

;;; personal
(setq user-full-name "Liam Hupfer"
      user-mail-address "liam@hpfr.net")

;;;; apps
;;; magit
(after! embark
  (require 'embark-vc))

(after! magit-branch
  (setq magit-branch-read-upstream-first 'fallback))

(after! forge-list
  (setq forge-owned-accounts '(("hpfr"))))

(after! git-auto-commit-mode
  (setq gac-ask-for-summary-p t
        gac-default-message "Update [git-auto-commit-mode]"))

;; move magit-annex from @ to & to accomodate forge
(after! magit
  (map! :map magit-mode-map
        "&" #'magit-annex-dispatch)
  (transient-append-suffix 'magit-dispatch '(0 -1 -1)
    '("&" "Annex" magit-annex-dispatch)))
;; despite the autoload after magit in magit-annex.el, the @ binding seems to
;; happen after magit-annex loads, so overwrite it then
(after! magit-annex
  (map! :map magit-mode-map
        "@" #'forge-dispatch)
  (transient-replace-suffix 'magit-dispatch "@"
    '("@" "Forge" forge-dispatch)))

;;; circe
(after! circe
  (setq circe-default-part-message ""
        circe-default-quit-message "")
  (set-irc-server! "chat.sr.ht soju"
    `(:host "chat.sr.ht"
      :port 6697
      :tls t
      :nick "lh"
      :sasl-username
      ,(concat (secrets-get-attribute "Main" "chat.sr.ht" :UserName)
               "@" (system-name))
      :sasl-password ,(secrets-get-secret "Main" "chat.sr.ht")))
  (set-irc-server! "Libera Chat"
    `(:host "chat.sr.ht"
      :port 6697
      :tls t
      :nick "lh"
      :sasl-username
      ,(concat (secrets-get-attribute "Main" "chat.sr.ht" :UserName)
               "/libera@" (system-name))
      :sasl-password ,(secrets-get-secret "Main" "chat.sr.ht"))))

;;; anki
(setq anki-editor-use-math-jax t)
(add-hook 'org-mode-hook #'anki-editor-mode)

(add-hook 'anki-mode-hook #'shrface-mode)
(add-hook 'anki-card-mode-hook #'shrface-mode)
(after! anki
  (setq anki-collection-dir
        (expand-file-name "~/.var/app/net.ankiweb.Anki/data/Anki2/User 1")
        anki-shr-rendering-functions
        (append anki-shr-rendering-functions shr-external-rendering-functions)))

;;; elfeed
(after! elfeed
  (setq-default elfeed-search-filter "@6-month-ago -read ")
  ;; poor man's sync
  (setq elfeed-db-directory "~/nc/config/elfeed/db/"
        elfeed-enclosure-default-dir "~/nc/config/elfeed/enclosures/")
  (require 'elfeed-tube))

(after! elfeed-tube
  (elfeed-tube-setup)
  (require 'elfeed-tube-mpv)
  (map! (:map elfeed-show-mode-map
         "F" #'elfeed-tube-fetch
         :localleader
         "f" #'elfeed-tube-mpv-follow-mode
         "w" #'elfeed-tube-mpv-where)
        (:map elfeed-search-mode-map
         "F" #'elfeed-tube-fetch)))

(after! elfeed-tube-mpv)

;;; elpher
(after! elpher
  (setq-hook! 'elpher-mode-hook gnutls-verify-error nil))

;;; eshell
(after! eshell
  (cl-pushnew 'eshell-tramp eshell-modules-list)
  (set-eshell-alias!
   "nrs" "doas nixos-rebuild switch $*"
   "nrsu" "doas nix-channel --update; doas nixos-rebuild switch $*"
   "nrsl" "doas nixos-rebuild switch --option builders '' $*"
   "mkd" "mkdir -pv"
   "sctl" "doas systemctl"
   "uctl" "systemctl --user"))

;; no autoloads
(require 'emacs-conflict)

;;; languages
;;; english
(after! spell-fu
  ;; aspell config is ignored by emacs ispell library
  (setq ispell-dictionary "en_US-w_accents"
        ispell-personal-dictionary (expand-file-name "~/nc/config/aspell/en.pws")))
(after! langtool
  (setq langtool-bin "languagetool-commandline"))
(load! "languagetool-server-jar.el")
(after! lexic
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))
(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

;;; latex
(add-to-list 'safe-local-eval-forms '(setq lsp-clients-texlab-executable (executable-find "texlab")))
(set-file-template! "\\.tex$" :mode 'latex-mode)

(after! latex
  (setq TeX-open-quote "“"
        TeX-close-quote "”"
        TeX-fold-type-list '(env macro))
  (setq reftex-default-bibliography '("~/nc/research/main.bib"))
  ;; for buffer-local
  (setq-default TeX-engine 'luatex
                TeX-command-list
                (cons
                 ;; %(mode) not supported yet? same with %(file-line-error)
                 '("Tectonic" "tectonic %S %(extraopts) %t" TeX-run-command nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run Tectonic")
                 TeX-command-list))

  ;; folding
  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  (map!
   :map LaTeX-mode-map
   :n "<tab>" 'outline-cycle
   :n "<backtab>" 'outline-cycle-buffer))

(after! cdlatex
  (map! :map cdlatex-mode-map
        :i "<tab>" 'cdlatex-tab))

(after! citar
  (setq citar-bibliography '("~/nc/research/main.bib")
        citar-library-paths '("~/nc/research/documents/")
        citar-notes-paths
        (list (expand-file-name "research/" org-roam-directory))))


(add-hook! '(LaTeX-mode-hook org-mode-hook) #'laas-mode)
(after! laas
  (aas-set-snippets 'laas-mode
    :cond #'laas-mathp
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ";;O" "Ω"
    ";;;O" "\\mho"

    "CC" "ℂ"
    "FF" "𝔽"
    "HH" "ℍ"
    "NN" "ℕ"
    "PP" "ℙ"
    "QQ" "ℚ"
    "RR" "ℝ"
    "ZZ" "ℤ"
    "..." "…"
    "..c" "⋯"

    "comp" "^c")

  (setq laas-use-unicode t)

  (after! cdlatex (map! :map cdlatex-mode-map
                        "`" nil
                        "'" nil
                        "_" nil
                        "^" nil))
  (after! org (map! :map org-cdlatex-mode-map
                    "`" nil
                    "'" nil
                    "_" nil
                    "^" nil)))

(add-hook! '(LaTeX-mode-hook
             ;; org-mode-hook
             ) #'lazytab-mode)
(after! lazytab

  (setf (alist-get "smat" cdlatex-command-alist nil nil #'string=)
        '("Insert smallmatrix env" "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
          lazytab-position-cursor-and-edit nil nil t)
        (alist-get "bmat" cdlatex-command-alist nil nil #'string=)
        '("Insert bmatrix env" "\\begin{bmatrix} ? \\end{bmatrix}"
          lazytab-position-cursor-and-edit nil nil t)
        (alist-get "pmat" cdlatex-command-alist nil nil #'string=)
        '("Insert pmatrix env" "\\begin{pmatrix} ? \\end{pmatrix}"
          lazytab-position-cursor-and-edit nil nil t)
        (alist-get "tbl" cdlatex-command-alist nil nil #'string=)
        '("Insert table" "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
          lazytab-position-cursor-and-edit nil t nil)))

;;; csharp
(add-to-list 'safe-local-eval-forms '(setq lsp-csharp-server-path (executable-find "omnisharp")))
(put 'lsp-csharp-solution-file 'safe-local-variable 'stringp)
(set-docsets! 'csharp-mode :add "NET Framework")

;;; python
(set-docsets! 'python-mode :add "Matplotlib")

;;; misc
;; TODO: make 'q' consistent across non-textual/popup buffers

;; TODO: fix popups
(after! tldr
  (setq tldr-directory-path (expand-file-name "tldr/" doom-etc-dir)))

(after! pdf-view
  ;; ;; no autoloads
  ;; (require 'pdf-continuous-scroll-mode)
  (map! :map pdf-view-mode-map
        :n "C-j" 'pdf-continuous-scroll-forward
        :n "C-k" 'pdf-continuous-scroll-backward
        :n "C-S-j" 'pdf-continuous-next-page
        :n "C-S-k" 'pdf-continuous-previous-page))
