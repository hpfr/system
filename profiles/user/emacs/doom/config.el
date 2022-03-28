;;; -*- lexical-binding: t; -*-
(load! "doom-source-dir.el")
;; doom does this automatically for doomdirs, but home-manager has a different directory
(add-to-list '+emacs-lisp-disable-flycheck-in-dirs lh/doom-source-dir)

;; append because first element is "not" which negates the list
(add-to-list '+format-on-save-enabled-modes 'sh-mode t) ; weird bash formatting
(add-to-list '+format-on-save-enabled-modes 'web-mode t) ; noisy htmltidy output buffers for emails

;; transparency for current frame and new frames
(set-frame-parameter (selected-frame) 'alpha '(85 . 82))
(add-to-list 'default-frame-alist '(alpha . (85 . 82)))

;; Replace selection when inserting text
(delete-selection-mode 1)

;; autosave files
(setq auto-save-default 1)

;; show weekend at week's end
(setq calendar-week-start-day 1)

(use-package! which-key
  :init
  (setq which-key-idle-delay 3.0))

(after! orderless
  (setq orderless-component-separator #'orderless-escapable-split-on-space))

(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q"
    'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q"
    'my-evil-textobj-anyblock-a-quote))

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

;; fonts
;; need floating-point size, otherwise will be treated as px instead of pt
(setq doom-font "monospace-12"
      ;; TODO: create "monospace-serif" family with fontconfig?
      doom-serif-font "Iosevka Term Curly Slab-12"
      doom-variable-pitch-font "sans-serif-12"
      ;; doom-unicode-font "sans-serif"
      doom-symbol-fallback-font-families nil
      ;; TODO: could set-fontset-font accept Fontconfig strings like face
      ;; functions do?
      doom-emoji-fallback-font-families '("JoyPixels"))
;; this is the only tofu I get with `view-hello-file'. not sure why it doesn't
;; work out of the box like the dozens of other scripts
(set-fontset-font t 'egyptian "Noto Sans Egyptian Hieroglyphs")
;; match monospace better. how can I change this for variable pitch face
;; can most of this be done in fontconfig? fall back from Iosevka to Julia Mono
;; to Source Han Mono and only then to sans-serif depending on script
(set-fontset-font t 'cjk-misc "Source Han Mono")
(set-fontset-font t 'han "Source Han Mono")
(set-fontset-font t 'kana "Source Han Mono")
(set-fontset-font t 'hangul "Source Han Mono")
(set-fontset-font t 'greek "monospace")
;; Iosevka doesn't have complete phonetic or Cyrillic coverage, but good enough
(set-fontset-font t 'phonetic "monospace")
(set-fontset-font t 'cyrillic "monospace")
(set-fontset-font t 'georgian "JuliaMono")

(global-display-fill-column-indicator-mode)

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
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; tramp stores .tramp_history in the home directory by default
  (when-let ((data-home (getenv "XDG_DATA_HOME")))
    (setq tramp-histfile-override (concat data-home "/tramp_history"))))

;; with fish as my login shell, this might be necessary if people use
;; non-fish-supported POSIX or bash features in commands called by
;; `shell-command`
(setq shell-file-name "/run/current-system/sw/bin/bash")
;; but override back to fish in an interactive context
(after! vterm
  (setq vterm-shell "/etc/profiles/per-user/lh/bin/fish"))

(after! projectile
  ;; TODO: dir-locals aren't loaded before workspace is created
  (setq projectile-project-name-function
        (lambda (project-root) (if (string= (directory-file-name project-root)
                                            "~/.config/emacs")
                                   "doom"
                                 (file-name-nondirectory (directory-file-name project-root)))))
  (put 'projectile-project-name 'safe-local-variable 'stringp))

;;; dired
(after! dired
  (setq all-the-icons-dired-monochrome nil)

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

;; use dired as a drag and drop source (dired already works as a sink)
(use-package! dired-dragon
  :after dired
  :config
  (map! :map dired-mode-map
        :localleader
        :desc "Drag and drop files" "d" #'dired-dragon))

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
;; hide shortcuts, I know them
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;; image
(setq image-use-external-converter t)   ; view HEIC files from Apple devices
(add-to-list 'auto-mode-alist '("\\.heic\\'" . image-mode))
(add-to-list 'image-file-name-extensions "heic")

;; use pdf-tools for latex rather than zathura, etc
(setq +latex-viewers '(pdf-tools))

;;; personal
(setq user-full-name "Liam Hupfer"
      user-mail-address "liam@hpfr.net")

;;;; apps
;;; magit
(after! magit-branch
  (setq magit-branch-read-upstream-first 'fallback))
(after! forge-list
  (setq forge-owned-accounts '(("hpfr"))))

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
(use-package! anki-editor
  :after org)
(use-package! anki
  :defer t
  :init
  (autoload 'anki "anki")
  (autoload 'anki-browser "anki")
  (autoload 'anki-list-decks "anki")
  :config
  (setq anki-collection-dir (expand-file-name "~/.var/app/net.ankiweb.Anki/data/Anki2/User 1")))

;;; elfeed
(after! elfeed
  ;; celluloid connects to MPRIS for playback control
  (defun celluloid-view (url)
    "Watch a video from URL in Celluloid"
    (async-shell-command (format "celluloid --new-window %s" url)))

  (defun elfeed-view-video ()
    "View feed link video in external program"
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (celluloid-view it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line)))))

;;; elpher
(after! elpher
  (setq-hook! 'elpher-mode-hook gnutls-verify-error nil))

;;; eshell
(after! eshell
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (set-eshell-alias!
   "nrs" "doas nixos-rebuild switch $*"
   "nrsu" "doas nix-channel --update; doas nixos-rebuild switch $*"
   "nrsl" "doas nixos-rebuild switch --option builders '' $*"
   "mkd" "mkdir -pv"
   "sctl" "doas systemctl"
   "uctl" "systemctl --user"))

;; find syncthing conflicts
(use-package! emacs-conflict)

;;; languages
;;; english
(after! spell-fu
  ;; aspell config is ignored by emacs ispell library
  (setq ispell-dictionary "en_US-w_accents"
        ispell-personal-dictionary (expand-file-name "~/nc/config/aspell/en.pws")))
(after! langtool
  (setq langtool-bin "languagetool-commandline"))
(load! "languagetool-server-jar.el")
(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
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
   :n "<backtab>" 'outline-cycle-buffer)
  )

(after! cdlatex
  (map! :map cdlatex-mode-map
        :i "<tab>" 'cdlatex-tab))

(after! citar
  (setq! citar-bibliography '("~/nc/research/main.bib")
         citar-library-paths '("~/nc/research/documents/")
         citar-notes-paths '("~/nc/personal/research/")))


(use-package! laas
  :hook ((LaTeX-mode org-mode) . laas-mode)
  :config
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

(use-package! lazytab
  :hook ((LaTeX-mode
          ;; org-mode ; doesn't work
          ) . lazytab-mode)
  :config
  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                        "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                        lazytab-position-cursor-and-edit
                                        nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                        "\\begin{bmatrix} ? \\end{bmatrix}"
                                        lazytab-position-cursor-and-edit
                                        nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                        "\\begin{pmatrix} ? \\end{pmatrix}"
                                        lazytab-position-cursor-and-edit
                                        nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                        "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                        lazytab-position-cursor-and-edit
                                        nil t nil)))

;;; csharp
(add-to-list 'safe-local-eval-forms '(setq lsp-csharp-server-path (executable-find "omnisharp")))
(put 'lsp-csharp-solution-file 'safe-local-variable 'stringp)
(set-docsets! 'csharp-mode :add "NET Framework")

;;; java
;; (setq lsp-java-format-settings-url "file:///home/lh/repos/system/profiles/user/emacs/doom/lib/eclipse-style-four-spaces.xml"
;;       lsp-java-format-settings-profile "Wisc")
;; (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
;;       lsp-java-format-settings-profile "GoogleStyle")

;;; python
(set-docsets! 'python-mode :add "Matplotlib")

;;; misc
;; TODO: make 'q' consistent across non-textual/popup buffers

(use-package! tldr
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" doom-etc-dir)))

;; TODO: debug. Wayland issue?
(use-package! yequake
  :config
  (setq yequake-frames
        '(("Agenda & scratch" .
           ((width . 0.75)
            (height. 0.4)
            (alpha . 0.95)
            (buffer-fns . ("*scratch*"))
            (frame-parameters . ((undecorated . t))))))))

(use-package! pdf-continuous-scroll-mode
  :disabled
  :after pdf-view
  :config
  (map!
   :map pdf-view-mode-map
   :n "C-j" 'pdf-continuous-scroll-forward
   :n "C-k" 'pdf-continuous-scroll-backward
   :n "C-S-j" 'pdf-continuous-next-page
   :n "C-S-k" 'pdf-continuous-previous-page))

(use-package! edwina
  :disabled
  :config
  (edwina-mode 1))
