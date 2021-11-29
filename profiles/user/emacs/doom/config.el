;;; .doom.d/config.el -*- lexical-binding: t; -*-
(load! "doom-source-dir.el")
;; doom does this automatically for doomdirs, but home-manager has a different directory
(add-to-list '+emacs-lisp-disable-flycheck-in-dirs lh/doom-source-dir)

;; append because first element is "not" which negates the list
(add-to-list '+format-on-save-enabled-modes 'sh-mode t) ; weird bash formatting
(add-to-list '+format-on-save-enabled-modes 'web-mode t) ; noisy htmltidy output buffers for emails

;; highlight links in comments and strings
(define-globalized-minor-mode global-goto-address-prog-mode goto-address-prog-mode
  (lambda () (goto-address-prog-mode 1)))
(global-goto-address-prog-mode 1)

;; transparency for current frame and new frames
(set-frame-parameter (selected-frame) 'alpha '(85 . 82))
(add-to-list 'default-frame-alist '(alpha . (85 . 82)))

;; Replace selection when inserting text
(delete-selection-mode 1)

;; autosave files
(setq auto-save-default 1)

;; show weekend at week's end
(setq calendar-week-start-day 1)

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

(setq doom-font "monospace"
      ;; TODO: create "monospace-serif" family with fontconfig?
      doom-serif-font "Iosevka Term Curly Slab"
      doom-variable-pitch-font "sans-serif" ; inherits `doom-font''s :size
      ;; doom-unicode-font "sans-serif"
      doom-symbol-fallback-font-families nil
      ;; TODO: this doesn't work, need the following hook
      doom-emoji-fallback-font-families '("JoyPixels"))

;; TODO: why doesn't "emoji" from fontconfig work?
(add-hook! 'after-setting-font-hook (set-fontset-font t 'unicode "JoyPixels"))

;; integrate with freedesktop secret service
;; TODO: determine why this doesn't completely work.
;; (secrets-create-item "Main" "test emacs item" "test pw") returns
;; D-Bus error: "No such method 'CreateItem' in interface 'org.freedesktop.Secret.Collection' at object path '/org/freedesktop/secrets/collection/main' (signature 'a{sv}(oayay)b')"
(require 'secrets)
(setq auth-sources '(default "secrets:Main"))

;;; tramp
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

;;; dired
(after! dired
  (setq all-the-icons-dired-monochrome nil))
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
        ispell-personal-dictionary (expand-file-name "~/.aspell.en.pws")))
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
(after! latex
  (setq-default TeX-command-list
                (cons
                 ;; %(mode) not supported yet? same with %(file-line-error)
                 '("Tectonic" "tectonic %S %(extraopts) %t" TeX-run-command nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run Tectonic")
                 TeX-command-list))
  (setq reftex-default-bibliography '("~/nc/research/main.bib")))
(after! citar
  (setq! citar-bibliography '("~/nc/research/main.bib")
         citar-library-paths '("~/nc/research/documents/")
         citar-notes-paths '("~/nc/personal/research/")))

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
