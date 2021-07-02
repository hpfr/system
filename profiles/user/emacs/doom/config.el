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

(let ((doom-dark-themes
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
                            )))
  ;; TODO: improve and hook into base16
  (defun load-random-theme ()
    "Load a random theme, light during the day"
    (interactive)
    (let* ((current-hour (string-to-number (format-time-string "%H")))
           (current-themes (if (or (>= current-hour 19) (< current-hour 7))
                               doom-dark-themes doom-light-themes)))
      (load-theme (nth (random (length current-themes)) current-themes) t))))

(load-random-theme)
(run-at-time "1 hour" 3600 #'load-random-theme)

(setq doom-font (font-spec :family "monospace")
      ;; TODO: create "monospace-serif" family with fontconfig?
      doom-serif-font (font-spec :family "Iosevka Term Curly Slab")
      doom-variable-pitch-font (font-spec :family "sans-serif") ; inherits `doom-font''s :size
      ;; this list is iterated over, each item is prepended, so order by reverse priority
      doom-unicode-extra-fonts '("Weather Icons" "github-octicons" "FontAwesome"
                                 "all-the-icons" "file-icons" "Material Icons"
                                 "Source Han Mono" "emoji")
      doom-unicode-font (font-spec :family "sans-serif"))

;; integrate with freedesktop secret service
;; TODO: determine why this doesn't completely work.
;; (secrets-create-item "Main" "test emacs item" "test pw") returns
;; D-Bus error: "No such method 'CreateItem' in interface 'org.freedesktop.Secret.Collection' at object path '/org/freedesktop/secrets/collection/main' (signature 'a{sv}(oayay)b')"
(require 'secrets)
(setq auth-sources '(default "secrets:Main"))

;;; dired
(after! dired
  (setq all-the-icons-dired-monochrome nil))

;;; image
(setq image-use-external-converter t)   ; view HEIC files from Apple devices
(add-to-list 'auto-mode-alist '("\\.heic\\'" . image-mode))
(add-to-list 'image-file-name-extensions "heic")

;; use pdf-tools for latex rather than zathura, etc
(setq +latex-viewers '(pdf-tools))

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

;;; telega
(load! "telega-server-libs.el")
(use-package! telega)

;;; eshell
(after! eshell
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (set-eshell-alias!
   "nrs" "sudo nixos-rebuild switch $*"
   "nrsu" "sudo nix-channel --update; sudo nixos-rebuild switch $*"
   "nrsl" "sudo nixos-rebuild switch -option builders '' $*"
   "mkd" "mkdir -pv"
   "sctl" "sudo systemctl"
   "uctl" "systemctl --user"))

;; find syncthing conflicts
(use-package! emacs-conflict)

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
