;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; TODO: make path more relative
(add-to-list '+emacs-lisp-disable-flycheck-in-dirs
             "/home/lh/repos/system/profiles/user/emacs/doom/")

;; append because first element is "not" which negates the list
(add-to-list '+format-on-save-enabled-modes 'sh-mode t)

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
       '(doom-one doom-city-lights doom-challenger-deep doom-dark+ doom-dracula
                  doom-ephemeral doom-fairy-floss doom-gruvbox doom-henna
                  doom-horizon doom-laserwave doom-material doom-manegarm
                  doom-molokai doom-monokai-classic doom-moonlight doom-nord
                  doom-oceanic-next doom-old-hope doom-opera doom-palenight
                  doom-peacock doom-rouge doom-snazzy doom-solarized-dark
                  doom-sourcerer doom-spacegrey doom-tomorrow-night
                  doom-wilmersdorf doom-nova))
      (doom-light-themes
       '(doom-one-light doom-gruvbox-light doom-solarized-light
                        doom-tomorrow-day doom-opera-light doom-nord-light)))
  (defun load-random-theme ()
    "Load a random theme, light during the day"
    (interactive)
    (let* ((current-hour (string-to-number (format-time-string "%H")))
           (current-themes (if (or (>= current-hour 19) (< current-hour 7))
                               doom-dark-themes doom-light-themes)))
      (load-theme (nth (random (length current-themes)) current-themes) t))))

(run-at-time "1 hour" 3600 #'load-random-theme)

(setq doom-theme 'doom-one
      doom-font (font-spec :family "monospace" :size 18)
      ;; doom-serif-font (font-spec :family "monospace-serif" :size 18)
      doom-variable-pitch-font (font-spec :family "sans-serif") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Noto Serif"))

;; integrate with freedesktop secret service
(require 'secrets)
(setq auth-sources '(default "secrets:Main"))

;; org configuration
(setq org-directory "~/box/personal"
      org-ellipsis " ▼ ")
(after! org
  ;; aggressive logging
  (setq org-log-into-drawer t
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-refile 'time
        org-treat-insert-todo-heading-as-state-change t)
  (setq org-todo-keywords '((sequence "TODO(t!)" "PROJ(p!)" "|" "DONE(d!)")
                            (sequence "[ ](T!)" "[-](P!)" "[?](M!)" "|" "[X](D!)")
                            (sequence "NEXT(n!)" "WAIT(w@)" "HOLD(h!)" "|" "ABRT(c@)")))
  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . emacs)))
  (setq org-file-apps-gnu
        ;; I don't have a mailcap file
        '((remote . emacs)
          ("\\.mp4\\'" . "mpv %s")
          ("\\.mkv\\'" . "mpv %s")
          (system . "xdg-open %s")
          (t . "xdg-open %s"))
        ;; always use UUID's in org headline links
        org-id-link-to-org-use-id t)
  (setq org-capture-templates
        '(("t" "todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:" :prepend t :kill-buffer t)
          ("d" "todo with deadline" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\nDEADLINE: %^t\n%i\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:" :prepend t :kill-buffer t)
          ("s" "scheduled todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\nSCHEDULED: %^t\n%i\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:" :prepend t :kill-buffer t)
          ("e" "event" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* %^t %? :event:\n%i\n%^{LOCATION}p" :prepend t :kill-buffer t)
          ("n" "notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("p" "templates for projects")
          ("pt" "project todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:" :prepend t :kill-buffer t)
          ("pn" "project notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i\n%a\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:" :prepend t :kill-buffer t)
          ("pc" "project changelog" entry
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i\n%a\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:" :prepend t :kill-buffer t))))

(use-package! org-super-agenda
  ;; :commands (org-super-agenda-mode)
  :after org-agenda
  ;; :init
  :config
  (org-super-agenda-mode)
  (setq org-agenda-prefix-format
        '((agenda . " %i %?-12t% s")
          (todo . " %i ")
          (tags . " %i ")
          (search . " %i "))
        org-super-agenda-groups
        '((:name "Schedule"
           :time-grid t)
          (:name "Today"
           :scheduled today)
          (:name "Due today"
           :deadline today)
          (:name "Overdue"
           :deadline past)
          (:name "Due soon"
           :deadline future)
          (:name "Waiting"
           :todo "WAIT"
           :order 98)
          (:name "Scheduled earlier"
           :scheduled past))))

;; disable special keybindings on header lines
(after! org-super-agenda
  (setq org-super-agenda-header-map nil))

;; (after! org-pomodoro
;;   (setq org-pomodoro-manual-break t))

(after! org-roam
  (setq org-roam-directory org-directory)
  (setq org-roam-capture-templates
        '(("d" "default" plain
           (function org-roam-capture--get-point) "%?"
           :file-name "%<%Y-%m-%d-%Hh%Mm%S>-${slug}"
           :head "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")))
  (defun my-title-to-slug (title)
    "Convert TITLE to a filename-suitable slug."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                      ("--*" . "-") ;; remove sequential hyphens
                      ("^-" . "")   ;; remove starting hyphen
                      ("-$" . ""))) ;; remove ending hyphen
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug))))
  (setq org-roam-title-to-slug-function #'my-title-to-slug))

;; (after! org-noter
;;   (setq org-noter-always-create-frame nil))

(after! deft
  (setq deft-directory org-directory))

;; nov.el
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package! org-caldav
  :after org
  :config
  (setq org-caldav-url "https://nextcloud.hpfr.net/remote.php/dav/calendars/lh"
        org-caldav-calendars '((:calendar-id "school-1"
                                :files ("~/box/personal/school.org")
                                :inbox "~/box/personal/school-inbox.org"))
        org-caldav-backup-file "~/box/personal/org-caldav-backup.org"
        org-caldav-save-directory "~/box/personal/")
  ;; This makes sure to-do items as a category can show up on the calendar
  (setq org-icalendar-include-todo t)
  ;; This ensures all org "deadlines" show up as due dates
  (setq org-icalendar-use-deadline '(todo-due))
  ;; This ensures "scheduled" org items show up as start times
  (setq org-icalendar-use-scheduled '(todo-start)))

(use-package! org-vcard
  :after org)

;; use pdf-tools for latex rather than zathura, etc
(setq +latex-viewers '(pdf-tools))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq nov-save-place-file (concat doom-cache-dir "nov-places"))
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

;; find syncthing conflicts
(use-package! emacs-conflict)

;; org-ify nov
(use-package! shrface)

