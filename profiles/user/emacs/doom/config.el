;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; TODO: make path more relative
(add-to-list '+emacs-lisp-disable-flycheck-in-dirs
             "~/repos/system/profiles/user/emacs/doom/")

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
                   doom-gruvbox doom-henna doom-horizon
                   doom-laserwave doom-material doom-molokai doom-monokai-classic
                   doom-moonlight doom-nord doom-oceanic-next doom-old-hope
                   doom-opera doom-palenight doom-peacock doom-rouge doom-snazzy
                   doom-solarized-dark doom-sourcerer doom-spacegrey
                   doom-tomorrow-night doom-wilmersdorf
                   ;; doom-ephemeral doom-nova doom-fairy-floss doom-manegarm
                   ;; doom-acario-light
                   ))
       (doom-light-themes
        '(doom-one-light doom-gruvbox-light doom-solarized-light
                         doom-tomorrow-day doom-opera-light
                         ;; doom-nord-light doom-acario-light
                         )))
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

 (use-package! org
   :hook ((before-save . lh/org-set-last-modified))
   :config
   ;; https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el#L2822-L2875
   (defvar lh/org-created-property-name "created"
     "The name of the org-mode property that stores the creation date of the entry")

   (defun lh/org-set-created-property (&optional active name)
     "Set a property on the entry giving the creation time.
 By default the property is called CREATED. If given, the ‘NAME’
 argument will be used instead. If the property already exists, it
 will not be modified.
 If the function sets CREATED, it returns its value."
     (interactive)
     (let* ((created (or name lh/org-created-property-name))
            (fmt (if active "<%s>" "[%s]"))
            (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
       (unless (org-entry-get (point) created nil)
         (org-set-property created now)
         now)))

   (defun lh/org-find-time-file-property (property &optional anywhere)
     "Return the position of the time file PROPERTY if it exists.
 When ANYWHERE is non-nil, search beyond the preamble."
     (save-excursion
       (goto-char (point-min))
       (let ((first-heading
              (save-excursion
                (re-search-forward org-outline-regexp-bol nil t))))
         (when (re-search-forward (format "^#\\+%s:" property)
                                  (if anywhere nil first-heading)
                                  t)
           (point)))))

   (defun lh/org-has-time-file-property-p (property &optional anywhere)
     "Return the position of time file PROPERTY if it is defined.
 As a special case, return -1 if the time file PROPERTY exists but
 is not defined."
     (when-let ((pos (lh/org-find-time-file-property property anywhere)))
       (save-excursion
         (goto-char pos)
         (if (and (looking-at-p " ")
                  (progn (forward-char)
                         (org-at-timestamp-p 'lax)))
             pos
           -1))))

   (defun lh/org-set-time-file-property (property &optional anywhere pos)
     "Set the time file PROPERTY in the preamble.
 When ANYWHERE is non-nil, search beyond the preamble.
 If the position of the file PROPERTY has already been computed,
 it can be passed in POS."
     (when-let ((pos (or pos
                         (lh/org-find-time-file-property property))))
       (save-excursion
         (goto-char pos)
         (if (looking-at-p " ")
             (forward-char)
           (insert " "))
         (delete-region (point) (line-end-position))
         (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
           (insert now)))))

   (defun lh/org-set-last-modified ()
     "Update the LAST_MODIFIED file property in the preamble."
     (when (derived-mode-p 'org-mode)
       (lh/org-set-time-file-property "last_modified"))))

 (after! org
   ;; aggressive logging
   (setq org-log-into-drawer t
         org-log-redeadline 'time
         org-log-reschedule 'time
         org-log-refile 'time
         org-treat-insert-todo-heading-as-state-change t)
   (setq org-todo-keywords
         '((sequence
            "TODO(t!)" ; a task that needs doing
            "NEXT(n!)" ; a task that is being worked on
            "PROJ(p!)" ; a project which usually contains other tasks
            "WAIT(w@)" ; a task blocked by an external factor
            "HOLD(h@)" ; a task blocked by me
            "|"
            "DONE(d!)"                   ; a task that was completed
            "ABRT(c@)")                  ; a task that was cancelled
           (sequence
            "[ ](T!)"                    ; a task that needs doing
            "[-](P!)"                    ; an in-progress task
            "[?](W!)"                    ; a task that is waiting
            "|"
            "[X](D!)")))                 ; a completed task
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
   (setq org-startup-folded 'showall)
   (setq org-capture-templates
         '(("t" "todo" entry
            (file+headline +org-capture-todo-file "Inbox")
            "* TODO %?\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n%i" :prepend t :kill-buffer t)
           ("d" "todo with deadline" entry
            (file+headline +org-capture-todo-file "Inbox")
            "* TODO %?\nDEADLINE: %^t\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n%i" :prepend t :kill-buffer t)
           ("s" "scheduled todo" entry
            (file+headline +org-capture-todo-file "Inbox")
            "* TODO %?\nSCHEDULED: %^t\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n%i" :prepend t :kill-buffer t)
           ("e" "event" entry
            (file "personal-calendar-inbox.org")
            "* %? :event:\n%^{LOCATION}p\n%^t\n%i" :prepend t :kill-buffer t)
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

 (after! org-pomodoro
   (setq org-pomodoro-manual-break t))

 (after! org-roam
   (setq org-roam-directory org-directory
         org-roam-file-exclude-regexp "^org-caldav-backup.org")
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

(after! org-noter
  ;;   (setq org-noter-always-create-frame nil)
  (map!
   :map pdf-view-mode-map
   :n "i" 'org-noter-insert-note))

(after! deft
  (setq deft-directory org-directory))


(use-package! calibredb
  :init (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir "~/box/library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/box/library"))))

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

(use-package! org-chef
  :after org)

;; (use-package! org-vcard
;;   :after org)

;; use pdf-tools for latex rather than zathura, etc
(setq +latex-viewers '(pdf-tools))

;; nov.el
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq nov-save-place-file (concat doom-cache-dir "nov-places"))
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; find syncthing conflicts
(use-package! emacs-conflict)

;; org-ify nov
(use-package! shrface)

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

(use-package! eww
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

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
