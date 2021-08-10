(setq org-directory "~/nc/personal"
      org-ellipsis " ▼ ")

(after! org
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
      (lh/org-set-time-file-property "last_modified")))

  (add-hook 'before-save-hook #'lh/org-set-last-modified))

(after! org
  ;; aggressive logging
  (setq org-log-into-drawer t
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-refile 'time
        org-treat-insert-todo-heading-as-state-change t
        org-todo-keywords
        '((sequence
           "TODO(t!)"             ; a task that needs doing
           "NEXT(n!)"             ; a task that is being worked on
           "PROJ(p!)"             ; a project which usually contains other tasks
           "WAIT(w@)"             ; a task blocked by an external factor
           "HOLD(h@)"             ; a task blocked by me
           "|"
           "DONE(d!)"                   ; a task that was completed
           "ABRT(c@)")                  ; a task that was cancelled
          (sequence
           "[ ](T!)"                    ; a task that needs doing
           "[-](P!)"                    ; an in-progress task
           "[?](W!)"                    ; a task that is waiting
           "|"
           "[X](D!)"))                  ; a completed task
        org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . emacs))
        org-file-apps-gnu
        ;; I don't have a mailcap file
        '((remote . emacs)
          ("\\.mp4\\'" . "mpv %s")
          ("\\.mkv\\'" . "mpv %s")
          (system . "xdg-open %s")
          (t . "xdg-open %s"))
        ;; always use UUID's in org headline links
        org-id-link-to-org-use-id t
        org-startup-folded 'showall
        ;; no shouting
        org-attach-auto-tag "attach"
        ;; export
        org-latex-pdf-process '("tectonic --outdir=%o %f")))

(use-package! doct
  :commands (doct doct-add-to))

(after! org-capture
  (setq doct-default-entry-type 'entry
        org-capture-templates
        (doct
         '((:group "standard todo's" :prepend t :kill-buffer t
            :file +org-capture-todo-file :headline "Inbox"
            :template ("* %{todo-state} %?"
                       ":LOGBOOK:"
                       "- State \"%{todo-state}\"       from              %U"
                       ":END:"
                       "%i")
            :children
            (("todo" :keys "t"
              :todo-state "TODO")
             ("upcoming todo" :keys "u"
              :todo-state "NEXT")))

           ("event" :keys "e" :prepend t :kill-buffer t
            :file "personal-calendar-inbox.org"
            :template ("* %?"
                       "%^{LOCATION}p"
                       "%^t"
                       "%i"))
           ("notes" :keys "n" :prepend t :kill-buffer t
            :file +org-capture-notes-file :headline "Inbox"
            :template ("* %u %?"
                       "%i"))
           ("email" :keys "m" :prepend t :kill-buffer t
            :file +org-capture-todo-file :headline "Inbox"
            :contexts (:in-mode "mu4e-\\(view\\|headers\\)-mode")
            :template ("* TODO %?"
                       ":LOGBOOK:"
                       "- State \"TODO\"       from              %U"
                       ":END:"
                       "- %a"
                       "%i"))

           ("project-local" :keys "p" :prepend t :kill-buffer t
            :file +org-capture-project-todo-file :headline "Inbox"
            :template ("* %U %?"
                       "- %a"
                       "%i")
            :children
            (("project-local todo" :keys "t"
              :template ("* TODO %?"
                         ":LOGBOOK:"
                         "- State \"TODO\"       from              %U"
                         ":END:"
                         "- %a"
                         "%i"))
             ("project-local notes" :keys "n"
              :file +org-capture-project-notes-file)
             ("project-local changelog" :keys "c"
              :file +org-capture-project-changelog-file :headline "Unreleased")))

           ("centralized project templates" :keys "o" :prepend t :kill-buffer t
            :file +org-capture-central-project-todo-file :headline "Inbox"
            :template ("* %U %?"
                       "- %a"
                       "%i")
            :children
            (("project-local todo" :keys "t"
              :template ("* TODO %?"
                         ":LOGBOOK:"
                         "- State \"TODO\"       from              %U"
                         ":END:"
                         "- %a"
                         "%i"))
             ("project-local notes" :keys "n"
              :file +org-capture-central-project-notes-file)
             ("project-local changelog" :keys "c"
              :file +org-capture-central-project-changelog-file :headline "Unreleased")))))))

(use-package! org-super-agenda
  ;; :commands (org-super-agenda-mode)
  :after org-agenda
  ;; :init
  :config
  (org-super-agenda-mode)
  ;; if I've explicitly scheduled a task with a deadline (maybe an assignment
  ;; doesn't release until the scheduled time), don't warn me until the
  ;; scheduled time
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
        org-agenda-prefix-format
        '((agenda . " %i %?-12t% s")
          (todo . " %i ")
          (tags . " %i ")
          (search . " %i "))
        ;; disable special keybindings on header lines
        org-super-agenda-header-map nil
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

(after! org-pomodoro
  (setq org-pomodoro-manual-break t))

(after! org-roam
  (setq org-roam-directory org-directory
        org-roam-file-exclude-regexp "^org-caldav-backup.org"
        +org-roam-open-buffer-on-find-file nil
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y-%m-%d-%Hh%Mm%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)))
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                                    (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                                         (ucs-normalize-NFC-string
                                          (apply #'string (seq-remove #'nonspacing-mark-p
                                                                      (ucs-normalize-NFD-string s)))))
                 (cl-replace (title pair)
                             (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ; convert anything not alphanumeric
                        ("--*" . "-")                   ; remove sequential hyphens
                        ("^-" . "")                     ; remove starting hyphen
                        ("-$" . "")))                   ; remove ending hyphen
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug))))))

;; I'm ok with longer link titles
(after! org-cliplink
  (setq org-cliplink-max-length 120))

(after! org-noter
  ;;   (setq org-noter-always-create-frame nil)
  (map!
   :map pdf-view-mode-map
   :n "i" 'org-noter-insert-note))

;; TODO: this block causes org-mode to load immediately
(use-package! org-caldav
  :after org
  :config
  (setq org-caldav-url "https://nextcloud.hpfr.net/remote.php/dav/calendars/lh"
        org-caldav-backup-file (concat doom-local-dir "org-caldav/backup.org")
        org-caldav-save-directory (concat doom-local-dir "org-caldav/"))
  ;; This makes sure to-do items as a category can show up on the calendar
  (setq org-icalendar-include-todo t)
  ;; This ensures all org "deadlines" show up as due dates
  (setq org-icalendar-use-deadline '(todo-due))
  ;; This ensures "scheduled" org items show up as start times
  (setq org-icalendar-use-scheduled '(todo-start)))

(use-package! org-chef
  :after org)

(use-package! org-vcard
  :after org)

(after! deft
  (setq deft-directory org-directory))

;; TODO: test featurep org and moving this elsewhere
(after! elfeed
  (setq rmh-elfeed-org-files
        (list (expand-file-name  "2021-04-26-12h06m49-elfeed.org" org-directory))))

(after! org-re-reveal
  (setq org-re-reveal-theme "night"
        org-re-reveal-transition "fade"
        org-re-reveal-plugins '(markdown notes search zoom chalkboard)
        org-re-reveal-progress 'nil
        org-re-reveal-head-preamble (concat "
<link rel=\"stylesheet\" href=\"" org-re-reveal-root "../revealjs-plugins-rajgoel/chalkboard/style.css\">
<link rel=\"stylesheet\" href=\"https://use.fontawesome.com/releases/v5.15.3/css/all.css\">
"))

  (add-to-list 'org-re-reveal-plugin-config '(chalkboard "RevealChalkboard" "../revealjs-plugins-rajgoel/chalkboard/plugin.js")))

(load! "private.el")
