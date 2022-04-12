;;; -*- lexical-binding: t; -*-
(setq org-directory (expand-file-name "~/exocortex/")
      org-ellipsis " ▼ ")

(after! org
  ;; aggressive logging
  (setq org-log-into-drawer t
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-refile 'time
        org-treat-insert-todo-heading-as-state-change t
        org-todo-keywords
        '((sequence
           "TODO(t!)"                   ; a task that needs doing
           "NEXT(n!)"                   ; a task that is being worked on
           "WAIT(w@)"                   ; a task blocked by an external factor
           "HOLD(h@)"                   ; a task blocked by me
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
        `((auto-mode . emacs)
          (directory . emacs)
          (,(rx ".mm" eos) . default)
          (,(rx "." (? "x") "htm" (? "l") eos) . default)
          (,(rx ".pdf" eos) . emacs))
        org-file-apps-gnu
        ;; I don't have a mailcap file
        `((remote . emacs)
          (,(rx ".mp4" eos) . "mpv %s")
          (,(rx ".mkv" eos) . "mpv %s")
          (system . "xdg-open %s")
          (t . "xdg-open %s"))
        org-id-link-to-org-use-id 'create-if-interactive
        org-use-property-inheritance '("ROAM_EXCLUDE")
        org-startup-folded 'showall
        ;; no shouting
        org-attach-auto-tag "attach"
        ;; default maxlevel is 3. headings don't get very deep usually, so
        ;; negligible impact, but can be nice to have
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        ;; ignore done tasks when refiling. org-done-keywords is buffer-local to
        ;; Org, so this works when refile calls it
        org-refile-target-verify-function
        (lambda () (not (member (nth 2 (org-heading-components))
                                org-done-keywords)))
        org-refile-allow-creating-parent-nodes 'confirm
        org-format-latex-header "\\documentclass{my-article}
\\usepackage[usenames]{color}
\[NO-PACKAGES]
\[NO-DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

  ;; tag for optimizing agenda files doesn't need to be inherited
  (add-to-list 'org-tags-exclude-from-inheritance "has-todo")

  (org-edna-mode)

  (require 'org-transclusion)
  (map! :map org-mode-map
        :localleader "X" #'org-transclusion-mode)

  (defun my/org-fold-done ()
    "Fold all completed tasks"
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (while (outline-previous-heading)
        (when (org-entry-is-done-p)
          (outline-hide-subtree)))))

  (define-minor-mode my/org-fold-done-mode
    "Make completed tasks appear folded by default"
    :global t
    :group 'my/org-fold-done
    :lighter " org-fold-done"
    :init-value nil
    (if my/org-fold-done-mode
        (add-hook 'org-mode-hook #'my/org-fold-done)
      (remove-hook 'org-mode-hook #'my/org-fold-done)))

  (my/org-fold-done-mode))

(after! oc
  ;; TODO biblio module should set from citar-bibliography
  (setq org-cite-global-bibliography '("~/nc/research/main.bib")
        org-cite-export-processors '((latex biblatex "ieee")
                                     (t csl))))

;; better latex preview
(add-hook 'org-mode-hook #'org-auctex-mode)

;; export
(after! ox
  (setq org-export-with-toc nil))

(after! ox-html
  (setq org-html-doctype "html5"
        org-html-html5-fancy t))

(after! ox-latex
  (let ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                            ("\\subsection{%s}" . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            ("\\paragraph{%s}" . "\\paragraph*{%s}")
                            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
    (setcdr (assoc "article" org-latex-classes)
            `("\\documentclass{my-article}\n[NO-DEFAULT-PACKAGES][NO-PACKAGES]\n\\KOMAoptions{paper=letter,DIV=11}\n\\recalctypearea\n[EXTRA]"
              ,@article-sections)))
  (setq org-latex-compiler "lualatex"
        org-latex-tables-booktabs t
        ;; TODO turn off hypersetup metadata if pdfx is used, because it handles that data
        org-latex-hyperref-template
        "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 breaklinks=true,
 colorlinks=true,
 linkcolor=link,
 urlcolor=url,
 citecolor=cite
}
\\urlstyle{same}\n"))
;; (after! ox-html
;;   (setq org-html-head "<link rel=\"stylesheet\" href=\"https://unpkg.com/@picocss/pico@latest/css/pico.classless.min.css\">"))

;; hide individual blocks with #+hide: t on the line preceding #+begin_...
(add-hook! 'org-mode-hook
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx bol "#+begin_" (or "src" "quote")) nil t)
      (and (save-excursion
             (goto-char (line-beginning-position 0))
             (looking-at-p (rx (* whitespace) "#+hide: t\n")))
           (org-hide-block-toggle t)))))

;;; capture
;; org-capture can load before org if you call it too early
(after! org
  (setq org-capture-templates
        (doct
         `((:group "standard todo's"
            :prepend t :empty-lines-after 1 :kill-buffer t
            :file "inbox.org"
            :template ("* %{todo-state} %?"
                       ":LOGBOOK:"
                       "- State \"%{todo-state}\"       from              %U"
                       ":END:"
                       "%i")
            :children
            (("task" :keys "t"
              :todo-state "TODO")
             ("upcoming task" :keys "u"
              :todo-state "NEXT")))

           ("event" :keys "e"
            :prepend t :empty-lines-after 1 :kill-buffer t
            :file "events/inbox.org"
            :template ("* %?"
                       "%^{LOCATION}p%^t"
                       "%i")
            :hook ,(lambda () (org-id-get (point) 'create)))
           ("notes" :keys "n"
            :prepend t :empty-lines-after 1 :kill-buffer t
            :file +org-capture-notes-file
            :template ("* %?"
                       "%i"))

           ("project-local" :keys "p"
            :prepend t :empty-lines-after 1 :kill-buffer t
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
              :file +org-capture-project-changelog-file :headline "Unreleased")))))))

;;; org-roam
(after! org-roam
  (setq org-roam-directory org-directory
        org-roam-file-exclude-regexp
        (rx bol (literal (file-name-as-directory org-roam-directory))
            (or ".attach" "events" "school-events") "/")
        org-roam-db-node-include-function
        (lambda ()
          (not (or (org-entry-get (point) "ROAM_EXCLUDE" 'selective)
                   (and (member (or org-attach-auto-tag "ATTACH") (org-get-tags))
                        (member (org-get-heading t t t) '("Attachments" "Resources"))))))
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y-%m-%d-%Hh%Mm%S>-${slug}.org"
                              "#+title: ${title}\n\n")
           :unnarrowed t)))

  ;; this works based on what node your cursor is in when you save, which is not
  ;; great. ideally, there would be a package that could parse Git revision history
  ;; for modifications within nodes
  (org-roam-timestamps-mode)

  (setq consult-org-roam-grep-func #'consult-ripgrep)
  (map! :map org-mode-map
        ;; TODO: backlinks needs autoload
        :localleader
        :prefix ("m" . "org-roam")
        "b" #'consult-org-roam-backlinks
        "l" #'consult-org-roam-forward-links))

;;; agenda
(after! org-agenda
  (setq
   ;; if I've explicitly scheduled a task with a deadline (maybe an assignment
   ;; doesn't release until the scheduled time), don't warn me until the
   ;; scheduled time
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-skip-scheduled-if-deadline-is-shown 'not-today
   ;; these items will appear in the standard agenda view and have already
   ;; been prioritized
   org-agenda-todo-ignore-deadlines 'near
   org-agenda-todo-ignore-scheduled 'all
   ;; org-agenda-todo-ignore-with-date t
   )

  ;; better agenda todo prefix
  (defun vulpea-agenda-category (&optional len)
    "Get category of item at point for agenda.
Category is defined by one of the following items:
- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension
When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.
Usage example:
  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))
Refer to `org-agenda-prefix-format' for more information."
    (require 'vulpea-buffer)
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (vulpea-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result) "…")
        result)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12(vulpea-agenda-category 12)%?-12t% s")
          (todo . " %i %-12(vulpea-agenda-category 12) ")
          (tags . " %i %-12(vulpea-agenda-category 12) ")
          (search . " %i %-12(vulpea-agenda-category 12) ")))

  ;; dynamic agenda files with roam
  ;; good stopgap until roam agenda features come out
  ;; if todo's are missing, run:
  ;; (dolist (file (org-roam-list-files))
  ;;   (message "processing %s" file)
  ;;   (with-current-buffer (or (find-buffer-visiting file)
  ;;                            (find-file-noselect file))
  ;;     (vulpea-has-todo-update-tag)
  ;;     (save-buffer)))
  (defun vulpea-has-todo-p ()
    "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                           ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-has-todo-update-tag ()
    "Update HAS-TODO tag in the current buffer."
    (require 'vulpea-buffer)
    (require 'org-roam)
    (when (and (not (active-minibuffer-window))
               (org-roam-file-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-has-todo-p)
              (setq tags (cons "has-todo" tags))
            (setq tags (remove "has-todo" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-has-todo-files ()
    "Return a list of note files containing 'has-todo' tag." ;
    (require 'org-roam)
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"has-todo\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-has-todo-files))
    (dolist (calendar org-caldav-calendars)
      (appendq! org-agenda-files
                (nth 1 (memq :files calendar))
                (list (nth 1 (memq :inbox calendar)))))
    ;; event files may have todo's so remove duplicates
    (setq org-agenda-files (delete-dups
                            (mapcar #'expand-file-name org-agenda-files))))

  (add-hook 'find-file-hook #'vulpea-has-todo-update-tag)
  (add-hook 'before-save-hook #'vulpea-has-todo-update-tag)
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)

  (org-super-agenda-mode))

;;; org-super-agenda
(defadvice! org-super-agenda-mode-silence (fn &rest args)
  "Disable the org-super-agenda-mode toggle message"
  :around 'org-super-agenda-mode
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply fn args)))
(after! org-super-agenda
  (setq
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

;;; org-caldav
;; must be available for parsing to agenda files
(setq org-caldav-calendars
      `((:calendar-id "school-1"
         :files
         ,(delete (expand-file-name "school-events/inbox.org" org-directory)
                  (directory-files (expand-file-name "school-events" org-directory) t "^[^.]"))
         :inbox ,(expand-file-name "school-events/inbox.org" org-directory))
        (:calendar-id "personal-1"
         :files
         ,(delete (expand-file-name "events/inbox.org" org-directory)
                  (directory-files (expand-file-name "events" org-directory) t "^[^.]"))
         :inbox ,(expand-file-name "events/inbox.org" org-directory))))
(after! org-caldav
  (setq org-caldav-url "https://nextcloud.hpfr.net/remote.php/dav/calendars/lh"
        org-caldav-backup-file (expand-file-name "org-caldav/backup.org" doom-local-dir)
        org-caldav-save-directory (expand-file-name "org-caldav/" doom-local-dir)
        ;; This makes sure to-do items as a category can show up on the calendar
        org-icalendar-include-todo t
        ;; This ensures all org "deadlines" show up as due dates
        org-icalendar-use-deadline '(todo-due)
        ;; This ensures "scheduled" org items show up as start times
        org-icalendar-use-scheduled '(todo-start)))

;; use hyphens instead of underscores in roam filenames
(after! org-roam-node
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    (let ((title (org-roam-node-title node)))
      (cl-flet* ((nonspacing-mark-p (char)
                                    (memq char ucs-normalize-combining-chars))
                 (strip-nonspacing-marks (s)
                                         (ucs-normalize-NFC-string
                                          (apply #'string (seq-remove #'nonspacing-mark-p
                                                                      (ucs-normalize-NFD-string s)))))
                 (cl-replace (title pair)
                             (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ; convert anything not alphanumeric
                        ("--*" . "-")   ; remove sequential hyphens
                        ("^-" . "")     ; remove starting hyphen
                        ("-$" . "")))   ; remove ending hyphen
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug))))))

(after! org-pomodoro
  (setq org-pomodoro-manual-break t))

;; I'm ok with longer link titles
(after! org-cliplink
  (setq org-cliplink-max-length 120))

(after! org-noter
  ;;   (setq org-noter-always-create-frame nil)
  (map!
   :map pdf-view-mode-map
   :n "i" 'org-noter-insert-note))

(after! elfeed-org
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
