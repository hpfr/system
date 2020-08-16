;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; TODO: make path more relative
(add-to-list '+emacs-lisp-disable-flycheck-in-dirs
             "/home/lh/repos/system/profiles/user/emacs/doom/")

;; highlight links in comments and strings
(define-globalized-minor-mode global-goto-address-prog-mode goto-address-prog-mode
  (lambda () (goto-address-prog-mode 1)))
(global-goto-address-prog-mode 1)

;; Replace selection when inserting text
(delete-selection-mode 1)

(setq doom-font (font-spec :family "monospace" :size 18)
      doom-variable-pitch-font (font-spec :family "sans-serif") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Noto Serif" :size 18)
      doom-big-font (font-spec :family "monospace" :size 28))

;; line numbers are unnecessary with easymotion and avy
(setq display-line-numbers-type nil)

;; integrate with freedesktop secret service
(require 'secrets)
(setq auth-sources '(default "secrets:Main"))

;; irc configuration
(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 7000
                     :nick "hpfr"
                     ;; TODO: secrets command works, but circe doesn't
                     :sasl-username ,(secrets-get-attribute "Main" "Freenode" :UserName)
                     :sasl-password ,(secrets-get-secret "Main" "Freenode")
                     :channels ("#nixos" "#home-manager" "#org-mode"))))

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
  (setq org-super-agenda-groups
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

;; find syncthing conflicts
(use-package! emacs-conflict)
