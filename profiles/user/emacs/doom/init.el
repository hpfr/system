;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(defvar my/gui-capable-hosts '("moon-watcher" "dave" "poole"))
(defvar my/personal-hosts '("moon-watcher" "dave"))

(doom!

 :completion
 company
 (vertico +icons)

 :ui
 doom
 doom-dashboard
 (emoji +unicode)
 hl-todo
 modeline
 nav-flash
 ophints
 (popup
  +defaults
  +all)
 (treemacs +lsp)
 (vc-gutter +pretty)
 window-select
 workspaces
 zen

 :editor
 (evil +everywhere)
 file-templates
 fold
 (format +onsave)
 lispy
 multiple-cursors
 rotate-text
 snippets
 ;; word-wrap

 :emacs
 (dired
  +icons)
 electric
 (ibuffer +icons)
 undo
 vc

 :term
 eshell
 vterm

 :checkers
 syntax
 spell
 grammar

 :tools
 biblio
 (debugger +lsp)
 direnv
 editorconfig
 (eval +overlay)
 (lookup
  +dictionary
  +docsets)
 (lsp +peek)
 (magit +forge)
 ;; make
 (:when (member (system-name) my/gui-capable-hosts)
   pdf)
 rgb
 tree-sitter
 ;; upload

 :lang
 (beancount +lsp)
 (cc +lsp +tree-sitter)
 (csharp +lsp +dotnet +tree-sitter)
 data
 emacs-lisp
 (go +lsp +tree-sitter)
 (java +lsp +tree-sitter)
 (javascript +lsp +tree-sitter)
 (latex
  +latexmk
  +cdlatex
  +lsp
  +fold)
 markdown
 (nix +tree-sitter)
 (org
  +dragndrop
  +hugo
  +noter
  +pandoc
  +pomodoro
  +present
  +roam2)
 (python +lsp +pyright +tree-sitter)
 (rest +jq)
 (sh +fish +tree-sitter)
 (web +lsp +tree-sitter)
 yaml

 ;; :os
 ;; tty

 (:when (member (system-name) my/personal-hosts)
   :email
   (mu4e +org +gmail))

 :app
 ;; everywhere
 (:when (member (system-name) my/personal-hosts)
   irc
   (rss +org))

 :config
 ;;literate
 (default +bindings +smartparens)

 :lh                                    ; personal modules
 (:when (member (system-name) my/personal-hosts)
   org
   email)
 ebooks
 unicode)

(setq use-package-inject-hooks t
      evil-respect-visual-line-mode t)
