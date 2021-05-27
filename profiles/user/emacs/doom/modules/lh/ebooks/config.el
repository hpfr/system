;;; nov
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places"))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;; calibredb
(use-package! calibredb
  :init (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir "~/nc/library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/nc/library"))))
