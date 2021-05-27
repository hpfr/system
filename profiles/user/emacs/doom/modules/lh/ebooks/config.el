;;; nov
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

;;; calibredb
(use-package! calibredb
  :init (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir "~/box/library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/box/library"))))
