(use-package! shrface
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  (add-hook 'nov-mode-hook #'shrface-mode)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))
