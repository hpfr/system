;;; -*- lexical-binding: t; -*-
(use-package! shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package! eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(use-package! nov
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

(use-package! anki
  :defer t
  :init
  (add-hook 'anki-mode-hook #'shrface-mode)
  (add-hook 'anki-card-mode-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq anki-shr-rendering-functions (append anki-shr-rendering-functions shr-external-rendering-functions)))
