;; -*- no-byte-compile: t; -*-
(package! emacs-conflict :recipe
  (:host github :repo "ibizaman/emacs-conflicts"))
(package! vimrc-mode)
(package! lexic)
(package! disk-usage)
(package! ztree :pin "f05677f9696e573c8c607e8876fb4a0cccbc491f")
(package! dired-dragon                  ; drag and drop
  :recipe (:host github :repo "jeetelongname/dired-dragon"))
(when (package! ement                   ; matrix
        :recipe (:host github :repo "alphapapa/ement.el"))
  (package! plz
    :recipe (:host github :repo "alphapapa/plz.el")))
(package! elpher)
(package! yequake
  :disable (not (member system-name gui-capable-hosts)))
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :disable (not (member system-name gui-capable-hosts)))
(package! edwina)
