;; -*- no-byte-compile: t; -*-
(package! emacs-conflict :recipe
  (:host github :repo "ibizaman/emacs-conflicts"))
(package! vimrc-mode)
(package! cook-mode
  :recipe (:host github
            :repo "cooklang/cook-mode"))
(package! aas :pin "b1a436922ba06ab9e1a5cc222f1a4f25a7697231")
(package! laas :pin "fa32c7affc1d232e5ab63b7c7a8a29461a465536")
(package! lexic)
(package! disk-usage)
(package! ztree :pin "f05677f9696e573c8c607e8876fb4a0cccbc491f")
(package! tldr)
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
