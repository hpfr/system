;; -*- no-byte-compile: t; -*-
(package! vundo :recipe (:host github :repo "casouri/vundo"))
(package! shrface)
(package! emacs-conflict :recipe
  (:host github :repo "ibizaman/emacs-conflicts"))
(package! vimrc-mode)
(package! cook-mode
  :recipe (:host github
            :repo "cooklang/cook-mode"))
(package! aas)
(package! laas)
(package! lazytab :recipe (:host github :repo "karthink/lazytab"))
(package! git-auto-commit-mode)
(package! lexic)
(package! disk-usage)
(package! daemons)
(package! ztree)
(package! tldr)
(package! dired-dragon                  ; drag and drop
  :recipe (:host github :repo "jeetelongname/dired-dragon")
  :disable t)
(package! 0x0)
(when (package! ement                   ; matrix
        :recipe (:host github :repo "alphapapa/ement.el"))
  (package! plz
    :recipe (:host github :repo "alphapapa/plz.el")))
(package! anki-editor)
(package! anki
  :recipe (:host github :repo "chenyanming/anki.el")
  :disable t)
(package! elpher)
;; pretty hacky
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :disable t)
(package! edwina :disable t)
