;; -*- no-byte-compile: t; -*-
;; doom includes this by default
(package! magit-gitflow :disable t)
(package! vundo)
(package! shrface)
(package! emacs-conflict :recipe
  (:host github :repo "ibizaman/emacs-conflicts"))
(package! vimrc-mode)
(package! cook-mode
  :recipe (:host github
            :repo "cooklang/cook-mode"))
(package! unfill)
(package! aas)
(package! laas)
(package! lazytab :recipe (:host github :repo "karthink/lazytab"))
(package! git-auto-commit-mode)
(package! lexic)
(package! magit-annex)
(package! annexview :recipe (:host nil :repo "https://git.kyleam.com/annexview"))
(package! disk-usage)
(package! daemons)
(package! ztree)
(package! tldr)
(package! dired-dragon                  ; drag and drop
  :recipe (:host github :repo "jeetelongname/dired-dragon")
  :disable t)
(package! 0x0)
(package! osm)
(when (package! ement                   ; matrix
        :recipe (:host github :repo "alphapapa/ement.el"))
  (package! plz
    :recipe (:host github :repo "alphapapa/plz.el")))
(package! anki-editor)
(package! anki
  :recipe (:host github :repo "chenyanming/anki.el")
  :disable t)
(package! elpher)
(package! form-feed)
;; pretty hacky
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :disable t)
(package! edwina :disable t)
