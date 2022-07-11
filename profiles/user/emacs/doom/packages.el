;; -*- no-byte-compile: t; -*-
;; doom includes this by default
(package! magit-gitflow :disable t)
(package! shrface)
(package! emacs-conflict :recipe
  (:host github :repo "ibizaman/emacs-conflicts"))
(package! vimrc-mode)
(package! cook-mode
  :recipe (:host github :repo "cooklang/cook-mode"))
(package! unfill)
(package! spaceship-mode
  :recipe (:host github :repo "tenbillionwords/spaceship-mode"))
(package! aas)
(package! laas)
(package! lazytab :recipe (:host github :repo "karthink/lazytab"))
(package! git-auto-commit-mode)
(package! lexic)
(package! magit-annex)
(package! embark-vc)
(package! gitignore-templates)
(package! annexview :recipe (:host nil :repo "https://git.kyleam.com/annexview"))
(package! disk-usage)
(package! daemons)
(package! ztree)
(package! tldr)
(package! 0x0)
(package! osm)
(package! ement :recipe (:host github :repo "alphapapa/ement.el")) ; matrix
(when (member (system-name) my/personal-hosts)
  (package! anki-editor)
  (package! anki
    :recipe (:host github :repo "chenyanming/anki.el")
    :disable t))
(when (featurep! :app rss)
  (package! elfeed-tube)
  (package! elfeed-tube-mpv)
  (package! mpv))
(package! elpher)
(package! form-feed)
;; pretty hacky
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :disable t)
(package! edwina :disable t)
