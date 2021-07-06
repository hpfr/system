;; -*- no-byte-compile: t; -*-
(package! emacs-conflict :recipe
  (:host github :repo "ibizaman/emacs-conflicts"))
(package! vimrc-mode)
(package! disk-usage)
(when (package! ement
        :recipe (:host github :repo "alphapapa/ement.el")
        :disable t)
  (package! plz
    :recipe (:host github :repo "alphapapa/plz.el")))
(package! telega
  :recipe (:branch "releases")
  :disable (not (member system-name personal-hosts)))
(package! elpher)
(package! yequake
  :disable (not (member system-name gui-capable-hosts)))
(package! pdf-continuous-scroll-mode
  :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
  :disable (not (member system-name gui-capable-hosts)))
(package! edwina)
