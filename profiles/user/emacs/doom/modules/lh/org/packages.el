;; -*- no-byte-compile: t; -*-
(package! org-super-agenda)
(package! doct)
(package! org-caldav)
;; TODO: references .emacs.d instead of variable
(package! org-vcard :disable t)
(package! org-chef)
(when (featurep! :lang org +present)
  (package! revealjs-plugins-rajgoel :recipe (:host github :repo "rajgoel/reveal.js-plugins" :files ("chalkboard"))))
;; convenient org and roam extensions by d12frosted, can probably be removed in
;; favor of my own implementations in future, since I don't use much
(package! vulpea)
