;; -*- no-byte-compile: t; -*-
(package! org-edna)
(package! org-super-agenda)
(package! doct)
(package! org-caldav)
(package! org-chef)
(when (featurep! :lang org +present)
  (package! revealjs-plugins-rajgoel :recipe (:host github :repo "rajgoel/reveal.js-plugins" :files ("chalkboard"))))
(package! org-roam-timestamps)
;; convenient org and roam extensions by d12frosted, can probably be removed in
;; favor of my own implementations in future, since I don't use much
(package! vulpea)
(package! org-auctex :recipe (:host github :repo "karthink/org-auctex"))
