;; -*- no-byte-compile: t; -*-
(package! org-edna)
(package! org-transclusion)
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
(package! consult-org-roam :recipe (:host github :repo "jgru/consult-org-roam"))
(when (package! org-roam-ui :recipe
        (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
  (package! websocket))
(package! org-table-color)
