;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! org-roam)
(package! org-roam)
(package! org-roam-ui)
(package! org-roam-timestamps)
(package! org-transclusion)
;; (package! org-fragtog
;;  :recipe (:host github :repo "io12/org-fragtog"))
(package! xenops)
(package! org-appear)
(package! rime)
;;(package! helm-bibtex)
;;(package! org-roam-bibtex)
(package! lsp-grammarly)
(package! valign)
(package! vulpea)
(package! keyfreq
  :recipe (:host github :repo "dacap/keyfreq"
           :files ("*.el")))
(package! minimap)
