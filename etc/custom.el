(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(auth-source-save-behavior nil)
 '(counsel-mode t)
 '(custom-safe-themes
   '("8776539c07241929171541d237c7d892b5e45db5d781095a912c767f97f08770" default))
 '(fci-rule-color "#333333")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-display-style 'fancy)
 '(ivy-re-builders-alist
   '((read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy--regex-plus)) t)
 '(ivy-use-virtual-buffers t)
 '(key-chord-two-key-delay 0.2 t)
 '(org-archive-location ".archive_%s::")
 '(org-attach-directory ".attach/" t)
 '(org-attach-id-dir ".attach/" t)
 '(org-attach-method 'lns t)
 '(org-crypt-disable-auto-save t t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii html icalendar latex md))
 '(org-export-default-language "fr" t)
 '(org-export-with-tags nil t)
 '(org-export-with-todo-keywords nil t)
 '(org-footnote-auto-adjust t)
 '(org-footnote-define-inline nil)
 '(org-footnote-fill-after-inline-note-extraction t)
 '(org-footnote-section nil)
 '(org-hide-block-startup t)
 '(org-image-actual-width '(300))
 '(org-indirect-buffer-display 'current-window)
 '(org-link-abbrev-alist
   '(("gsc" . "https://scholar.google.com/scholar?q=%h")
     ("sep" . "https://plato.stanford.edu/search/search?query=%h")
     ("etym" . "http://www.cnrtl.fr/etymologie/%h")
     ("bu" . "http://hola.univ-lyon1.fr/ipac20/ipac.jsp?menu=search&aspect=basic_search&npp=10&ipp=20&spp=20&profile=scd&ri=&index=.GK&term=%h&terms=&valider=Ok")))
 '(org-src-preserve-indentation t)
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(org-tags-column 20)
 '(org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
 '(org-use-speed-commands
   '(closure
     (t)
     nil
     (and
      (looking-at org-outline-regexp)
      (looking-back "^**"
                    (1-
                     (point))))))
 '(outshine-use-speed-commands t)
 '(package-selected-packages '(key-seq key-chord))
 '(which-key-idle-delay 0.3)
 '(which-key-min-display-lines 7)
 '(which-key-popup-type 'side-window)
 '(which-key-side-window-max-height 0.3)
 '(which-key-side-window-max-width 0.5)
 '(which-key-sort-order 'which-key-key-order))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
