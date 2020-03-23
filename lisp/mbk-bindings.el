(defvar mbk-keymap (make-sparse-keymap))
;;(make-local-variable 'wk-local-map-ctrl-c-prefix)

(general-define-key
 :prefix-command 'mbk-keymap
 :wk-full-keys nil
 ;; pending #'redo #'undo based on undo-tree
 ;; use M-x align-regex " \('\|#\) to align
 "SPC"	 '(counsel-M-x :wk "M-x")
 "a"	 '(:prefix-command mbk-appmap :wk "apps")
 "ac"	 '(calc :wk "calc")
 "ad"	 '(counsel-dired :wk "dired")
 "b"	 '(:prefix-command mbk-buffmap :wk "bufferr")
 "br"	 '(revert-buffer :wk "revert")
 "bs"	 '(counsel-switch-buffer :wk "swtich")
 "bv"	 '(counsel-find-file :wk "visit")
 "c"	 '(:prefix-command mbk-cmap :wk "ctrl-c") ;; find a better name
 "cc"     (general-simulate-key "C-C" :which-key "ctrl-c") ;; access the existing major-mode bindings
 "g"	 '(:prefix-command mbk-magitmap :wk "magit")
 "gs"	 '(magit-status :wk "status")
 "o"	 '(:prefix-command mbk-magitmap :wk "oline/org")
 "oa"    '(outline-show-all :wk "ol-show-all")
 "ot"    '(outline-hide-body :wk "ol-hide-body")
 "op"    '(outline-previous-visible-heading :wk "prv-vis")
 "on"    '(outline-next-visible-heading :wk "nxt-vis")
 "ou"    '(outline-up-heading :wk "up-head")
 "ob"    '(outline-backward-same-level :wk "bwd-sib")
 "of"    '(outline-forward-same-level :wk "fwd-sib")
 "t"	 '(:prefix-command mbk-toggles :wk "toggle")
 "tc"    '(counsel-mode :wk "counsel-mode")
 "tw"    '(which-key-mode :wk "which-key")
 "tf"    '(fci-mode :wk "fill-column-indicator")
 "tz"    '(text-scale-adjust :wk "zoom font")
  ) ;; end
(general-define-key :keymaps 'override
                    (general-chord "hu") #'mbk-keymap)
(general-define-key :keymaps 'override
                    (general-chord "uh") #'mbk-keymap)
;;(add-hook 'which-key-initb-buffer-hook (lambda () (message "adding local-map C-c") (define-key 'mbk-cmap (kbd "c") (cadr (lookup-key (current-local-map) (kbd "C-c")))) ))
;;(setq mbk-local-map-ctrl-c (cadr (lookup-key (current-local-map) (kbd "C-c"))))
;;(general-emacs-define-key mbk-cmap "c" 'mbk-local-map-ctrl-c)
(provide 'mbk-bindings)
