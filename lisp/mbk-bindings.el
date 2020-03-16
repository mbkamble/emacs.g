(defvar mbk-keymap (make-sparse-keymap))

(general-define-key
 :prefix-command 'mbk-keymap
 :wk-full-keys nil
 ;; pending #'redo #'undo based on undo-tree
 ;; use M-x align-regex " \('\|#\) to align
 "SPC"	 '(counsel-M-x :wk "M-x")
 "g"	 '(:prefix-command mbk-magitmap :wk "magit")
 "gs"	 #'magit-status
 "o"	 '(:prefix-command mbk-magitmap :wk "oline/org")
 "oa"    '(outline-show-all :wk "ol-show-all")
 "ot"    '(outline-hide-body :wk "ol-hide-body")
 "op"    '(outline-previous-visible-heading :wk "prv-vis")
 "on"    '(outline-next-visible-heading :wk "nxt-vis")
 "ou"    '(outline-up-heading :wk "up-head")
 "ob"    '(outline-backward-same-level :wk "bwd-sib")
 "of"    '(outline-forward-same-level :wk "fwd-sib")
 "t"	 '(:prefix-command mbk-toggles :wk "toggle")
 "tw"    '(which-key-mode :wk "which-key")
  )
(general-define-key :keymaps 'override (general-chord "hu") #'mbk-keymap)
(general-define-key :keymaps 'override (general-chord "uh") #'mbk-keymap)

(provide 'mbk-bindings)
