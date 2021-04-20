(defvar mbk-keymap (make-sparse-keymap))
;;(make-local-variable 'wk-local-map-ctrl-c-prefix)

(general-define-key
 :prefix-command 'mbk-keymap
 :wk-full-keys nil
 ;; pending #'redo #'undo based on undo-tree
 ;; use M-x align-regex " \('\|#\) to align
 "SPC"	 '(:prefix-command mbk-meta :wk "meta prefix")
 "SPC x"	 '(counsel-M-x :wk "M-x")
 "SPC a"  (general-simulate-key "M-a" :which-key "M-a")
 "SPC e"  (general-simulate-key "M-e" :which-key "M-e")
 "SPC f"  (general-simulate-key "M-f" :which-key "M-f")
 "SPC b"  (general-simulate-key "M-b" :which-key "M-b")
 "SPC F"  (general-simulate-key "M-F" :which-key "M-F")
 "SPC B"  (general-simulate-key "M-B" :which-key "M-B")
 "SPC ,"  '(point-stack-pop :wk "point-stack fwd")
 "SPC ."  '(point-stack-forward-stack-pop :wk "point-stack bwd")
 "a"	 '(:prefix-command mbk-appmap :wk "apps")
 "ac"	 '(calc :wk "calc")
 "ad"	 '(counsel-dired :wk "dired")
 "ae"	 '(eshell :wk "eshell")
 "ak"	 '(unlock-keepass :wk "unlock keepass")
 "b"	 '(:prefix-command mbk-buffmap :wk "bufferr")
 "br"	 '(revert-buffer :wk "revert")
 "bk"	 '(kill-buffer :wk "kill")
 "bs"	 '(swiper-isearch :wk "search")
 "bw"	 '(counsel-switch-buffer :wk "switch")
 "bo"	 '(counsel-switch-buffer-other-window :wk "switch in other")
 "bv"	 '(counsel-find-file :wk "visit")
 "c"	 '(:prefix-command mbk-cmap :wk "ctrl-c") ;; find a better name
 "cc"     (general-simulate-key "C-C" :which-key "ctrl-c") ;; access the existing major-mode bindings
 "g"	 '(:prefix-command mbk-magitmap :wk "magit")
 "gs"	 '(magit-status :wk "status")
 "f"	 '(:prefix-command mbk-fmap :wk "files")
 "ff"    '(counsel-find-file :wk "find file")
 "fr"    '(counsel-buffer-or-recentf :wk "recent file")
 "o"	 '(:prefix-command mbk-ol-orgmap :wk "outline/org")
 "op"    '(outline-previous-visible-heading :wk "prv-vis")
 "on"    '(outline-next-visible-heading :wk "nxt-vis")
 "ou"    '(outline-up-heading :wk "up-head")
 "ob"    '(outline-backward-same-level :wk "bwd-sib")
 "of"    '(outline-forward-same-level :wk "fwd-sib")
 "t"	 '(:prefix-command mbk-toggles :wk "toggle")
 "ta"    '(auto-fill-mode :wk "auto-fill")
 "tc"    '(counsel-mode :wk "counsel-mode")
 "th"    '(hl-line-mode :wk "highlight line")
 "tw"    '(which-key-mode :wk "which-key")
 "tf"    '(fci-mode :wk "fill-column-indicator")
 "tz"    '(text-scale-adjust :wk "zoom font")
  ) ;; end
(general-define-key :keymaps 'override
                    (general-chord "hh") #'mbk-keymap)
(general-define-key :keymaps 'override
                    (general-chord "uu") #'mbk-keymap)
(general-emacs-define-key ctl-x-map "o" #'counsel-switch-buffer-other-window)

;; C-x-r-h and C-x-r-v are free. Use it to bind to
;;calc-grab-sum-across and down respectively
(general-define-key
 :keymaps 'ctl-x-r-map
 "h" 'calc-grab-sum-across
 "v" 'calc-grab-sum-down)

;;(add-hook 'which-key-initb-buffer-hook (lambda () (message "adding local-map C-c") (define-key 'mbk-cmap (kbd "c") (cadr (lookup-key (current-local-map) (kbd "C-c")))) ))
;;(setq mbk-local-map-ctrl-c (cadr (lookup-key (current-local-map) (kbd "C-c"))))
;;(general-emacs-define-key mbk-cmap "c" 'mbk-local-map-ctrl-c)
(provide 'mbk-bindings)

;;;; Developer notes
;;;;; First level category of bindings
(setq foo "
a : apps (calc, dired, eshell)
m : movement (cursor motion)
b : buffer (revert, save, split, search)
w : window ops (split, close, rotate)

")
