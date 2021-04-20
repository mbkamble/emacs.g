;; from https://stackoverflow.com/questions/605785/how-do-i-get-a-list-of-emacs-lisp-non-interactive-functions
;; how to get list of emacs lisp non-interactive functions
(cl-flet ((first-line (text)
         (if text
             (substring text 0 (string-match "\n" text))
           "")))
  (mapatoms
   (lambda (x)
     (and (fboundp x)                          ; does x name a function?
          (not (commandp (symbol-function x))) ; is it non-interactive?
          (subrp (symbol-function x))          ; is it built-in?
          (insert (symbol-name x) " - " (first-line (documentation x)) "\n")))))

;; get list of all interactive functions (aka commands)
(setq myfoo-commands nil)
(mapatoms
 (lambda (x)
   (and (fboundp x)                          ; does x name a function?
	(commandp (symbol-function x)) ; is it interactive?
	(not  (subrp (symbol-function x)))          ; not built-in?
	(add-to-list 'myfoo-commands x))))
;; butlast was used to initially select only a few first elements from myfoo-commands.
;; to mapcar over entire list just directly use myfoo-commands
(--filter (cdr it) (mapcar (lambda (x) (cons x (mapcar 'key-description (where-is-internal x overriding-local-map)))) myfoo-commands)) ;;(butlast myfoo 0)

(setq myfoo-varsnamed-star-map nil)
(mapatoms
 (lambda (x)
   (and (not (fboundp x))
	(string-suffix-p "-map" (symbol-name x))
	(add-to-list 'myfoo-varsnamed-star-map x))))

(setq myfoo-varsnamed-star-binding nil)
(mapatoms
 (lambda (x)
   (and (not (fboundp x))
	(or (string-suffix-p "-binding" (symbol-name x))
	    (string-suffix-p "-bindings" (symbol-name x)))
	(add-to-list 'myfoo-varsnamed-star-binding x))))

(where-is-internal 'write-file overriding-local-map) ;; technique to find full key sequence for a target "command"
(key-description '[3 22 14]) ;; prints human-friendly key sequence

;;;;;;;;;;;;;;Toggle or indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacsorphanage/terraform-mode/issues/20
;; Allow simple visibility toggling in outline mode with TAB
;;

(defun my-outline-toggle-or-indent (&optional ARG)
  (interactive)
  (if (outline-on-heading-p)
	  (outline-toggle-children)
	(indent-for-tab-command ARG)
	)
  )

(define-key outline-minor-mode-map "\C-i" 'my-outline-toggle-or-indent)
;;;;;;;;;;;;;;Toggle or indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
