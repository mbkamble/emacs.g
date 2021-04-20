;;; dump-keys.el -- Output emacs keybindings in a format suitable for grepping
;;; Maintained by Tal Wrii
;;; Copyright Tal Wrii 2017
;;; Copyright Justin Burkett 2015 (redistributed under GPLv3)
;;; source: https://emacs.stackexchange.com/questions/36576/dump-list-of-current-keybindings-to-standard-out
;;; This is highly influenced by Justin Burkett's which-keys.el
;;; and used its code as starting point
;;; Justin Burkett therefore  has copyright over some of this code (as does Tal Wrii)

;;; Tal Wrii is of course responsible for everything bad about this code
;;; and should be contacted if there are any bugs.
;;; Justin Burkett is responsible for anything good and should be
;;; admired from afar.

;; The main purpose of this script is to get keybindings out of
;; emacs in a format suitable for machine processing from the commandline
;; e.g. 
;;     emacs-keys | grep balh | grep blah e


;; A script to doing this is included below

;; TODO: remove bugs, csv output ?, json output?, include mode info?, deal with shadowing?, sorting?

;; #!/bin/bash
;; set -o errexit
;; set -o nounset
;; set -o pipefail

;; # Write a list of emacs keys (suitable for grepping)

;; filename=$(mktemp)

;; emacsclient -eval "(with-current-buffer (window-buffer (selected-window)) (write-region (format \"%S\" (dump-keys-dump)) nil \"$filename\" ))" >/dev/null

;; cat $filename | parsemacs.py


(defun dump-keys--map-keymap (func keymap &optional filter)
  (map-keymap
   (lambda (ev def)
     (when (filter ev def)
       (func ev def)))
   keymap))

(defun dump-keys--log (&rest args)
  (if dump-keys-log
  (apply 'message args)))

(defun dump-keys-toggle-log ()
  (interactive)
  (setq  dump-keys-log (not dump-keys-log )))

(defvar dump-keys-log nil "Print debug output to messages")

(defun dump-keys--get-keymap-bindings (keymap &optional prefix)
  (dump-keys--log "Get bindings for: %S" prefix)
  (apply 'append
         (dump-keys--get-simple-keys keymap prefix)
         (mapcar
          (lambda (arg)
            (-let
                (((new-prefix keymap) arg))
              (dump-keys--get-keymap-bindings keymap (if prefix (s-concat prefix " " new-prefix) new-prefix))))
          (dump-keys--get-prefixes keymap))))

(defun dump-keys--get-prefixes (keymap)
  (let (result)
    (map-keymap
     (lambda (ev def)
       (when (keymapp def)
         (setq result
           (cons
            (list
             (key-description (list ev))
             def) result))))
     keymap)
     result))

(defun dump-keys--get-info (prefix ev def)
  (dump-keys--log "Getting key info %S %S" prefix ev)
  (let ((key (key-description (list ev))))
  (cons (if prefix (s-concat prefix  " " key) key)
        (cond
         ((symbolp def) (copy-sequence (symbol-name def)))
         ((eq 'lambda (car-safe def)) "lambda")
         (t (format "%s" def))))))


(defun dump-keys--get-simple-keys (keymap &optional prefix)
  (let (bindings)

  (dump-keys--log "Getting simple keys for %S %S" keymap prefix)
      (map-keymap
       (lambda (ev def)
         (if (symbolp def)
             (setq def (copy-sequence (symbol-name def) )))
         (when (and def
                    (not (keymapp def))
                    (not (listp def))
                    (not (eq (car-safe def) 'menu-item)))
           (setq bindings
                 (cons
                  (dump-keys--get-info prefix ev def) bindings))))
       keymap)
      (dump-keys--log "Found: %S bindings" (length bindings))
      bindings))


(defun dump-keys--to-string (list)
  (s-join "\n"
     (mapcar (lambda (x) (s-join " " x)) list)))

(defun dump-keys--format-and-replace (unformatted)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let ((sep-w-face "->")
        (local-map (current-local-map))
        new-list)
    (dolist (key-binding unformatted)
      (let* ((key (car key-binding))
             (orig-desc (cdr key-binding))
             ;; At top-level prefix is nil
             )
        (when (consp key-binding)
          (push
           (list
                  (car key-binding)
                 sep-w-face
(cdr key-binding)
                 )
           new-list))))
    (nreverse new-list)))


(defun dump-keys-dump ()
  (dump-keys--to-string (dump-keys--format-and-replace (apply 'append (mapcar 'dump-keys--get-keymap-bindings (current-active-maps))))))


(provide 'dump-keys)
;;; dumpkeys ends here
