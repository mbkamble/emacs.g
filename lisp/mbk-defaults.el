;;; mbkamble.el --- personal configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Milind Kamble

;; Author: Milind Kamble <milindbkamble@gmail.com>
;; Keywords: config

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Custom

(defgroup mbk nil
  "Customization for my personnal variables and functions."
  :group 'convenience
  :version 1.0
  :prefix "mbk-")

(defcustom mbk-font "Victor Mono SemiBold 12"
  "Font for coding situations."
  :group 'mbk
  :type 'string)

(defcustom mbk-variable-pitch-font "Victor Mono SemiBold 12"
  "Font for text"
  :group 'mbk
  :type 'string)

(defcustom mbk-theme 'leuven
  "Default theme for my config"
  :group 'mbk
  :type 'theme)

(defcustom mbk-use-variable-pitch-font t
  "Whether to use a variable pitch font for non-coding situations or not.

Defaults to t."
  :group 'mbk
  :type 'boolean)

;;;; Macros

;;;###autoload
(defmacro add-hook! (hook &rest body)
  "Nicer add-hooking that prevents writing lambdas explicitely.

Add a lamdba containing BODY to hook HOOK."
  (declare (indent 1))
  `(add-hook ,hook
             (lambda () (progn ,@body))))

;;;; Default adjustment

(defun mbk--initialize-frame! ()
  "Set the default dimension and position of a new frame."
  (let* ((a-width (* (display-pixel-width) 0.50))
         (a-height (* (display-pixel-height) 0.60))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))

    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame)
                    (truncate a-width)
                    (truncate a-height)
                    t )))

(defun mbk--windows! ()
  "Set the default behavior for windows and splitting buffers."
  (defvar my-window--parameters
    '(window-parameters . ((no-other-window . t)
                           (no-delete-other-windows . t))))

  (setq fit-window-to-buffer-horizontally t)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)

  (setq display-buffer-alist
        `(("\\*\\(?:Buffer List\\|Bookmark List\\|Bookmark Annotation\\)\\*"
           display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . fit-window-to-buffer)
           (preserve-size . (nil . t)) ,my-window--parameters)
          ("\\*Buffer List\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . 10)
           (preserve-size . (nil . t)) ,my-window--parameters)
          ("\\*ibuffer\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . 10)
           (preserve-size . (nil . t)) ,my-window--parameters)
          ("\\*Tags List\\*" display-buffer-in-side-window
           (side . right)
           (slot . 1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,my-window--parameters)
          ("\\*toc\\*" display-buffer-in-side-window
           (side . left)
           (slot . 2)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,my-window--parameters)
          ("\\*\\(?:help\\|grep\\|Completions\\|undo-tree\\)\\*"
           display-buffer-in-side-window
           (side . left)
           (window-width . fit-window-to-buffer)
           (slot . -1)
           (preserve-size . (t . nil)) ,my-window--parameters)
          ("\\*\\(?:shell\\|Async Shell Command\\)\\*" display-buffer-in-side-window
           (side . top)
           (slot . 1)
           (preserve-size . (nil . t)) ,my-window--parameters)
          ("\\*\\(?:compilation\\|interpretation\\)\\*" display-buffer-in-side-window
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t)) ,my-window--parameters)
          ("\\*Org Select\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,my-window--parameters)
          ("\\*Agenda Commands\\*" display-buffer-in-side-window
           (side . left)
           (slot . -1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,my-window--parameters)
          ("\\*ivy\\*" display-buffer-in-side-window
           (side . left)
           (slot . 1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,my-window--parameters)
          ("\\*ivy-occur .*\\*" display-buffer-in-side-window
           (side . top)
           (slot . 2)
           (window-height . 10)
           (preserve-size . (nil . t)) ,my-window--parameters))))

(defun mbk--appearances! ()
;;   "Set the default theme and fonts"
;;   (unless (eq sam-theme 'default)
;;     (load-theme sam-theme t))


  (when mbk-use-variable-pitch-font
    (set-face-attribute
     'variable-pitch
     nil
     :family mbk-variable-pitch-font
     :height 120)
    (remove-hook 'text-mode-hook
      (variable-pitch-mode 1)))

  (when window-system
    ;; increase space between lines
    (setq-default line-spacing 0)

    ;; change default font for current frame
    (add-to-list 'default-frame-alist `(font . ,mbk-font))
    (add-to-list 'default-frame-alist `(:height . 120))
    (set-face-attribute 'default nil :font mbk-font :height 120)))

(defun mbk--defaults! ()
  ;; disable warnings when opening file or directory with
  ;; local variables set
  (setq-default enable-local-eval t)
  (setq-default enable-local-variables :safe)
  (setq-default enable-dir-local-variables t)
  (setq
   version-control t                      ; enable version control
   vc-make-backup-files t                 ; backups file even when under vc
   vc-follow-symlinks t                   ; follow symlinks
   ring-bell-function 'ignore             ; mute the bell
   default-fill-column 72
   initial-scratch-message ""
   select-enable-clipboard t              ; Merge system's and Emacs' clipboard
   help-window-select t                   ; focus help window when opened
   ;; tab-width does not influence indentation step
   tab-width 4                            ; num spaces for tab char display
   auto-window-vscroll nil                ; Lighten vertical scroll
   confirm-kill-emacs 'yes-or-no-p        ; Confirm before exiting Emacs
   cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
   display-time-format "%H:%M"            ; Format the time string
   sentence-end-double-space nil          ; End a sentence after a dot and a space

   text-scale-mode-step 1.05              ; finer grain of text increasing
   epa-armor t
   epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
   epa-pinentry-mode 'loopback            ; use minibuffer reading passphrase
   )

  (epa-file-name-regexp-update)

  (setq-default indent-tabs-mode nil
		        tab-width 4)



  (prefer-coding-system 'utf-8)           ; utf-8 is default coding system

  (defalias 'yes-or-no-p 'y-or-n-p)       ; replace yes-no with y-n
  (defalias 'kill-frame #'delete-frame)
  (defalias 'kill-other-frames #'delete-other-frames)
  (defalias 'kill-window #'delete-window)
  (defalias 'kill-other-windows #'delete-other-windows)

  (save-place-mode)                       ; remember location in files
  (delete-selection-mode 1)               ; replace highlighted text with type
  (add-hook 'before-save-hook
            'delete-trailing-whitespace)  ; self explanatory

  (add-hook! 'after-save-hook
    (executable-make-buffer-file-executable-if-script-p))
  )

;;;###autoload
(defun mbk-initialize! ()
  (interactive)
  (mbk--initialize-frame!)
  (mbk--windows!)
  (mbk--appearances!)
  (mbk--defaults!)
  )

;; reduce text size in help side-window
(add-hook! 'help-mode-hook
  (text-scale-set -2))

(provide 'mbk-defaults)
