;;; mbkamble.el --- personal configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Milind Kamble
;; Copyright (C) 2019  Samuel Barreto

;; Author: Milind Kamble <milindbkamble@gmail.com>
;; Original Author: Samuel Barreto <samuel.barreto8@gmail.com>
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


;;;; personal utility funcs
(use-package mbk-utils :load-path "lisp")

;;;; personal defaults
(use-package mbk-defaults
  :load-path "lisp"
  :hook (after-init . mbk-initialize!))

(use-package mbk  ;; load lisp/mbk.el
  :load-path "lisp")

;;;; essential utils
(use-package f :defer t)   ;; file manipulation
(use-package s)   ;; string manipulation
(use-package ht :defer t)  ;; hash table manipulation
(use-package ts :defer t)  ;; time manipulation
(use-package seq)          ;; sequence utils
(use-package parson)       ;; json parsing utility

(require 'general)         ;; generic and powerful keybinding

;;;; essential utils
(use-package key-chord     ;; my leader key uses key-chord pair
  :commands (key-chord-mode
             key-chord-define-global)
  :custom
  (key-chord-two-key-delay 0.2)
  :init
  (key-chord-mode 1)
)

(use-package no-littering
  :commands (no-littering-expand-var-file-name
             no-littering-expand-etc-file-name)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package point-stack
  :config
  (point-stack-setup-advices))

'(use-package objed
  :defer t)

(use-package blackout
  :demand t)

;; from radian.el
;; (defun mbk--do-auto-fill ()
;;   "Replacement for `do-auto-fill' that respects `normal-auto-fill-function'.
;; The reason we need this is that in order to enable auto-fill
;; globally, we are supposed to set the default value of variable
;; `auto-fill-function'. However, some major modes set
;; `normal-auto-fill-function' (itself normally set to
;; `do-auto-fill', which is what we generally set the default value
;; of variable `auto-fill-function' to), expecting `auto-fill-mode'
;; to be enabled afterwards (which copies the value of
;; `normal-auto-fill-function' into variable `auto-fill-function').
;; However, since we enable auto-fill globally by means of setting
;; variable `auto-fill-function' directly, this setting gets lost.
;; The workaround is to set variable `auto-fill-function' globally
;; to a function which looks up the value of
;; `normal-auto-fill-function' \(generally just `do-auto-fill') and
;; calls that. This is a slight inversion of the usual flow of
;; control and might make you slightly uncomfortable, but we'll just
;; have to live with it :3"
;;   (funcall normal-auto-fill-function))

;; ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
;; (setq-default auto-fill-function #'mbk--do-auto-fill)

(use-package ivy
  :blackout t
  :commands (ivy-mode)
  :bind* (:map ivy-mode-map ("C-'" . ivy-avy))
  :custom
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy) ;; automatic .* between each character
        (t . ivy--regex-plus))) ;; Spaces are wildcards, '!' negates match
  :config
  (ivy-mode))

;; based on author's reco. https://oremacs.com/2019/04/07/swiper-isearch/
;; Once search is begun, C-s and C-r moves fwd/bwd on matching lines (like isearch)
(use-package swiper
  :after ivy
  :bind* (:map swiper-isearch-map
          ("C-s" . #'ivy-next-line)
          ("C-r" . #'ivy-previous-line)
          ("C-t" . #'ivy-yank-word)
          :map global-map
          ("C-s" . #'swiper-isearch))
  ;; thought of using hl-line face for swiper-line-face, but that is
  ;; used only when hl-line-mode is activated. Don't fancy
  ;; hl-line-mode at this time. So changing highlight face color in
  ;; theme itself
  ;; :custom-face
  ;; (swiper-line-face    ((t (:inherit hl-line))))
  )

(use-package counsel
  :commands (counsel-load-theme
             counsel-bookmark
             counsel-yank-pop)
  :bind* (("C-x b" . counsel-switch-buffer)
          ("C-x C-r" . counsel-buffer-or-recentf)
          ("C-c C-/" . counsel-rg)
          ("M-x" . counsel-M-x))
  :config
  (setq counsel-find-file-ignore-regexp
        dired-garbage-files-regexp)
  (setq counsel-find-file-at-point t)

  (defun counsel-font-headings ()
    "Change font of variable pitch."
    (interactive)
    (ivy-read
     "Chose font :"
     (font-family-list)
     :caller 'counsel-font-headings
     :action
      (lambda (x)
        (set-face-attribute 'variable-pitch nil
                            :family x :height 120))))

  (defun counsel-font ()
    "Change font of current frame"
    (interactive)
    (ivy-read "Chose font :"
              (font-family-list)
              :caller 'counsel-font
              :action (lambda (x) (set-frame-font x)))))

(use-package which-key
    ;; :defer 1
    :blackout ;; prevent mode display in mode-line
    :commands (which-key-mode
               which-key-setup-side-window-right-bottom
               which-key-add-key-based-replacements)
    :custom
    ;; simple then alphabetic order.
    (which-key-sort-order 'which-key-key-order)
    (which-key-popup-type 'side-window)
    (which-key-side-window-max-height 0.3)
    (which-key-side-window-max-width 0.5)
    ;; will tweak towards higher values as mbk-bindings get memorized
    (which-key-idle-delay 0.6)
    (which-key-min-display-lines 7)
    :init
    (setq which-key-enable-extended-define-key t)
    :config
    (which-key-mode +1)
    (which-key-setup-side-window-right-bottom)
    ;; key description for C-x
    (which-key-add-key-based-replacements
      "C-x RET" "coding system -input"
      "C-x 4"   "Other Window"
      "C-x 5"   "Frame"
      "C-x 6"   "2C"
      "C-x @"   "event"
      "C-x 8"   "special char"
      "C-x a"   "abbrev"
      "C-x n"   "narrow"
      "C-x r"   "rectangle"
      "C-x v"   "version control"
      "C-c &"   "yas"
      "C-c @"   "hide-show"
      "M-SPC h" "info"
      "M-SPC g" "grep"
      "M-SPC M-s" "occur"))

(use-package hydra
  :demand t)
(use-package hercules
  :after hydra
  :demand t
  :commands (hercules-def))

;;;; outshine
(use-package outshine
  :defer t
  :init
  (defvar outline-minor-mode-prefix "\M-#")
  ;; need to use cons form bcos hook name is
  ;; outshine-mode-hook, but pkg name is outshine
  :hook (emacs-lisp-mode . outshine-mode)
)

;;;; fill-column-indicator aka fci
(use-package fill-column-indicator
  :config
  (define-globalized-minor-mode global-fci-mode
    fci-mode (lambda () (fci-mode 1)))
  )

;; virtual-auto-fill
(use-package virtual-auto-fill
  :hook
  ((text-mode org-mode markdown-mode) . virtual-auto-fill-mode))

;(use-package speed-type)
