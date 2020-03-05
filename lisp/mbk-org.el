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

;;; Code:

(use-package org
  :commands (org-link-make-string
             org-next-visible-heading
             org-previous-visible-heading
             org-display-inline-images
             org-remove-inline-images
             org-toggle-inline-images
             org-get-buffer-tags)
  :functions (org-read-date)
  :mode (("\\.org\\'" . org-mode)
         ("README\\'"   . org-mode))
  :bind* (("C-c C-w" . org-refile)
          ("C-c C-l" . org-store-link)
          :map org-mode-map
          ("s-e"     . org-babel-tangle-all-block-same-file)
          ("s-l"     . org-latex-export-to-latex)
          ("C-c ."   . org-time-stamp)
          ("C-c M-i" . org-insert-link)
          ("C-c m"   . hydra-org-image/body)
          ("C-c $"   . hydra-org-archive/body)
          ("C-c e d" . org-decrypt-entry)
          ("C-c e e" . org-encrypt-entry)
          ("C-c e s" . org-sparse-tree)
          ("C-c e t" . org-tags-sparse-tree))
  :custom
  (org-archive-location ".archive_%s::")
  ;; activate speed commands when on any outline star
  (org-use-speed-commands
   (lambda () (and (looking-at org-outline-regexp)
              (looking-back "^\**" (1- (point))))))
  (org-indirect-buffer-display 'current-window)
  ;; make symlink instead of hard copy
  (org-attach-method 'lns)
  ;; ;; delete attachment when archiving entry
  ;; (org-attach-archive-delete t)
  ;; change folder from data/ to .attach/
  (org-attach-directory ".attach/")
  (org-tags-column 20)
  (org-hide-block-startup t)


  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-enforce-todo-dependencies t)
  (org-link-abbrev-alist
   '(("gsc"  . "https://scholar.google.com/scholar?q=%h")
     ("sep"  . "https://plato.stanford.edu/search/search?query=%h")
     ("etym" . "http://www.cnrtl.fr/etymologie/%h")
     ("bu"   . "http://hola.univ-lyon1.fr/ipac20/ipac.jsp?menu=search&aspect=basic_search&npp=10&ipp=20&spp=20&profile=scd&ri=&index=.GK&term=%h&terms=&valider=Ok")))
  (org-crypt-disable-auto-save t)
  (org-src-preserve-indentation t)
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline nil)
  (org-footnote-fill-after-inline-note-extraction t)
  (org-footnote-section nil)
  (org-export-with-todo-keywords nil)
  (org-export-default-language "fr")
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-with-tags nil)
  (org-startup-with-inline-images t)
  (org-startup-indented t)
  (org-image-actual-width '(300))
  :config
  ;; (require 'org-datetree)
  (setq org-modules '(org-crypt))

  )

(provide 'mbk-org)
