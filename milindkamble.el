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

(use-package mbk-defaults
  :load-path "lisp"
  :hook (after-init . mbk-initialize!))

(use-package no-littering
  :commands (no-littering-expand-var-file-name
             no-littering-expand-etc-file-name)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package objed
  :defer t)

(use-package blackout
  :demand t)

(use-package ivy
  :blackout t
  :commands (ivy-mode)
  :bind* (("s-t" . ivy-switch-buffer)
          ("s-<backspace>" . ivy-switch-buffer)
          :map ivy-mode-map
          ("C-'" . ivy-avy))
  :custom
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode))

(use-package mbk  ;; load lisp/mbk.el
  :load-path "lisp")
