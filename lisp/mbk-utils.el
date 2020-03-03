;;; mbk-utils.el --- personal macros and functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto
;; Copyright (C) 2020  Milind Kamble

;; Author: Milind Kamble <milindbkamble@gmail.com>
;; Original Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: functions, elisp

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

(defun mbk--group (source n)
  "Divide SOURCE list in N groups and stack together the last
elements.
"
  (if (zerop n) (error "Zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro mbk-defaliases (&rest alias)
  (declare (indent 0))
  `(progn
     ,@(mapcar
        (lambda (pair)
          `(defalias ,@pair))
        (mbk--group alias 2))))

(defmacro mbk--hash (var docstring &rest kv-pairs)
  "Create a new hash table named VAR documented by DOCSTRING that
maps key to value for each key-value pair in KV-PAIRS."
  (declare (indent defun) (doc-string 2))
  (let ((s (/ (length kv-pairs) 2)))
    `(progn
       (defvar ,var (make-hash-table :size ,s :test 'equal) ,docstring)
       (mbk-with-args2 pair
         (puthash (car pair) (cadr pair) ,var)
         ,@kv-pairs))))

(defun mbk-add-to-list (lst &rest args)
  (mapcar
   (lambda (el) (add-to-list lst el))
   args))

(defmacro mbk-with-args2 (name body &rest args)
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (g)
          `(cl-symbol-macrolet ((,name ',g))
             ,body))
        (mbk--group args 2))))

(defmacro mbk-set-custom (&rest args)
  (declare (indent 0))
  `(progn
     ,@(mapcar
        (lambda (pair) `(custom-set-variables '(,@pair)))
        (mbk--group args 2))))

(defun mbk-side-buffer (buffer &optional params)
  "Display BUFFER in a side window with parameters PARAMS."
  (declare (indent 2))
  (display-buffer-in-side-window
   buffer params)
  (select-window (get-buffer-window buffer)))

(defmacro define-after-save-hook-mode (name fun &optional lighter docstring)
  "Define a minor mode named NAME-after-save-mode that will run
FUN each time buffer is saved.

Minor mode has lighter LIGHTER and is documented by DOCSTRING."
    (declare (doc-string 4))
    (let ((toggler (intern (format "%s--toggle" name)))
          (mmode   (intern (format "%s-after-save-mode" name))))
      `(progn
         (defun ,toggler (toggle)
           (pcase toggle
             (:on  (add-hook    'after-save-hook ,fun nil t))
             (:off (remove-hook 'after-save-hook ,fun t    ))))

         (define-minor-mode ,mmode
           ,docstring
           nil ,lighter nil
           (if ,mmode (,toggler :on) (,toggler :off))))))

(defun mbk-append-string-to-file (string file)
  "Add STRING to end of FILE"
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (insert string)
    (save-buffer)
    (kill-current-buffer)))

(defun mbk--read-table (file)
  "Return contents of FILE as list of string, one element per
line."
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
        (split-string
         (buffer-substring-no-properties (point-min) (point-max)))
      (kill-current-buffer))))

(defsubst mbk-concat (seq sep)
  "Concatenate sequence SEQ using SEP as separator"
  (mapconcat #'identity seq sep))

(defsubst add-to-hook (hook &rest funs)
  "Add FUNS to HOOK in a single run."
  (declare (indent 1))
  (mapcar (lambda (f) (add-hook hook f)) funs))

(provide 'mbk-utils)