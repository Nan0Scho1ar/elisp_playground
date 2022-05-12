;;; word-count.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 12, 2022
;; Modified: May 12, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/word-count
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'cl-lib)
(require 'seq)

(defun word-count (str)
  "Count occurrences of each word in STR."
  (let* ((strip-chars "[;:'\" \n\t!.,@#$%^&]+")
         (stripped
          (mapcar
           (lambda (x) (downcase (string-trim x strip-chars strip-chars)))
           (split-string str)))
         (cleaned (seq-remove (lambda (x) (equal x "")) stripped)))
    (cl-loop for word in (seq-uniq cleaned) collect
             (cons word (cl-count word cleaned :test #'equal)))))


(provide 'word-count)
;;; word-count.el ends here
