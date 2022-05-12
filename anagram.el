;;; anagram.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 09, 2022
;; Modified: May 09, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/anagram
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun string-sort-chars (str)
  "Sort the characters in string STR."
  (apply #'concat
         (sort (split-string str "" t) #'string-lessp)))

(defun is-anagram-p (word1 word2)
  "Return non-nil if WORD2 is an anagram of WORD1."
  (and (= (length word1) (length word2))
       (not (equal word1 word2))
       (equal (string-sort-chars (downcase word1))
              (string-sort-chars (downcase word2)))))

;; seq-filter is an alternative for this do list

(defun anagrams-for (word wlist)
  "Return a sublist of WLIST containing all words\
which are an anagram of WORD."
  (reverse
   (let (sublist)
     (dolist (w wlist sublist)
       (when (is-anagram-p word w)
         (setq sublist (cons w sublist)))))))


(provide 'anagram)
;;; anagram.el ends here
