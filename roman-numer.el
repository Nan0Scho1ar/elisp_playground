;;; roman-numer.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 08, 2022
;; Modified: May 08, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/roman-numer
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun to-roman (num)
  "Convert a decimal integer NUM (between 0 and 4000) to roman numerals."
  (when (and (numberp num) (> num 0) (< num 4000))
    (let ((m '("" "M" "MM" "MMM"))
          (c '("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"))
          (x '("" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"))
          (i '("" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")))
      (concat (nth (/ num 1000) m)
              (nth (/ (mod num 1000) 100) c)
              (nth (/ (mod num 100) 10) x)
              (nth (/ (mod num 10) 1) i)))))

(provide 'roman-numer)
;;; roman-numer.el ends here
