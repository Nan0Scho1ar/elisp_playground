;;; leap-year.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 08, 2022
;; Modified: May 08, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/leap-year
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun leap-year-p (year)
  "Return non-nil if YEAR is a leap year."
  (or (and (zerop (mod year 4))
           (not (zerop (mod year 100))))
      (zerop (mod year 400))))

(provide 'leap-year)
;;; leap-year.el ends here
