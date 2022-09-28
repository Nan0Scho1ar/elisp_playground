;;; schnitz.el -*- lexical-binding: t; -*-
;;; schnitz --- Helping you break meal planning into bite sized pieces.
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 13, 2022
;; Modified: May 13, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/schnitz
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'org-roam)


(defun schnitz-filter-raw-org-roam-data-by-tag (data tag)
  "Return all rows in DATA tagged with TAG."
  (mapcar #'car
          (seq-filter
           (lambda (x) (string-match tag (cadddr x)))
           data)))


(defun schnitz-filter-raw-roam-data-by-ids-then-group (data node-ids)
  "Return all nodes and tags in DATA which are associated with the given NODE-IDS."
  (seq-group-by #'car
                (seq-filter (lambda (x) (seq-contains-p node-ids (car x)))
                            data)))


(defun schnitz-restructure-raw-org-roam-data (data)
  "Combine DATA to only a single list per node containing a list of tags."
  (mapcar (lambda (x) (list (car x)
                       (cadadr x)
                       (car (cddadr x))
                       (mapcar #'cadddr (cdr x))))
          data))


(defun schnitz-fetch-raw-org-roam-data ()
  "Fetch ids, title, file paths, and tads from the org roam database."
  (org-roam-db-query [:select :distinct [id title file tag]
                      :from tags :join nodes :on (= id node-id)]))


(defun schnitz-fetch-org-roam-data-by-tag (tag)
  "Fetch org roam files [id, title, path, [tags]] matching TAG."
  (let* ((data (schnitz-fetch-raw-org-roam-data))
         (node-ids (schnitz-filter-raw-org-roam-data-by-tag data tag))
         (nodes  (schnitz-filter-raw-roam-data-by-ids-then-group data node-ids)))
    (schnitz-restructure-raw-org-roam-data nodes)))


(defun schnitz-fetch-recipe-file-list ()
  "Fetch recipe file list from org roam."
  (schnitz-fetch-org-roam-data-by-tag "Recipe"))


(defun schnitz-create-recipe-link (recipe)
  "Return an org roam link for the given RECIPE."
  (concat "[[id:" (car recipe) "][" (cadr recipe) "]]"))


(provide 'schnitz)
;;; schnitz.el ends here


;; Scratch

(schnitz-create-recipe-link (car (schnitz-fetch-recipe-file-list)))
