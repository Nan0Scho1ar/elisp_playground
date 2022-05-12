;;; scratch.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 08, 2022
;; Modified: May 08, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/scratch
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
;;;
;;;



(defun choose-recipe ()
  "Choose a recipe from org roam."
  (org-roam-db-query [:select * :from nodes])
  (org-roam-db-query [:select * :from tags :where (= tag "Recipe")])

  (org-roam-db-query " SELECT n.*
                     FROM nodes n
                     JOIN tags t1
                     ON n.id = t1.node_id
                     JOIN tags t2
                     ON n.id = t2.node_id
                     WHERE t2.tag = Recipe")

  (org-roam-db-query "SELECT tag FROM tags WHERE tag = \"Recipe\"")


  (org-roam-db-query "SELECT t1.NODEID FROM tags t1 ")

  (org-roam-db-query "PRAGMA table_info(tags)"))




;; SELECT name FROM sqlite_master


;; (emacsql-with-bind db ([:select [name phone]
;;                         :from people
;;                         :where (= name $1)] my-name)



((files
  [(file :unique :primary-key) title (hash :not-null) (atime :not-null) (mtime :not-null)])
 (nodes
  ([(id :not-null :primary-key) (file :not-null) (level :not-null) (pos :not-null) todo priority (scheduled text) (deadline text) title properties olp]
   (:foreign-key [file] :references files [file] :on-delete :cascade)))
 (aliases
  ([(node-id :not-null) alias]
   (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))
 (citations
  ([(node-id :not-null) (cite-key :not-null) (pos :not-null) properties]
   (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))
 (refs
  ([(node-id :not-null) (ref :not-null) (type :not-null)]
   (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))
 (tags
  ([(node-id :not-null) tag]
   (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))
 (links
  ([(pos :not-null) (source :not-null) (dest :not-null) (type :not-null) (properties :not-null)]
   (:foreign-key [source] :references nodes [id] :on-delete :cascade))))



;;; scratch.el ends here
