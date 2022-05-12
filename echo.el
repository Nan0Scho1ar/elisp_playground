#!/usr/bin/env -S emacs --script
(condition-case nil
  (progn
    (dotimes (i (length argv) nil)
            (princ (nth i argv))
            (princ " "))
    (princ "\n"))
  (error nil))
