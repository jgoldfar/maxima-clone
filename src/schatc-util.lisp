;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;; Utility functions for working with M2 pattern matching results

(defun cdras (a b)
  "Extract the value associated with key A from association list B.
   This is commonly used to extract matched variables from M2 pattern matching results."
  (cdr (assoc a b :test #'equal)))
