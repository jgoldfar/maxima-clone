;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;;; Predicate functions

(defun integerp2 (x)
  "Returns x if x is an integer, else false"
  (let (u)
    (cond ((not (numberp x)) nil)
	  ((not (floatp x)) x)
	  ((prog2 (setq u (maxima-rationalize x))
	       (equal (cdr u) 1))
           (car u)))))

(defun integerp1 (x)
  "Returns 2*x if 2*x is an integer, else nil"
  (integerp2 (mul2* 2 x)))

(defun zerp (a) (equal a 0))

(defun nonzerp (a) (not (equal a 0)))

