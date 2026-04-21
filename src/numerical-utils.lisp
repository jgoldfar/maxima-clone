;;;;  -*- LISP -*-

;;;; This software has NO WARRANTY, not even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

;;; Utilities for determining if numerical evaluation should be done.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions test if numerical evaluation has to be done.
;;; The functions should help to test for numerical evaluation more consistent
;;; and without complicated conditional tests including more than one or two
;;; arguments.
;;;
;;; The functions take a list of arguments. All arguments have to be a CL or
;;; Maxima number. If all arguments are numbers we have two cases:
;;; 1. $numer is T we return T. The function has to be evaluated numerically.
;;; 2. One of the args is a float or a bigfloat. Evaluate numerically.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test for numerically evaluation in float precision

(defun float-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (float-or-rational-p ll)) 
        (return-from float-numerical-eval-p nil))
      (when (floatp ll) (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in complex float precision

(defun complex-float-numerical-eval-p (&rest args)
  "Determine if ARGS consists of numerical values by determining if
  the real and imaginary parts of each arg are nuemrical (but not
  bigfloats).  A non-NIL result is returned if at least one of args is
  a floating-point value or if numer is true.  If the result is
  non-NIL, it is a list of the arguments reduced via COMPLEX-NUMBER-P"
  (let (flag values)
    (dolist (ll args)
      (multiple-value-bind (bool rll ill)
          (complex-number-p ll 'float-or-rational-p)
        (unless bool
          (return-from complex-float-numerical-eval-p nil))
        ;; Always save the result from complex-number-p.  But for backward
        ;; compatibility, only set the flag if any item is a float.
        (push (add rll (mul ill '$%i)) values)
        (setf flag (or flag (or (floatp rll) (floatp ill))))))
    (when (or $numer flag)
      ;; Return the values in the same order as the args!
      (nreverse values))))

;;; Test for numerically evaluation in bigfloat precision

(defun bigfloat-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (bigfloat-or-number-p ll)) 
        (return-from bigfloat-numerical-eval-p nil))
      (when ($bfloatp ll)
	(setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in complex bigfloat precision

(defun complex-bigfloat-numerical-eval-p (&rest args)
  "Determine if ARGS consists of numerical values by determining if
  the real and imaginary parts of each arg are nuemrical (including
  bigfloats). A non-NIL result is returned if at least one of args is
  a floating-point value or if numer is true. If the result is
  non-NIL, it is a list of the arguments reduced via COMPLEX-NUMBER-P."

  (let (flag values)
    (dolist (ll args)
      (multiple-value-bind (bool rll ill)
          (complex-number-p ll 'bigfloat-or-number-p)
        (unless bool
          (return-from complex-bigfloat-numerical-eval-p nil))
	;; Always save the result from complex-number-p.  But for backward
	;; compatibility, only set the flag if any item is a bfloat.
	(push (add rll (mul ill '$%i)) values)
	(when (or ($bfloatp rll) ($bfloatp ill))
          (setf flag t))))
    (when (or $numer flag)
      ;; Return the values in the same order as the args!
      (nreverse values))))

;;; Test for numerical evaluation in any precision, real or complex.
(defun numerical-eval-p (&rest args)
  (or (apply 'float-numerical-eval-p args)
      (apply 'complex-float-numerical-eval-p args)
      (apply 'bigfloat-numerical-eval-p args)
      (apply 'complex-bigfloat-numerical-eval-p args)))

;;; Check for an integer or a float or bigfloat representation. When we
;;; have a float or bigfloat representation return the integer value.

(defun integer-representation-p (x)
  (let ((val nil))
    (cond ((integerp x) x)
          ((and (floatp x) (= 0 (nth-value 1 (truncate x))))
           (nth-value 0 (truncate x)))
          ((and ($bfloatp x) 
                (eq ($sign (sub (setq val ($truncate x)) x)) '$zero))
           val)
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for Bigfloat numerical evaluation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmul (x y) ($rectform (mul x y)))

(defun cdiv (x y) ($rectform (div x y)))

(defun cpower (x y) ($rectform (power x y)))


