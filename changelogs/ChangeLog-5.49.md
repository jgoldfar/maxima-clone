Maxima 5.49 change log
===========================

New items in core:
------------------
 * new flag display_matrix_padding_vertical to control between-rows padding for matrix display
 * new flag display_matrix_padding_horizontal to control between-columns padding for matrix display
 * new flag display_determinant_bars to display determinant of a literal matrix with a bar on either side
 * new flag display_matrix_brackets to display matrices with a bracket on either side
 * new flag display_box_double_lines to display box expressions with double-line characters, otherwise single-line

Bug fixes for numbered bugs:
----------------------------
 * \#3041: limit(inf\*(zeroa+inf)) => und, should be inf
 * \#3750: Quoting atan2 inhibits simplification
 * \#4383: great not transitive (so simplifya not idempotent)
 * \#4577: Update CSS for examples and nav bar
 * \#4601: 5.48 regression in spherical_bessel_j integral
 * \#4607: CF issues
 * \#4609: atan2(inf,inf) -> 0
 * \#4613: integrate(atan2(sin(x), cos(x)), x, 0, 9\*%pi)
 * \#4614: atan2 reflection rule
 * \#4615: carg range is not in (-%pi, %pi]
 * \#4603: Control stack regression with abs_integrate / 5.48.0
 * \#4619: limit(inf = inf) causes stack overflow
 * \#4633: integrate(exp(- t) log(t), t, 0, 1) --> integral nounform
 * \#4636: signum(ind) is an error
 * \#4632: gentran file management functions mishandle streams
 * \#4631: Lisp error in gentran with gentranlang = c (attempting symbol operation on non-symbol)
 * \#4626: solve(tan(x)^2 + 1, x) triggers error "atanh: argument 1 isn't in the domain of atanh"
 * \#4625: Lisp error in gentran with gentranlang = c
 * \#4642: sign(1.0e-310\*%i) gives error because 1e-310\*x/1e-310 fails
 * \#4646: minor hstep problems and missing features
 * \#4647: Maxima error in --preload file causes Lisp error
 * \#4648: autoload problem with hstep

Bug fixes for unnumbered bugs:
------------------------------
 * mailing list 2025-10-31: "are boxed objects noun/verb confused?"
 * unreported: enable ABCL to load lapack and dlsode via ASDF
 * unreported: in 2-d display for derivatives, correct depth of denominator in Leibniz notation

Changes in the Windows installer:
---------------------------------
 * Update SBCL
