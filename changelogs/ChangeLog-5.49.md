Maxima 5.49 change log
===========================

Bug fixes for numbered bugs:
----------------------------
 * \#3041: limit(inf*(zeroa+inf)) => und, should be inf
 * \#3750: Quoting atan2 inhibits simplification
 * \#4383: great not transitive (so simplifya not idempotent)
 * \#4577: Update CSS for examples and nav bar
 * \#4601: 5.48 regression in spherical_bessel_j integral
 * \#4607: CF issues
 * \#4609: atan2(inf,inf) -> 0
 * \#4613: integrate(atan2(sin(x), cos(x)), x, 0, 9*%pi)
 * \#4614: atan2 reflection rule
 * \#4615: carg range is not in (-%pi, %pi]
 * \#4603: Control stack regression with abs_integrate / 5.48.0
 * \#4619: limit(inf = inf) causes stack overflow
 * \#4633: integrate(exp(- t) log(t), t, 0, 1) --> integral nounform
 * \#4636: signum(ind) is an error
 * \#4642 sign(1.0e-310*%i) gives error because 1e-310*x/1e-310 fails
 * \#4646: minor hstep problems and missing features
 * \#4647: Maxima error in --preload file causes Lisp error
 * \#4648: autoload problem with hstep

Changes in the Windows installer:
---------------------------------
 * Update SBCL
