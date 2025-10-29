Maxima 5.48 change log
===========================

New items in core:
------------------
 * Numerous performance improvements
 * Print errors and warnings to stderr instead of stdout
 * New command-line option --quit-on-error to make Maxima quit upon first
   runtime error
 * New command-line option --very-very-quiet to silence Maxima
 * New command-line option --suppress-input-echo to suppress input echo
   when running noninteractively
 * New global variable batch_answers_from_file
 * New Unicode display2d and global variable display2d_unicode
 * A new function random-name in plot.lisp that generates a random string
 * New function timer_reset: set accumulated elapsed time to zero
 * New functions float_infinity_p, float_nan_p: recognize float infinity and not-a-number

New items in share:
-------------------
 * New example share/colnew/prob5.mac
 * New package raddenest: radical denesting function
 * New draw terminals: tikz and tikz_standalone
 * ode2 now solves Modified Bessel differential equation
 * Documentation for share/contrib/levin
 * New package guess: guess a formula for a sequence of numbers
 * New package gamma_simp: simplification of gamma function
 * Package descriptive: function cov accepts optional argument for per-datum weights
 * Package wrstcse: Support tolerance calculations using the RMS method

Changes in core:
----------------
 * Add answers_from_file option to run_testsuite
 * Add rtest_raddenest to share_testsuite_files
 * Plotting: clipping is no longer done when plot_format is xmaxima.
	The default y-range for plots is determined only by points within
	x-range. New function reset_plot_options. New plotting options
	gnuplot, gnuplot_pipes and xmaxima.
 * Function chinese renamed to solve_congruences
 * Function 'system': capture output (e.g. with_stdout) from executed command

Changes in share:
-----------------
 * openr_binary: work around bug in Allegro CL by omitting :ELEMENT-TYPE argument.
 * readline: trim carriage return from end of string, if any.

Changes in Xmaxima:
-------------------
 * Line clipping in plot2d  and plotdf
 * Better integration algorithm for plotdf
 * new options tstep and algorithm for plotdf
 * plot linewidths and arrow lengths change when a plot window is resized

Changes in the Windows installer:
---------------------------------
 * Update Gnuplot
 * Update ABCL
 * Update wxMaxima
 * Update wxWidgets
 * Update TCL/TK
 * Update SBCL
 * Support Clozure Common Lisp (for 64 bit installers)
 * Use configured paths to Lisp compilers, this should help,
   if one is building Maxima on Windows (native) and uses
   special paths to the Lisp compilers.
 * Fix calculation of %maxima_prefix% in maxima.bat.in.
 * Allow building an installer with any (supported) Lisp.
   Before, CLISP and SBCL was always included.

Bug fixes for numbered bugs:
----------------------------
 * \#2249: question from limit involving a float
 * \#2317: subst allows more than 3 arguments
 * \#2577: Parser complains but doesn't err out on bad syntax
 * \#2905: Assigning variable twice yields different results
 * \#2910: In maxima 5.35.1 powerseries fail if there complex root
 * \#3054: limit(exp(exp(2*log(x**5 + x)*log(log(x)))) / exp(exp(10*log(x)*log(log(x)))), x, inf)
 * \#3191: parsing problem: thru 3 for i in [a,b]
 * \#3579: Overly eager simplification in trigreduce
 * \#3645: Lisp error for elliptic_f
 * \#3755: Insecure tmpdir usage
 * \#3803: Xmaxima should reopen the browser when needed
 * \#3849: plot2d incorrectly plots |xy|+1=|x|+|y|
 * \#3904: dispsym is not defined in manual
 * \#3907: gnuplot_postamble not actually the last Gnuplot output before plot
 * \#3932: inconsistent limit results with trig
 * \#3981: Trigreduce doesn't return principal value of atan(tan(...))
 * \#4001: limit(sin(x)/​x + sin(x) + cos(x),x,inf) = 0
 * \#4003: limit bug with on-default values for ratsimpexpons and ratfac
 * \#4024: integrate(x*sin(x)*exp(-x^2),x,0, inf);
 * \#4073: limit(log(sin(x)+9),x,inf) --> und could be ind
 * \#4079: gamma to factorial conversions in limit
 * \#4099: gruntz called on algebraic
 * \#4117: [Plotting] Unexpected Behaviour of run_viewer and gnu_term dumb
 * \#4121: atan2(0, <nz>) doesn't simplify
 * \#4133: limit(abs(cos(a*x)),x,inf)
 * \#4138: limit((1+1/​x)^x,x,6/​5) is "und" in complex domain
 * \#4143: atan2(0,minf) wrong
 * \#4145: pochhammer doesn't survive kill(all)
 * \#4147: integrate(log(sin(x))/​cos(x),x,0,%pi/​2)
 * \#4149: HTML docs display at wrong location
 * \#4151: limit bug as x tend to inf for trigonometric expressions
 * \#4153: rectform(atan2(1,1+%i)) gives an internal error
 * \#4154: Some info nodes have bad headers (and no "Up"s)
 * \#4155: to_poly_solve simpatan2
 * \#4156: generalized_lambert_w won't provide float solution
 * \#4158: unable to numerically evaluate li[2](%i)
 * \#4160: LISP error when integrating
 * \#4161: imaxima and imath web site not found
 * \#4168: test file not found
 * \#4269: plot3d ignores the option gnuplot_postamble
 * \#4174: ecl: Zeilberger/​constants.mac not found
 * \#4176: fullratsubst(old = new, expr) returns incorrect result
 * \#4177: lratsubst1 in testsuite
 * \#4179: [XMaxima] Pb in maximizing window
 * \#4181: quad_qagp fails if endpoint is a singularity and it also appears on list of special points
 * \#4182: infsimp* tests for atom, should be a mapatom
 * \#4183: plotting error: "can't open file"
 * \#4184: bessel_simplify error
 * \#4191: limits of gamma_incomplete
 * \#4195: cmucl 21e intermittent lisp error in limit
 * \#4196: The function `antid` is listed in the category for derivatives
 * \#4197: limits of erfi expressions & extended reals
 * \#4198: 2D Plot issue
 * \#4200: simplim%limit ignoring special variables
 * \#4208: Error with complex numbers - example with trigreduce
 * \#4210: tlimit consults global assumptions for limit variable
 * \#4214: read_xpm not autoloading and not documented as requiring load("draw")
 * \#4215: limit(sqrt(-1+%i*x),x,0,minus)
 * \#4218: read_xpm can't handle XPM files generated by ImageMagick and maybe other sources
 * \#4220: tex output does not respect linel
 * \#4221: integration of a squared variable or abs of squared variable is not assumed to be positive.
 * \#4222: limit(6^x, x, 1) and similar cases: stack overflow crash
 * \#4227: limit(abs(sin(x))/sin(x), x, inf) = 1
 * \#4237: implicit_plot punting to plot2d implicit fails when argument is a list of equations
 * \#4238: limit((abs(sin(x)*cos(x)-x^4) -x)/x^3,x,0,plus) gives internal error
 * \#4245: is(string1<string2) gives Lisp error
 * \#4246: tcl9.0b1 upgrade and xmaxima
 * \#4251: imaxima could not display image with Emacs 29.1
 * \#4254: plog returns unsimplfied results
 * \#4257: Incorrect change of variable in integral involving diff
 * \#4260: translate fails with go tag in final position
 * \#4263: plot2d discrete with x range computes y range over all data, not restricted to x
 * \#4265: integrate(sin(m*x)*sin(n*x), x, 0, 2*%pi) unconditionally zero
 * \#4269: plot3d ignores the option gnuplot_postamble
 * \#4273: limit(integrate(f(x)*g(x), x), x, a) incorrect results
 * \#4277: limit misses some constant expressions
 * \#4281: product sign - test suite
 * \#4284: Incorrect arg to quad_qawo produces a lisp error instead of a maxima error.
 * \#4287: 2d pretty printer ignores successive empty lines
 * \#4292: Special values for li[2](z)
 * \#4295: Add Jabobi Amplitude
 * \#4296: expop/​expon must be integers
 * \#4297: plot3d() error with Gnuplot 6
 * \#4298: atanh(- 1.0110463357335220588733500035655b-33+1b0*%i) hangs
 * \#4299: complex-atanh signals error
 * \#4304:"Warning: 'tube_extremes' is deprecated, using 'capping' instead..."
 * \#4307: partswitch affects op and operatorp
 * \#4308: gfactor throws Lisp error
 * \#4317: Maxima doesn't know that signum(real) can only be -1, 0 or +1
 * \#4325: Maxima can't differentiate beta function
 * \#4326: sin(asec(x)), cos(acsc(x)) and many more are incorrectly simplified
 * \#4328: Undefined limit product $INF * $ZEROA in lim-times
 * \#4346: Inconsistent behavior of trig functions at undefined points for floats and bfloats
 * \#4349: user-defined rules apply to expressions with square brackets as well as parentheses
 * \#4350: arrayinfo complains "not an array" when supplied a Lisp array or hash table
 * \#4352: elliptic_e(1,1.23) --> lisp numeric error (introduces complex %i)
 * \#4354: gnuplot_dumb_term_command causes "set term dumb" to be ignored
 * \#4355: documentation for trigvalue etc
 * \#4356: support tracing simplifying functions
 * \#4368: incorrect limit(li[2](2*exp(x*%i)), x, 0, plus)
 * \#4370: eigenvalues doc should xref dgeev
 * \#4373: conjugate doesn't know li[n](x) is complex in general
 * \#4378: trace(AUTOLOADFUN) gives error
 * \#4379: assignment: cannot assign ... to `tab': must be a string.
 * \#4380: Strange error message for plot2d(a=b...)
 * \#4384: plot3d with no real values gives internal error
 * \#4386: quad_qagi doesn't recognize -inf as equivalent to minf
 * \#4388: tex() output under simp:false, incorrect placement of `+`
 * \#4394: Proviso printing overlaps with output
 * \#4398: laplace (delta (t - a) * sin(b*t), t, s) gives zero
 * \#4401: Maxima News page is way out of date
 * \#4402: Intro to Maxima page is missing
 * \#4404: Message about incorrect syntax with thru and do is hard to read
 * \#4405: rtest8 problem 197 fails
 * \#4406: Lapack docs should probably use mathjax for nice equations
 * \#4408: letsimp doesn't simplify before returning result
 * \#4413: Noun/​verb confusion in dgemm
 * \#4414: Bug reporting section is a bit confusing
 * \#4415: Make website work a bit better on mobile devices
 * \#4417: letsimp treats minus expressions as -1 times something
 * \#4418: arctan and trigreduce error
 * \#4421: plot3d: calculation of surface intersection failed
 * \#4423: Documentation for trig functions should reference triginverses and maybe other flags
 * \#4424: draw2d doesnt accept fill_density in order to create opaque shapes
 * \#4426: Remove repeated function resm1
 * \#4427: Turn number to cell in mark+3@db.lisp
 * \#4430: solve calls eval on its arguments
 * \#4434: polydecomp documentation wrong
 * \#4435: When using a frame field the icc2 or msc symbol is incorrect.
 * \#4441: gf_factor in ASDF build
 * \#4442: rationalize applied to subscripted
 * \#4446: append doesn't know that structs are of fixed arity
 * \#4450: No tests for psi[s](p/​q)?
 * \#4451: ratsimp(%e^(%i*sqrt(%pi+1)/​2)/​%pi), algebraic - endless loop/​recursion
 * \#4454: Support * in to_lisp() mode
 * \#4455: 'great' errors
 * \#4456: ?great(1.0, 1.0) -> T
 * \#4458: orderlessp(A, B) with unsimplified A can modify A in place by marking it as simplified, when it's not
 * \#4459: great(2,2.0), great(2.0,2), and alike1(2,2.0) are all false
 * \#4461: sec(%pi/​2) gives funny error message and other cases too
 * \#4462: sign(1/sqrt(x))
 * \#4464: simpsum1/​simpprod1 replace $sum/​$product with %sum/​%product
 * \#4467: ordering of big floats
 * \#4471: with_stdout doesn't capture output of command executed via system
 * \#4474: "sign: argument cannot be imaginary" in simple substitution
 * \#4475: min/max (%i,...)
 * \#4479: Setting inchar to empty string causes infinite lisp errors
 * \#4482: Internal code using user-level functions, e.g. $bfloatp
 * \#4483: Speed up onep1
 * \#4486: trigrat(atan2(1,sin(k))) gives error
 * \#4487: problem & line numbers in batch files
 * \#4490: error message when adding non-conformable matrices
 * \#4491: minpack_solve does better with a *repeated* expression
 * \#4492: minpack_solve doesn't check number of equations and unknowns
 * \#4497: defrule(false,...) gives Lisp error
 * \#4500: rectform of set
 * \#4502: Antiderivative missing term "x": integrate(x/​(x+%e^x+1),x)
 * \#4504: properties shows properties with NIL value
 * \#4505: rectform(psi(x))
 * \#4506: Subscripted function simplifiers don't check subscript/​argument count
 * \#4507: rectform uses conjugate when it doesn't help
 * \#4509: distribute-over does too little and too much simplification
 * \#4515: alike1 ignores bfloat precisions, compares only mantissa and exponent
 * \#4516: definite integral error with gamma_expand : true, domain : complex
 * \#4519: tellsimp and tellsimpafter don't check argument count
 * \#4521: Can not compile
 * \#4522: in definite integrals each log(expr) is erroneously evaluated as log(abs(expr))
 * \#4523: compile warning for function log*rat
 * \#4527: batch("foo.mac", test) triggers Lisp error in UNIX-LIKE-BASENAME
 * \#4529: atan2 limit with radexpand : false
 * \#4530: limit(floor( 1/​2 + sin(1/​x)/​10), x, 0) returns ind but should be 0
 * \#4531: Integer-valuedness of signum and unit_step; noun/​verb
 * \#4536: atan2(0,0) undefined error while evaluating a limit
 * \#4537: simplimln sometimes exits non-locally when it doesn’t need to.
 * \#4539: some limits of expintegral_ci
 * \#4540: scanmap non-bottomup should be easier
 * \#4544: ratdisrep([taylor(k,k,inf,1)]) is unsimplified
 * \#4546: limit of a log of a rational trig polynomial
 * \#4548: float(10^400) returns false on GCL
 * \#4550: $member and $assoc are O(n) even if sought element is at front
 * \#4560: integrals of the form integrate((a*z+a*b)^p*%e^(c*z+d),z)
 * \#4561: taylim (non user level) exits non-locally
 * \#4567: gnuplot gives obscure Lisp error when gnuplot_command is not a string
 * \#4569: reset(plot_options) doesn't reset plot_options
 * \#4572: integration with subs of the form y = exp(X)
 * \#4575: expintrep doesn't reset

Unnumbered bugs fixed:
---------------------
 * mailing list 2023-11-25: Possibilities for basic image processing
 * mailing list 2023-11-26: plotdf sliders hidden
 * mailing list 2023-09-25: Casting SPELs
 * mailing list 2024-12-24: Typographic Error (xmaxima)
 * mailing list 2025-03-04: trigsolve requires to_poly_solve to work
 * correct failing test in share/contrib/diffequations/tests
 * recovers option xfun of plotdf which was no longer working
 * fixes narrows option of plotdf (narrows should remain after window resizing)
 * unreported bug: Clisp error when standard input is not writeable
 * unreported bug: Function 'system': fix problems encountered when testing on Windows

Documentation:
--------------
 * Document colnew share package
 * \#4154: Some info nodes have bad headers (and no "Up"s)
 * HTML files for the manual now have the name of the corresponding section
 * Documentation for display2d_unicode in section "Command Line"
 * HTML documentation supports syntax highlighting of examples

Build system:
-------------
 * Install Maxima icons in an 'icons' subdirectory rather than 'pixmaps'
 * maxima.asd: load defsystem.lisp in order to load share packages which use it.
 * Various compatibility fixes for Allegro CL and LispWorks
 * Configure supports new option `--enable-syntax-highlighting` that
	   enables syntax highlighting of HTML version of the user manual.
