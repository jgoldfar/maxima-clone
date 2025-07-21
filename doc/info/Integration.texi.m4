@c -*- mode: texinfo -*-
@menu
* Introduction to Integration::
* Functions and Variables for Integration::
* Introduction to QUADPACK::
* Functions and Variables for QUADPACK::
@end menu

@c -----------------------------------------------------------------------------
@node Introduction to Integration, Functions and Variables for Integration, Integration, Integration
@section Introduction to Integration
@c -----------------------------------------------------------------------------

Maxima has several routines for handling integration.
The @mref{integrate} function makes use of most of them.  There is also the
@mref{antid} package, which handles an unspecified function (and its
derivatives, of course).  For numerical uses,
there is a set of adaptive integrators from QUADPACK, named @mrefcomma{quad_qag}
@mrefcomma{quad_qags} etc., which are described under the heading @code{QUADPACK}.
Hypergeometric functions are being worked on,
see @mref{specint} for details.
Generally speaking, Maxima only handles integrals which are
integrable in terms of the "elementary functions" (rational functions,
trigonometrics, logs, exponentials, radicals, etc.) and a few
extensions (error function, dilogarithm).  It does not handle
integrals in terms of unknown functions such as @code{g(x)} and @code{h(x)}.

@c end concepts Integration

@c -----------------------------------------------------------------------------
@node Functions and Variables for Integration, Introduction to QUADPACK, Introduction to Integration, Integration
@section Functions and Variables for Integration
@c -----------------------------------------------------------------------------

@c NEEDS WORK

@c -----------------------------------------------------------------------------
@anchor{changevar}
@deffn {Function} changevar (@var{expr}, @var{f(x,y)}, @var{y}, @var{x})

Makes the change of variable given by @code{@var{f(x,y)} = 0} in all integrals
occurring in @var{expr} with integration with respect to @var{x}.
The new variable is @var{y}.

The change of variable can also be written @code{@var{f(x)} = @var{g(y)}}.

@c HMM, THIS EXAMPLE YIELDS A CORRECT BUT SLIGHTLY STRANGE RESULT...
@c ===beg===
@c assume(a > 0)$
@c 'integrate (%e**sqrt(a*y), y, 0, 4);
@c changevar (%, y-z^2/a, z, y);
@c ===end===
@example
(%i1) assume(a > 0)$
@group
(%i2) 'integrate (%e**sqrt(a*y), y, 0, 4);
                      4
                     /
                     |    sqrt(a) sqrt(y)
(%o2)                |  %e                dy
                     |
                     /
                      0
@end group
@group
(%i3) changevar (%, y-z^2/a, z, y);
                      0
                     /
                     |              abs(z)
                   2 |            %e       z dz
                     |
                     /
                      - 2 sqrt(a)
(%o3)            - ----------------------------
                                a
@end group
@end example

An expression containing a noun form, such as the instances of @code{'integrate}
above, may be evaluated by @code{ev} with the @code{nouns} flag.
For example, the expression returned by @code{changevar} above may be evaluated
by @code{ev (%o3, nouns)}.

@code{changevar} may also be used to make changes in the indices of a sum or
product.  However, it must be realized that when a change is made in a
sum or product, this change must be a shift, i.e., @code{i = j+ ...}, not a
higher degree function.  E.g.,

@c ===beg===
@c sum (a[i]*x^(i-2), i, 0, inf);
@c changevar (%, i-2-n, n, i);
@c ===end===
@example
@group
(%i1) sum (a[i]*x^(i-2), i, 0, inf);
                         inf
                         ____
                         \         i - 2
(%o1)                     >    a  x
                         /      i
                         ----
                         i = 0
@end group
@group
(%i2) changevar (%, i-2-n, n, i);
                        inf
                        ____
                        \               n
(%o2)                    >      a      x
                        /        n + 2
                        ----
                        n = - 2
@end group
@end example

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@c THIS ITEM IS A MESS, BUT DON'T BOTHER TO CLEAN IT UP:
@c THE GAUSS-KRONROD FUNCTIONS (QUADPACK) MAKE THIS OBSOLETE

@c -----------------------------------------------------------------------------
@anchor{dblint}
@deffn {Function} dblint (@var{f}, @var{r}, @var{s}, @var{a}, @var{b})

A double-integral routine which was written in
top-level Maxima and then translated and compiled to machine code.
Use @code{load ("dblint")} to access this package.  It uses the Simpson's rule
method in both the x and y directions to calculate

@tex
$$\int_a^b \int_{r\left(x\right)}^{s\left(x\right)} f\left(x,y\right) \, dy \, dx.$$
@end tex
@ifnottex
@example
@group
/b /s(x)
|  |
|  |    f(x,y) dy dx
|  |
/a /r(x)
@end group
@end example
@end ifnottex

The function @var{f} must be a translated or compiled function of two variables,
and @var{r} and @var{s} must each be a translated or compiled function of one
variable, while @var{a} and @var{b} must be floating point numbers.  The routine
has two global variables which determine the number of divisions of the x and y
intervals: @code{dblint_x} and @code{dblint_y}, both of which are initially 10,
and can be changed independently to other integer values (there are
@code{2*dblint_x+1} points computed in the x direction, and @code{2*dblint_y+1}
in the y direction).  The routine subdivides the X axis and then for each value
of X it first computes @code{@var{r}(x)} and @code{@var{s}(x)}; then the Y axis
between @code{@var{r}(x)} and @code{@var{s}(x)} is subdivided and the integral
along the Y axis is performed using Simpson's rule; then the integral along the
X axis is done using Simpson's rule with the function values being the
Y-integrals.  This procedure may be numerically unstable for a great variety of
reasons, but is reasonably fast: avoid using it on highly oscillatory functions
and functions with singularities (poles or branch points in the region).  The Y
integrals depend on how far apart @code{@var{r}(x)} and @code{@var{s}(x)} are,
so if the distance @code{@var{s}(x) - @var{r}(x)} varies rapidly with X, there
may be substantial errors arising from truncation with different step-sizes in
the various Y integrals.  One can increase @code{dblint_x} and @code{dblint_y}
in an effort to improve the coverage of the region, at the expense of
computation time.  The function values are not saved, so if the function is very
time-consuming, you will have to wait for re-computation if you change anything
(sorry).  It is required that the functions @var{f}, @var{r}, and @var{s} be
either translated or compiled prior to calling @code{dblint}.  This will result
in orders of magnitude speed improvement over interpreted code in many cases!

@code{demo ("dblint")} executes a demonstration of @code{dblint} applied to an
example problem.
@c demo (dblint_1) FAILS WITH Could not find `fltdfnk.mc' -- DON'T BOTHER TO MENTION IT. !!!
@c @code{demo (dblint_1)} executes another demonstration.

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{defint}
@deffn {Function} defint (@var{expr}, @var{x}, @var{a}, @var{b})

Attempts to compute a definite integral.  @code{defint} is called by
@code{integrate} when limits of integration are specified, i.e., when
@code{integrate} is called as
@code{integrate (@var{expr}, @var{x}, @var{a}, @var{b})}.
Thus from the user's point of view, it is sufficient to call @code{integrate}.
@c SHOULD WE BOTHER TO DOCUMENT defint ??? NO FUNCTIONALITY HERE THAT IS NOT ALREADY PRESENT IN integrate !!!

@code{defint} returns a symbolic expression, either the computed integral or the
noun form of the integral.  See @mref{quad_qag} and related functions for
numerical approximation of definite integrals.

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{erfflag}
@defvr {Option variable} erfflag
Default value: @code{true}

When @code{erfflag} is @code{false}, prevents @code{risch} from introducing the
@code{erf} function in the answer if there were none in the integrand to
begin with.

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end defvr

@c NEEDS WORK

@c -----------------------------------------------------------------------------
@anchor{ilt}
@deffn {Function} ilt (@var{expr}, @var{s}, @var{t})

Computes the inverse Laplace transform of @var{expr} with
respect to @var{s} and parameter @var{t}. @var{expr} must be a ratio of
polynomials whose denominator has only linear and quadratic factors;
there is an extension of @code{ilt}, called @mref{pwilt} (Piece-Wise
Inverse Laplace Transform) that handles several other cases where
@code{ilt} fails.

By using the functions @code{laplace} and @code{ilt} together with the
@code{solve} or @code{linsolve} functions the user can solve a single
differential or convolution integral equation or a set of them.

@c WARNING:  This needs manual editing for the last test to put the
@c answer in the right place.
@c ===beg===
@c 'integrate (sinh(a*x)*f(t-x), x, 0, t) + b*f(t) = t**2;
@c laplace (%, t, s);
@c linsolve ([%], ['laplace(f(t), t, s)]);
@c ilt (rhs (first (%)), s, t);
@c input:pos;
@c ===end===
@example
@group
(%i1) 'integrate (sinh(a*x)*f(t-x), x, 0, t) + b*f(t) = t**2;
              t
             /
             |                                    2
(%o1)        |  f(t - x) sinh(a x) dx + b f(t) = t
             |
             /
              0
@end group
@group
(%i2) laplace (%, t, s);
       a laplace(f(t), t, s)                           2
(%o2)  --------------------- + b laplace(f(t), t, s) = --
               2    2                                   3
              s  - a                                   s
@end group
@group
(%i3) linsolve ([%], ['laplace(f(t), t, s)]);
                                        2      2
                                     2 s  - 2 a
(%o3)     [laplace(f(t), t, s) = --------------------]
                                    5         2     3
                                 b s  + (a - a  b) s
@end group
@group
(%i4) ilt (rhs (first (%)), s, t);
Is a b (a b - 1) positive, negative or zero?
pos;

               sqrt(a b (a b - 1)) t
        2 cosh(---------------------)       2
                         b               a t
(%o4) - ----------------------------- + -------
              3  2      2               a b - 1
             a  b  - 2 a  b + a
                                                       2
                                             + ------------------
                                                3  2      2
                                               a  b  - 2 a  b + a
@end group
@end example

@opencatbox{Categories:}
@category{Laplace transform}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{intanalysis}
@defvr {Option variable} intanalysis
Default value: @code{true}

When @code{true}, definite integration tries to find poles in the integrand in 
the interval of integration.  If there are, then the integral is evaluated
appropriately as a principal value integral.  If intanalysis is @code{false}, 
this check is not performed and integration is done assuming there are no poles.

See also @mrefdot{ldefint}

Examples:

Maxima can solve the following integrals, when @mref{intanalysis} is set to
@code{false}:

@c ===beg===
@c integrate(1/(sqrt(x+1)+1),x,0,1);
@c integrate(1/(sqrt(x)+1),x,0,1),intanalysis:false;
@c integrate(cos(a)/sqrt((tan(a))^2+1),a,-%pi/2,%pi/2),intanalysis:false;
@c intanalysis:false$
@c integrate(cos(a)/sqrt((tan(a))^2 +1),a,-%pi/2,%pi/2);
@c ===end===
@example
@group
(%i1) integrate(1/(sqrt(x+1)+1),x,0,1);
                                              3/2
(%o1)      - 2 log(sqrt(2) + 1) + 2 log(2) + 2    - 2
@end group
@group
(%i2) integrate(1/(sqrt(x)+1),x,0,1),intanalysis:false;
(%o2)                     2 - 2 log(2)
@end group
@group
(%i3) integrate(cos(a)/sqrt((tan(a))^2+1),a,-%pi/2,%pi/2),intanalysis:false;
(%o3)               %i log(2) - %i log(2 %i)
@end group
(%i4) intanalysis:false$
@group
(%i5) integrate(cos(a)/sqrt((tan(a))^2 +1),a,-%pi/2,%pi/2);
(%o5)               %i log(2) - %i log(2 %i)
@end group
@end example

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{integrate}
@deffn  {Function} integrate @
@fname{integrate} (@var{expr}, @var{x}) @
@fname{integrate} (@var{expr}, @var{x}, @var{a}, @var{b})

Attempts to symbolically compute the integral of @var{expr} with respect to
@var{x}.  @code{integrate (@var{expr}, @var{x})} is an indefinite integral,
while @code{integrate (@var{expr}, @var{x}, @var{a}, @var{b})} is a definite
integral, with limits of integration @var{a} and @var{b}.  The limits should
not contain @var{x}, although @code{integrate} does not enforce this
restriction.  @var{a} need not be less than @var{b}.
If @var{b} is equal to @var{a}, @code{integrate} returns zero.

See @mref{quad_qag} and related functions for numerical approximation of
definite integrals.  See @mref{residue} for computation of residues
(complex integration).  See @mref{antid} for an alternative means of computing
indefinite integrals.

The integral (an expression free of @code{integrate}) is returned if
@code{integrate} succeeds.  Otherwise the return value is
the noun form of the integral (the quoted operator @code{'integrate})
or an expression containing one or more noun forms.
The noun form of @code{integrate} is displayed with an integral sign.

In some circumstances it is useful to construct a noun form by hand, by quoting
@code{integrate} with a single quote, e.g.,
@code{'integrate (@var{expr}, @var{x})}.  For example, the integral may depend
on some parameters which are not yet computed.
The noun may be applied to its arguments by @code{ev (@var{i}, nouns)}
where @var{i} is the noun form of interest.

@c BEGIN EXPOSITION ON HEURISTICS
@code{integrate} handles definite integrals separately from indefinite, and
employs a range of heuristics to handle each case.  Special cases of definite
integrals include limits of integration equal to zero or infinity (@mref{inf} or
@mref{minf}), trigonometric functions with limits of integration equal to zero
and @code{%pi} or @code{2 %pi}, rational functions, integrals related to the
definitions of the @mref{beta} and @mref{psi} functions, and some logarithmic
and trigonometric integrals.  Processing rational functions may include
computation of residues.  If an applicable special case is not found, an attempt
will be made to compute the indefinite integral and evaluate it at the limits of
integration.  This may include taking a limit as a limit of integration goes to
infinity or negative infinity; see also @mrefdot{ldefint}

Special cases of indefinite integrals include trigonometric functions,
exponential and logarithmic functions,
and rational functions.
@code{integrate} may also make use of a short table of elementary integrals.

@code{integrate} may carry out a change of variable
if the integrand has the form @code{f(g(x)) * diff(g(x), x)}.
@code{integrate} attempts to find a subexpression @code{g(x)} such that
the derivative of @code{g(x)} divides the integrand.
This search may make use of derivatives defined by the @code{gradef} function.
See also @mref{changevar} and @mrefdot{antid}

If none of the preceding heuristics find the indefinite integral, the Risch
algorithm is executed.  The flag @mref{risch} may be set as an @mrefcomma{evflag}
in a call to @code{ev} or on the command line, e.g.,
@code{ev (integrate (@var{expr}, @var{x}), risch)} or
@code{integrate (@var{expr}, @var{x}), risch}.  If @code{risch} is present,
@code{integrate} calls the @mref{risch} function without attempting heuristics
first.  See also @mrefdot{risch}
@c END EXPOSITION ON HEURISTICS

@code{integrate} works only with functional relations represented explicitly
with the @code{f(x)} notation.  @code{integrate} does not respect implicit
dependencies established by the @mref{depends} function.

@code{integrate} may need to know some property of a parameter in the integrand.
@code{integrate} will first consult the @mref{assume} database,
and, if the variable of interest is not there,
@code{integrate} will ask the user.
Depending on the question,
suitable responses are @code{yes;} or @code{no;},
or @code{pos;}, @code{zero;}, or @code{neg;}.

@code{integrate} is not, by default, declared to be linear.  See @code{declare}
and @code{linear}.

@code{integrate} attempts integration by parts only in a few special cases.

Examples:

@itemize @bullet
@item
Elementary indefinite and definite integrals.

@c ===beg===
@c integrate (sin(x)^3, x);
@c integrate (x/ sqrt (b^2 - x^2), x);
@c integrate (cos(x)^2 * exp(x), x, 0, %pi);
@c integrate (x^2 * exp(-x^2), x, minf, inf);
@c ===end===
@example
@group
(%i1) integrate (sin(x)^3, x);
                           3
                        cos (x)
(%o1)                   ------- - cos(x)
                           3
@end group
@group
(%i2) integrate (x/ sqrt (b^2 - x^2), x);
                                 2    2
(%o2)                    - sqrt(b  - x )
@end group
@group
(%i3) integrate (cos(x)^2 * exp(x), x, 0, %pi);
                               %pi
                           3 %e      3
(%o3)                      ------- - -
                              5      5
@end group
@group
(%i4) integrate (x^2 * exp(-x^2), x, minf, inf);
                            sqrt(%pi)
(%o4)                       ---------
                                2
@end group
@end example

@item
Use of @code{assume} and interactive query.

@c WARNING:  This needs manual editing to get the answers to the
@c questions in the right place.
@c ===beg===
@c assume (a > 1)$
@c integrate (x**a/(x+1)**(5/2), x, 0, inf);
@c input:no;
@c input:neg;
@c ===end===
@example
(%i1) assume (a > 1)$
@group
(%i2) integrate (x**a/(x+1)**(5/2), x, 0, inf);
Is a an integer?
no;

@end group
@group
Is 2 a - 1 positive, negative or zero?
neg;
                            3
(%o2)                  beta(- - a, a + 1)
                            2
@end group
@end example

@item
Change of variable.  There are two changes of variable in this example:
one using a derivative established by @mrefcomma{gradef} and one using the
derivation @code{diff(r(x))} of an unspecified function @code{r(x)}.

@c ===beg===
@c gradef (q(x), sin(x**2));
@c diff (log (q (r (x))), x);
@c integrate (%, x);
@c ===end===
@example
@group
(%i1) gradef (q(x), sin(x**2));
(%o1)                         q(x)
@end group
@group
(%i2) diff (log (q (r (x))), x);
                      d               2
                     (-- (r(x))) sin(r (x))
                      dx
(%o2)                ----------------------
                            q(r(x))
@end group
@group
(%i3) integrate (%, x);
(%o3)                     log(q(r(x)))
@end group
@end example

@item
Return value contains the @code{'integrate} noun form.  In this example, Maxima
can extract one factor of the denominator of a rational function, but cannot
factor the remainder or otherwise find its integral.  @mref{grind} shows the
noun form @code{'integrate} in the result.  See also
@mref{integrate_use_rootsof} for more on integrals of rational functions.

@c ===beg===
@c expand ((x-4) * (x^3+2*x+1));
@c integrate (1/%, x);
@c grind (%);
@c ===end===
@example
@group
(%i1) expand ((x-4) * (x^3+2*x+1));
                    4      3      2
(%o1)              x  - 4 x  + 2 x  - 7 x - 4
@end group
@group
(%i2) integrate (1/%, x);
                              /  2
                              | x  + 4 x + 18
                              | ------------- dx
                              |  3
                 log(x - 4)   / x  + 2 x + 1
(%o2)            ---------- - ------------------
                     73               73
@end group
@group
(%i3) grind (%);
log(x-4)/73-('integrate((x^2+4*x+18)/(x^3+2*x+1),x))/73$
(%o3)                         done
@end group
@end example

@item
Defining a function in terms of an integral.  The body of a function is not
evaluated when the function is defined.  Thus the body of @code{f_1} in this
example contains the noun form of @code{integrate}.  The quote-quote operator
@code{'@w{}'} causes the integral to be evaluated, and the result becomes the
body of @code{f_2}.

@c ===beg===
@c f_1 (a) := integrate (x^3, x, 1, a);
@c ev (f_1 (7), nouns);
@c /* Note parentheses around integrate(...) here */  f_2 (a) := ''(integrate (x^3, x, 1, a));
@c f_2 (7);
@c ===end===
@example
@group
(%i1) f_1 (a) := integrate (x^3, x, 1, a);
                                     3
(%o1)           f_1(a) := integrate(x , x, 1, a)
@end group
@group
(%i2) ev (f_1 (7), nouns);
(%o2)                          600
@end group
@group
(%i3) /* Note parentheses around integrate(...) here */
      f_2 (a) := ''(integrate (x^3, x, 1, a));
                                   4
                                  a    1
(%o3)                   f_2(a) := -- - -
                                  4    4
@end group
@group
(%i4) f_2 (7);
(%o4)                          600
@end group
@end example
@end itemize

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{integration_constant}
@defvr {System variable} integration_constant
Default value: @code{%c}

When a constant of integration is introduced by indefinite integration of an
equation, the name of the constant is constructed by concatenating
@code{integration_constant} and @code{integration_constant_counter}.

@code{integration_constant} may be assigned any symbol.

Examples:

@c ===beg===
@c integrate (x^2 = 1, x);
@c integration_constant : 'k;
@c integrate (x^2 = 1, x);
@c ===end===
@example
@group
(%i1) integrate (x^2 = 1, x);
                           3
                          x
(%o1)                     -- = x + %c1
                          3
@end group
@group
(%i2) integration_constant : 'k;
(%o2)                           k
@end group
@group
(%i3) integrate (x^2 = 1, x);
                            3
                           x
(%o3)                      -- = x + k2
                           3
@end group
@end example

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{integration_constant_counter}
@defvr {System variable} integration_constant_counter
Default value: 0

When a constant of integration is introduced by indefinite integration of an
equation, the name of the constant is constructed by concatenating
@code{integration_constant} and @code{integration_constant_counter}.

@code{integration_constant_counter} is incremented before constructing the next
integration constant.

Examples:

@c ===beg===
@c integrate (x^2 = 1, x);
@c integrate (x^2 = 1, x);
@c integrate (x^2 = 1, x);
@c reset (integration_constant_counter);
@c integrate (x^2 = 1, x);
@c ===end===
@example
@group
(%i1) integrate (x^2 = 1, x);
                           3
                          x
(%o1)                     -- = x + %c1
                          3
@end group
@group
(%i2) integrate (x^2 = 1, x);
                           3
                          x
(%o2)                     -- = x + %c2
                          3
@end group
@group
(%i3) integrate (x^2 = 1, x);
                           3
                          x
(%o3)                     -- = x + %c3
                          3
@end group
@group
(%i4) reset (integration_constant_counter);
(%o4)            [integration_constant_counter]
@end group
@group
(%i5) integrate (x^2 = 1, x);
                           3
                          x
(%o5)                     -- = x + %c1
                          3
@end group
@end example

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{integrate_use_rootsof}
@defvr {Option variable} integrate_use_rootsof
Default value: @code{false}

When @code{integrate_use_rootsof} is @code{true} and the denominator of
a rational function cannot be factored, @mref{integrate} returns the integral
in a form which is a sum over the roots (not yet known) of the denominator.

For example, with @code{integrate_use_rootsof} set to @code{false},
@code{integrate} returns an unsolved integral of a rational function in noun
form:

@c ===beg===
@c integrate_use_rootsof: false$
@c integrate (1/(1+x+x^5), x);
@c ===end===
@example
(%i1) integrate_use_rootsof: false$
@group
(%i2) integrate (1/(1+x+x^5), x);
      /  2
      | x  - 4 x + 5
      | ------------ dx                            2 x + 1
      |  3    2                2            5 atan(-------)
      / x  - x  + 1       log(x  + x + 1)          sqrt(3)
(%o2) ----------------- - --------------- + ---------------
              7                 14             7 sqrt(3)
@end group
@end example

Now we set the flag to be true and the unsolved part of the integral will be
expressed as a summation over the roots of the denominator of the rational
function:

@c ===beg===
@c integrate_use_rootsof: true$
@c integrate (1/(1+x+x^5), x);
@c ===end===
@example
(%i1) integrate_use_rootsof: true$
@group
(%i2) integrate (1/(1+x+x^5), x);
       ____
       \                                         2
(%o2) ( >                                   ((%r1  - 4 %r1 + 5)
       /
       ----
                         3      2
       %r1 in rootsof(%r1  - %r1  + 1, %r1)
                                          2
                     2               log(x  + x + 1)
 log(x - %r1))/(3 %r1  - 2 %r1))/7 - ---------------
                                           14
          2 x + 1
   5 atan(-------)
          sqrt(3)
 + ---------------
      7 sqrt(3)
@end group
@end example

Alternatively the user may compute the roots of the denominator separately,
and then express the integrand in terms of these roots, e.g.,
@code{1/((x - a)*(x - b)*(x - c))} or @code{1/((x^2 - (a+b)*x + a*b)*(x - c))}
if the denominator is a cubic polynomial.
Sometimes this will help Maxima obtain a more useful result.

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{laplace}
@deffn {Function} laplace (@var{expr}, @var{t}, @var{s})

Attempts to compute the Laplace transform of @var{expr} with respect to the 
variable @var{t} and transform parameter @var{s}.  The Laplace
transform of the function @code{f(t)} is the one-sided transform defined by
m4_displaymath(
<<<F(s) = \int_0^{\infty} f(t) e^{-st} dt>>>,
<<<
@example
F(s) = integrate(f(t) * exp(-s*t), t, 0, inf)
@end example
>>>)

where @math{F(s)} is the transform of @math{f(t)}, represented by @var{expr}.

@code{laplace} recognizes in @var{expr} the functions @mrefcomma{delta} @mrefcomma{exp}
@mrefcomma{log} @mrefcomma{sin} @mrefcomma{cos} @mrefcomma{sinh} @mrefcomma{cosh} and @mrefcomma{erf}
as well as @code{derivative}, @mrefcomma{integrate} @mrefcomma{sum} and @mrefdot{ilt} If
@code{laplace} fails to find a transform the function @mref{specint} is called.
@mref{specint} can find the laplace transform for expressions with special
functions like the bessel functions @mrefcomma{bessel_j} @mrefcomma{bessel_i} @dots{}
and can handle the @mref{unit_step} function.  See also @mrefdot{specint}

If @mref{specint} cannot find a solution too, a noun @code{laplace} is returned.

@c REPHRASE THIS
@var{expr} may also be a linear, constant coefficient differential equation in
which case @mref{atvalue} of the dependent variable is used.
@c "used" -- USED HOW ??
The required atvalue may be supplied either before or after the transform is
computed.  Since the initial conditions must be specified at zero, if one has
boundary conditions imposed elsewhere he can impose these on the general
solution and eliminate the constants by solving the general solution
for them and substituting their values back.

@code{laplace} recognizes convolution integrals of the form
m4_displaymath(
<<<\int_0^t f(x) g(t-x) dx>>>,
<<<
@example
integrate (f(x) * g(t - x), x, 0, t)
@end example
>>>)

Other kinds of convolutions are not recognized.

Functional relations must be explicitly represented in @var{expr};
implicit relations, established by @mrefcomma{depends} are not recognized.
That is, if @math{f} depends on @math{x} and @math{y},
@math{f (x, y)} must appear in @var{expr}.

See also @mrefcomma{ilt} the inverse Laplace transform.

Examples:

@c ===beg===
@c laplace (exp (2*t + a) * sin(t) * t, t, s);
@c laplace ('diff (f (x), x), x, s);
@c diff (diff (delta (t), t), t);
@c laplace (%, t, s);
@c assume(a>1)$
@c declare(a, integer)$
@c laplace(gamma_incomplete(a,t),t,s),gamma_expand:true;
@c factor(laplace(gamma_incomplete(1/2,t),t,s));
@c assume(exp(%pi*s)>1, n > 0)$
@c laplace(sum((-1)^n*unit_step(t-n*%pi)*sin(t),n,0,inf),t,s),
@c   simpsum;
@c ===end===
@example
@group
(%i1) laplace (exp (2*t + a) * sin(t) * t, t, s);
                            a
                          %e  (2 s - 4)
(%o1)             -----------------------------
                   4      3       2
                  s  - 8 s  + 26 s  - 40 s + 25
@end group
@group
(%i2) laplace ('diff (f (x), x), x, s);
(%o2)             s laplace(f(x), x, s) - f(0)
@end group
@group
(%i3) diff (diff (delta (t), t), t);
                          2
                         d
(%o3)                    --- (delta(t))
                           2
                         dt
@end group
@group
(%i4) laplace (%, t, s);
                            |
               d            |         2
(%o4)        - -- (delta(t))|      + s  - delta(0) s
               dt           |
                            |t = 0
@end group
(%i5) assume(a>1)$
(%i6) declare(a, integer)$
@group
(%i7) laplace(gamma_incomplete(a,t),t,s),gamma_expand:true;
                                       - a - 1
                  gamma(a)   gamma(a) s
(%o7)             -------- - -----------------
                     s            1     a
                                 (- + 1)
                                  s
@end group
@group
(%i8) factor(laplace(gamma_incomplete(1/2,t),t,s));
                                       s + 1
               sqrt(%pi) (sqrt(s) sqrt(-----) - 1)
                                         s
(%o8)          -----------------------------------
                         3/2      s + 1
                        s    sqrt(-----)
                                    s
@end group
(%i9) assume(exp(%pi*s)>1, n > 0)$
@group
(%i10) laplace(sum((-1)^n*unit_step(t-n*%pi)*sin(t),n,0,inf),t,s),
  simpsum;
                              %pi s
                            %e
(%o10)           ------------------------------
                    %pi s       2     %pi s
                 (%e      - 1) s  + %e      - 1
@end group
@end example

@opencatbox{Categories:}
@category{Laplace transform}
@category{Differential equations}
@closecatbox
@end deffn

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{ldefint}
@deffn {Function} ldefint (@var{expr}, @var{x}, @var{a}, @var{b})

Attempts to compute the definite integral of @var{expr} by using @mref{limit}
to evaluate the indefinite integral of @var{expr} with respect to @var{x}
at the upper limit @var{b} and at the lower limit @var{a}.
If it fails to compute the definite integral,
@code{ldefint} returns an expression containing limits as noun forms.

@code{ldefint} is not called from @mrefcomma{integrate} so executing
@code{ldefint (@var{expr}, @var{x}, @var{a}, @var{b})} may yield a different
result than @code{integrate (@var{expr}, @var{x}, @var{a}, @var{b})}.
@code{ldefint} always uses the same method to evaluate the definite integral,
while @code{integrate} may employ various heuristics and may recognize some
special cases.

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@c UMM, IS THERE SOME TEXT MISSING HERE ???
@c WHAT IS THIS ABOUT EXACTLY ??

@c -----------------------------------------------------------------------------
@anchor{pwilt}
@deffn {Function} pwilt (@var{expr}, @var{s}, @var{t})

Computes the inverse Laplace transform of @var{expr} with
respect to @var{s} and parameter @var{t}. Unlike @mrefcomma{ilt}
@code{pwilt} is able to return piece-wise and periodic functions
and can also handle some cases with polynomials of degree greater than 3
in the denominator.

Two examples where @code{ilt} fails:
@c ===beg===
@c pwilt (exp(-s)*s/(s^3-2*s-s+2), s, t);
@c pwilt ((s^2+2)/(s^2-1), s, t);
@c ===end===
@example
@group
(%i1) pwilt (exp(-s)*s/(s^3-2*s-s+2), s, t);
                        t - 1               - 2 (t - 1)
                      %e      (t - 1)   2 %e
(%o1)   hstep(t - 1) (--------------- - ---------------)
                             3                 9
@end group
@group
(%i2) pwilt ((s^2+2)/(s^2-1), s, t);
                                  t       - t
                              3 %e    3 %e
(%o2)              delta(t) + ----- - -------
                                2        2
@end group
@end example

@opencatbox{Categories:}
@category{Laplace transform}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{potential}
@deffn {Function} potential (@var{givengradient})

The calculation makes use of the global variable @code{potentialzeroloc[0]}
which must be @code{nonlist} or of the form

@example
[indeterminatej=expressionj, indeterminatek=expressionk, ...]
@end example

the former being equivalent to the nonlist expression for all right-hand
sides in the latter.  The indicated right-hand sides are used as the
lower limit of integration.  The success of the integrations may
depend upon their values and order.  @code{potentialzeroloc} is initially set
to 0.
@end deffn

@c -----------------------------------------------------------------------------
@anchor{prefer_d}
@defvr {Option variable} prefer_d
Default value: @code{false}

When @code{prefer_d} is @code{true}, @mref{specint} will prefer to
express solutions using @mref{parabolic_cylinder_d} rather than
hypergeometric functions.

In the example below, the solution contains @mref{parabolic_cylinder_d}
when @code{prefer_d} is @code{true}. 

@c ===beg===
@c assume(s>0);
@c factor(ex:specint(%e^-(t^2/8)*exp(-s*t),t));
@c specint(ex,t),prefer_d=true;
@c ===end===
@example
@group
(%i1) assume(s>0);
(%o1)                        [s > 0]
@end group
@group
(%i2) factor(ex:specint(%e^-(t^2/8)*exp(-s*t),t));
                        2
                     2 s
(%o2)    - sqrt(2) %e     sqrt(%pi) (erf(sqrt(2) s) - 1)
@end group
@group
(%i3) specint(ex,t),prefer_d=true;
                             2
                          2 s
(%o3) specint(- sqrt(2) %e     sqrt(%pi) erf(sqrt(2) s), t)
                                                  2
                                               2 s
                           + specint(sqrt(2) %e     sqrt(%pi), t)
@end group
@end example

@opencatbox{Categories:}
@category{Laplace transform}
@category{Special functions}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{residue}
@deffn {Function} residue (@var{expr}, @var{z}, @var{z_0})

Computes the residue in the complex plane of the expression @var{expr} when the
variable @var{z} assumes the value @var{z_0}.  The residue is the coefficient of
@code{(@var{z} - @var{z_0})^(-1)} in the Laurent series for @var{expr}.

@c ===beg===
@c residue (s/(s**2+a**2), s, a*%i);
@c residue (sin(a*x)/x**4, x, 0);
@c ===end===
@example
@group
(%i1) residue (s/(s**2+a**2), s, a*%i);
                                1
(%o1)                           -
                                2
@end group
@group
(%i2) residue (sin(a*x)/x**4, x, 0);
                                 3
                                a
(%o2)                         - --
                                6
@end group
@end example

@opencatbox{Categories:}
@category{Integral calculus}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{risch}
@deffn {Function} risch (@var{expr}, @var{x})

Integrates @var{expr} with respect to @var{x} using the
transcendental case of the Risch algorithm.  (The algebraic case of
the Risch algorithm has not been implemented.)  This currently
handles the cases of nested exponentials and logarithms which the main
part of @code{integrate} can't do.  @mref{integrate} will automatically apply
@code{risch} if given these cases.

@code{erfflag}, if @code{false}, prevents @code{risch} from introducing the
@code{erf} function in the answer if there were none in the integrand to begin
with.

@c ===beg===
@c risch (x^2*erf(x), x);
@c diff(%, x), ratsimp;
@c ===end===
@example
@group
(%i1) risch (x^2*erf(x), x);
                             2
             3            - x              2
        %pi x  erf(x) + %e     (sqrt(%pi) x  + sqrt(%pi))
(%o1)   -------------------------------------------------
                              3 %pi
@end group
@group
(%i2) diff(%, x), ratsimp;
                             2
(%o2)                       x  erf(x)
@end group
@end example

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@anchor{specint}
@deffn {Function} specint (exp(- s*@var{t}) * @var{expr}, @var{t})

Compute the Laplace transform of @var{expr} with respect to the variable @var{t}.
The integrand @var{expr} may contain special functions.   The
parameter @var{s} maybe be named something else; it is determined
automatically, as the examples below show where @var{p} is used in
some places.

The following special functions are handled by @code{specint}: incomplete gamma 
function, error functions (but not the error function @code{erfi}, it is easy to 
transform @code{erfi} e.g. to the error function @code{erf}), exponential 
integrals, bessel functions (including products of bessel functions), hankel 
functions, hermite and the laguerre polynomials.

Furthermore, @code{specint} can handle the hypergeometric function 
@code{%f[p,q]([],[],z)}, the Whittaker function of the first kind 
@code{%m[u,k](z)} and of the second kind @code{%w[u,k](z)}.

The result may be in terms of special functions and can include unsimplified 
hypergeometric functions.  If variable @mref{prefer_d} is @code{true}
then the @mref{parabolic_cylinder_d} function may be used in the result
in preference to hypergeometric functions.

When @mref{laplace} fails to find a Laplace transform, @code{specint} is called. 
Because @mref{laplace} knows more general rules for Laplace transforms, it is 
preferable to use @mref{laplace} and not @code{specint}.

@code{demo("hypgeo")} displays several examples of Laplace transforms computed by 
@code{specint}.

Examples:
@c ===beg===
@c assume (p > 0, a > 0)$
@c specint (t^(1/2) * exp(-a*t/4) * exp(-p*t), t);
@c specint (t^(1/2) * bessel_j(1, 2 * a^(1/2) * t^(1/2)) 
@c               * exp(-p*t), t);
@c ===end===
@example
(%i1) assume (p > 0, a > 0)$
@group
(%i2) specint (t^(1/2) * exp(-a*t/4) * exp(-p*t), t);
                           sqrt(%pi)
(%o2)                     ------------
                                 a 3/2
                          2 (p + -)
                                 4
@end group
@group
(%i3) specint (t^(1/2) * bessel_j(1, 2 * a^(1/2) * t^(1/2))
              * exp(-p*t), t);
                           - a/p
                         %e      sqrt(a)
(%o3)                    ---------------
                                2
                               p
@end group
@end example

Examples for exponential integrals:

@example
(%i4) assume(s>0,a>0,s-a>0)$
(%i5) ratsimp(specint(%e^(a*t)
                      *(log(a)+expintegral_e1(a*t))*%e^(-s*t),t));
                             log(s)
(%o5)                        ------
                             s - a
(%i6) logarc:true$

(%i7) gamma_expand:true$

radcan(specint((cos(t)*expintegral_si(t)
                     -sin(t)*expintegral_ci(t))*%e^(-s*t),t));
                             log(s)
(%o8)                        ------
                              2
                             s  + 1
ratsimp(specint((2*t*log(a)+2/a*sin(a*t)
                      -2*t*expintegral_ci(a*t))*%e^(-s*t),t));
                               2    2
                          log(s  + a )
(%o9)                     ------------
                                2
                               s
@end example

Results when using the expansion of @mref{gamma_incomplete} and when changing 
the representation to @mref{expintegral_e1}:

@example
(%i10) assume(s>0)$
(%i11) specint(1/sqrt(%pi*t)*unit_step(t-k)*%e^(-s*t),t);
                                            1
                            gamma_incomplete(-, k s)
                                            2
(%o11)                      ------------------------
                               sqrt(%pi) sqrt(s)

(%i12) gamma_expand:true$
(%i13) specint(1/sqrt(%pi*t)*unit_step(t-k)*%e^(-s*t),t);
                              erfc(sqrt(k) sqrt(s))
(%o13)                        ---------------------
                                     sqrt(s)

(%i14) expintrep:expintegral_e1$
(%i15) ratsimp(specint(1/(t+a)^2*%e^(-s*t),t));
                              a s
                        a s %e    expintegral_e1(a s) - 1
(%o15)                - ---------------------------------
                                        a
@end example

@opencatbox{Categories:}
@category{Laplace transform}
@closecatbox
@end deffn

@c NEEDS EXPANSION, CLARIFICATION, AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{tldefint}
@deffn {Function} tldefint (@var{expr}, @var{x}, @var{a}, @var{b})

Equivalent to @code{ldefint} with @code{tlimswitch} set to @code{true}.

@opencatbox{Categories:}
@category{Integral calculus}
@closecatbox
@end deffn

@footnotestyle end

@c -----------------------------------------------------------------------------
@node Introduction to QUADPACK, Functions and Variables for QUADPACK, Functions and Variables for Integration, Integration
@section Introduction to QUADPACK
@c -----------------------------------------------------------------------------

@c FOLLOWING TEXT ADAPTED WITH HEAVY MODIFICATION FROM https://www.netlib.org/slatec/src/qpdoc.f

QUADPACK is a collection of functions for the numerical
computation of one-dimensional definite integrals.
It originated from a joint project of
R. Piessens @footnote{Applied Mathematics and Programming Division, K.U. Leuven},
E. de Doncker @footnote{Applied Mathematics and Programming Division, K.U. Leuven},
C. Ueberhuber @footnote{Institut f@"ur Mathematik, T.U. Wien},
and D. Kahaner @footnote{National Bureau of Standards, Washington, D.C., U.S.A}.

The QUADPACK library included in Maxima is an automatic translation (via the
program @code{f2cl}) of the Fortran source code of QUADPACK as it appears in
the SLATEC Common Mathematical Library, Version 4.1 @footnote{@url{https://www.netlib.org/slatec}}.
The SLATEC library is dated July 1993, but the QUADPACK functions
were written some years before.
There is another version of QUADPACK at Netlib @footnote{@url{https://www.netlib.org/quadpack}};
it is not clear how that version differs from the SLATEC version.

The QUADPACK functions included in Maxima are all automatic, in the sense that
these functions attempt to compute a result to a specified accuracy, requiring
an unspecified number of function evaluations.  Maxima's Lisp translation of
QUADPACK also includes some non-automatic functions, but they are not exposed
at the Maxima level.

Further information about QUADPACK can be found in the QUADPACK book
@footnote{R. Piessens, E. de Doncker-Kapenga, C.W. Uberhuber, and D.K. Kahaner.
@i{QUADPACK: A Subroutine Package for Automatic Integration.}
Berlin: Springer-Verlag, 1983, ISBN 0387125531.}.

@c -----------------------------------------------------------------------------
@subsection Overview
@c -----------------------------------------------------------------------------

@table @code
@item @mref{quad_qag}
Integration of a general function over a finite interval.
@mref{quad_qag} implements a simple globally adaptive integrator using the
strategy of Aind (Piessens, 1973).
The caller may choose among 6 pairs of Gauss-Kronrod quadrature
formulae for the rule evaluation component.
The high-degree rules are suitable for strongly oscillating integrands.

@item @mref{quad_qags}
Integration of a general function over a finite interval.
@mref{quad_qags} implements globally adaptive interval subdivision with
extrapolation (de Doncker, 1978) by the Epsilon algorithm (Wynn, 1956).

@item @mref{quad_qagi}
Integration of a general function over an infinite or semi-infinite interval.
The interval is mapped onto a finite interval and
then the same strategy as in @code{quad_qags} is applied.

@item @mref{quad_qawo}

Integration of 
m4_math(<<<\cos(\omega x) f(x)>>>,<<<@math{cos(omega x) f(x)}>>>) 
or 
m4_math(<<<\sin(\omega x) f(x)>>>,<<<@math{sin(omega x) f(x)}>>>) 
over a
finite interval, where 
m4_math(\omega, @math{omega}) 
is a constant.
The rule evaluation component is based on the modified Clenshaw-Curtis
technique.  @mref{quad_qawo} applies adaptive subdivision with extrapolation,
similar to @mrefdot{quad_qags}

@item @mref{quad_qawf}
Calculates a Fourier cosine or Fourier sine transform on a semi-infinite
interval.  The same approach as in @mref{quad_qawo} is applied on successive
finite intervals, and convergence acceleration by means of the Epsilon algorithm
(Wynn, 1956) is applied to the series of the integral contributions.

@item @mref{quad_qaws}
Integration of 
m4_math(w(x)f(x), @math{w(x) f(x)}) 
over a finite interval @math{[a, b]}, where
@math{w} is a function of the form 
m4_math((x-a)^\alpha (b-x)^\beta v(x), @math{(x - a)^alpha (b -
x)^beta v(x)}) 
and
@math{v(x)} is 1 or 
m4_math(\log(x-a), @math{log(x - a)}) 
or 
m4_math(\log(b-x), @math{log(b - x)}) 
or
m4_mathcomma(\log(x-a)\log(b-x), @math{log(x - a) log(b - x)}) 
and 
m4_math(\alpha > -1, @math{alpha > -1}) 
and 
m4_mathdot(\beta > -1, @math{beta > -1})

A globally adaptive subdivision strategy is applied, with modified
Clenshaw-Curtis integration on the subintervals which contain @math{a}
or @math{b}.

@item @mref{quad_qawc}
Computes the Cauchy principal value of @math{f(x)/(x - c)} over a finite
interval @math{(a, b)} and specified @math{c}.
The strategy is globally adaptive, and modified
Clenshaw-Curtis integration is used on the subranges
which contain the point @math{x = c}.

@item @mref{quad_qagp}
Basically the same as @mref{quad_qags} but points of singularity or
discontinuity of the integrand must be supplied.  This makes it easier
for the integrator to produce a good solution.
@end table


@opencatbox{Categories:}
@category{Integral calculus}
@category{Numerical methods}
@category{Share packages}
@category{Package quadpack}
@closecatbox

@c -----------------------------------------------------------------------------
@node Functions and Variables for QUADPACK, , Introduction to QUADPACK, Integration
@section Functions and Variables for QUADPACK
@c -----------------------------------------------------------------------------

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qag}
@deffn  {Function} quad_qag @
@fname{quad_qag} (@var{f(x)}, @var{x}, @var{a}, @var{b}, @var{key}, [@var{epsrel}, @var{epsabs}, @var{limit}]) @
@fname{quad_qag} (@var{f}, @var{x}, @var{a}, @var{b}, @var{key}, [@var{epsrel}, @var{epsabs}, @var{limit}])

Integration of a general function over a finite interval.  @code{quad_qag}
implements a simple globally adaptive integrator using the strategy of Aind
(Piessens, 1973).  The caller may choose among 6 pairs of Gauss-Kronrod
quadrature formulae for the rule evaluation component.  The high-degree rules
are suitable for strongly oscillating integrands.

@code{quad_qag} computes the integral

m4_displaymath(
<<<\int_a^b f(x)\, dx>>>,
@math{integrate (f(x), x, a, b)}>>>)

The function to be integrated is @math{f(x)}, with dependent
variable @math{x}, and the function is to be integrated between the
limits @math{a} and @math{b}.  @var{key} is the integrator to be used
and should be an integer between 1 and 6, inclusive.  The value of
@var{key} selects the order of the Gauss-Kronrod integration rule.
High-order rules are suitable for strongly oscillating integrands.

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

The numerical integration is done adaptively by subdividing the
integration region into sub-intervals until the desired accuracy is
achieved.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit} is the
maximum number of subintervals to use.  Default is 200.
@end table

@code{quad_qag} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
if no problems were encountered;
@item 1
if too many sub-intervals were done;
@item 2
if excessive roundoff error is detected;
@item 3
if extremely bad integrand behavior occurs;
@item 6
if the input is invalid.

@end table

@c NEED CROSS REFS HERE -- EITHER CROSS REF A QUADPACK OVERVIEW, OR CROSS REF EACH OF THE quad_* FUNCTIONS

Examples:

@c ===beg===
@c quad_qag (x^(1/2)*log(1/x), x, 0, 1, 3, 'epsrel=5d-8);
@c integrate (x^(1/2)*log(1/x), x, 0, 1);
@c ===end===
@example
@group
(%i1) quad_qag (x^(1/2)*log(1/x), x, 0, 1, 3, 'epsrel=5d-8);
(%o1)  [0.44444444445742953, 8.737223570614865e-9, 899, 0]
@end group
@group
(%i2) integrate (x^(1/2)*log(1/x), x, 0, 1);
                                4
(%o2)                           -
                                9
@end group
@end example

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qags}
@deffn  {Function} quad_qags @
@fname{quad_qags} (@var{f(x)}, @var{x}, @var{a}, @var{b}, [@var{epsrel}, @var{epsabs}, @var{limit}]) @
@fname{quad_qags} (@var{f}, @var{x}, @var{a}, @var{b}, [@var{epsrel}, @var{epsabs}, @var{limit}])

Integration of a general function over a finite interval.
@code{quad_qags} implements globally adaptive interval subdivision with
extrapolation (de Doncker, 1978) by the Epsilon algorithm (Wynn, 1956).

@code{quad_qags} computes the integral

m4_displaymath(
<<<\int_a^b f(x)\, dx>>>,
@math{integrate (f(x), x, a, b)}>>>)

The function to be integrated is @math{f(x)}, with
dependent variable @math{x}, and the function is to be integrated
between the limits @math{a} and @math{b}.

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit} is the
maximum number of subintervals to use.  Default is 200.
@end table

@code{quad_qags} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 4
failed to converge
@item 5
integral is probably divergent or slowly convergent
@item 6
if the input is invalid.
@end table

@c NEED CROSS REFS HERE -- EITHER CROSS REF A QUADPACK OVERVIEW, OR CROSS REF EACH OF THE quad_* FUNCTIONS

Examples:

@c ===beg===
@c quad_qags (x^(1/2)*log(1/x), x, 0, 1, 'epsrel=1d-10);
@c ===end===
@example
@group
(%i1) quad_qags (x^(1/2)*log(1/x), x, 0, 1, 'epsrel=1d-10);
(%o1) [0.44444444444444475, 1.1102230246251565e-15, 315, 0]
@end group
@end example

Note that @code{quad_qags} is more accurate and efficient than @code{quad_qag} for this integrand.

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qagi}
@deffn  {Function} quad_qagi @
@fname{quad_qagi} (@var{f(x)}, @var{x}, @var{a}, @var{b}, [@var{epsrel}, @var{epsabs}, @var{limit}]) @
@fname{quad_qagi} (@var{f}, @var{x}, @var{a}, @var{b}, [@var{epsrel}, @var{epsabs}, @var{limit}])

Integration of a general function over an infinite or semi-infinite interval.
The interval is mapped onto a finite interval and
then the same strategy as in @code{quad_qags} is applied.

@code{quad_qagi} evaluates one of the following integrals

m4_displaymath(
<<<\int_a^\infty f(x) \, dx>>>,
<<<@math{integrate (f(x), x, a, inf)}>>>)

m4_displaymath(
<<<\int_\infty^a f(x) \, dx>>>,
<<<@math{integrate (f(x), x, minf, a)}>>>)

m4_displaymath(
<<<\int_{-\infty}^\infty f(x) \, dx>>>,
<<<@math{integrate (f(x), x, minf, inf)}>>>)

using the Quadpack QAGI routine.  The function to be integrated is
@math{f(x)}, with dependent variable @math{x}, and the function is to
be integrated over an infinite range.

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

One of the limits of integration must be infinity.  If not, then
@code{quad_qagi} will just return the noun form.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit} is the
maximum number of subintervals to use.  Default is 200.
@end table

@code{quad_qagi} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 4
failed to converge
@item 5
integral is probably divergent or slowly convergent
@item 6
if the input is invalid.

@end table

@c NEED CROSS REFS HERE -- EITHER CROSS REF A QUADPACK OVERVIEW, OR CROSS REF EACH OF THE quad_* FUNCTIONS

Examples:

@c ===beg===
@c quad_qagi (x^2*exp(-4*x), x, 0, inf, 'epsrel=1d-8);
@c integrate (x^2*exp(-4*x), x, 0, inf);
@c ===end===
@example
@group
(%i1) quad_qagi (x^2*exp(-4*x), x, 0, inf, 'epsrel=1d-8);
(%o1)       [0.03125, 2.9591610299500215e-11, 105, 0]
@end group
@group
(%i2) integrate (x^2*exp(-4*x), x, 0, inf);
                               1
(%o2)                          --
                               32
@end group
@end example

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qawc}
@deffn  {Function} quad_qawc @
@fname{quad_qawc} (@var{f(x)}, @var{x}, @var{c}, @var{a}, @var{b}, [@var{epsrel}, @var{epsabs}, @var{limit}]) @
@fname{quad_qawc} (@var{f}, @var{x}, @var{c}, @var{a}, @var{b}, [@var{epsrel}, @var{epsabs}, @var{limit}])

Computes the Cauchy principal value of @math{f(x)/(x - c)} over a finite
interval.  The strategy is globally adaptive, and modified
Clenshaw-Curtis integration is used on the subranges
which contain the point @math{x = c}.

@code{quad_qawc} computes the Cauchy principal value of

m4_displaymath(
<<<\int_{a}^{b}{{{f\left(x\right)}\over{x-c}}\>dx}>>>,
<<<@math{integrate (f(x)/(x - c), x, a, b)}>>>)

using the Quadpack QAWC routine.  The function to be integrated is
@math{f(x)/(x-c)}, with dependent variable @math{x}, and the
function is to be integrated over the interval @math{a} to @math{b}.

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit} is the
maximum number of subintervals to use.  Default is 200.
@end table

@code{quad_qawc} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 6
if the input is invalid.

@end table

Examples:

@c ===beg===
@c quad_qawc (2^(-5)*((x-1)^2+4^(-5))^(-1), x, 2, 0, 5, 'epsrel=1d-7);
@c integrate (2^(-alpha)*(((x-1)^2 + 4^(-alpha))*(x-2))^(-1), x, 0, 5);
@c ev (%, alpha=5, numer);
@c ===end===
@example
@group
(%i1) quad_qawc (2^(-5)*((x-1)^2+4^(-5))^(-1), x, 2, 0, 5, 'epsrel=1d-7);
(%o1) [- 3.130120337415925, 1.3068301402495579e-8, 495, 0]
@end group
@group
(%i2) integrate (2^(-alpha)*(((x-1)^2 + 4^(-alpha))*(x-2))^(-1), x, 0, 5);
Principal Value
          2 alpha - 1      2 alpha + 4
         2            log(2            + 1)
(%o2) (- ----------------------------------
                     2 alpha
                    2        + 1
    3 alpha       alpha + 2     2 alpha - 1      2 alpha
   2        atan(2         )   2            log(2        + 1)
 - ------------------------- + ------------------------------
          2 alpha                        2 alpha
         2        + 1                   2        + 1
    3 alpha + 1       alpha     3 alpha       alpha
   2            atan(2     )   2        atan(2     )
 - ------------------------- + ---------------------
          2 alpha                   2 alpha
         2        + 1              2        + 1
    2 alpha           2 alpha
   2        log(3)   2        log(2)   alpha
 + --------------- - ---------------)/2
     2 alpha           2 alpha
    2        + 1      2        + 1
@end group
@group
(%i3) ev (%, alpha=5, numer);
(%o3)                 - 3.1301203374159177
@end group
@end example

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qawf}
@deffn  {Function} quad_qawf @
@fname{quad_qawf} (@var{f(x)}, @var{x}, @var{a}, @var{omega}, @var{trig}, [@var{epsabs}, @var{limit}, @var{maxp1}, @var{limlst}]) @
@fname{quad_qawf} (@var{f}, @var{x}, @var{a}, @var{omega}, @var{trig}, [@var{epsabs}, @var{limit}, @var{maxp1}, @var{limlst}])

Calculates a Fourier cosine or Fourier sine transform on a semi-infinite
interval using the Quadpack QAWF function.  The same approach as in
@code{quad_qawo} is applied on successive finite intervals, and convergence
acceleration by means of the Epsilon algorithm (Wynn, 1956) is applied to the
series of the integral contributions.

@code{quad_qawf} computes the integral

m4_displaymath(
<<<\int_a^\infty f(x) \, w(x) \, dx>>>,
<<<@math{integrate (f(x)*w(x), x, a, inf)}>>>)

The weight function @math{w} is selected by @var{trig}:

@table @code
@item cos
m4_math(w(x) = \cos\omega x, @math{w(x) = cos (omega x)})
@item sin
m4_math(w(x) = \sin\omega x, @math{w(x) = sin (omega x)})
@end table

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsabs
Desired absolute error of approximation.  Default is 1d-10.
@item limit
Size of internal work array.  (@var{limit} - @var{limlst})/2 is the
maximum number of subintervals to use.  Default is 200.
@item maxp1
Maximum number of Chebyshev moments.  Must be greater than 0.  Default
is 100.
@item limlst
Upper bound on the number of cycles.  Must be greater than or equal to
3.  Default is 10.
@end table

@code{quad_qawf} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 6
if the input is invalid.

@end table

Examples:

@c ===beg===
@c quad_qawf (exp(-x^2), x, 0, 1, 'cos, 'epsabs=1d-9);
@c integrate (exp(-x^2)*cos(x), x, 0, inf);
@c ev (%, numer);
@c ===end===
@example
@group
(%i1) quad_qawf (exp(-x^2), x, 0, 1, 'cos, 'epsabs=1d-9);
(%o1)  [0.6901942235215714, 2.848463002545743e-11, 215, 0]
@end group
@group
(%i2) integrate (exp(-x^2)*cos(x), x, 0, inf);
                          - 1/4
                        %e      sqrt(%pi)
(%o2)                   -----------------
                                2
@end group
@group
(%i3) ev (%, numer);
(%o3)                  0.6901942235215714
@end group
@end example

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qawo}
@deffn  {Function} quad_qawo @
@fname{quad_qawo} (@var{f(x)}, @var{x}, @var{a}, @var{b}, @var{omega}, @var{trig}, [@var{epsrel}, @var{epsabs}, @var{limit}, @var{maxp1}, @var{limlst}]) @
@fname{quad_qawo} (@var{f}, @var{x}, @var{a}, @var{b}, @var{omega}, @var{trig}, [@var{epsrel}, @var{epsabs}, @var{limit}, @var{maxp1}, @var{limlst}])

Integration of 
m4_math(<<<\cos(\omega x) f(x)>>>,<<<@math{cos (omega x) f(x)}>>>) 
or 
m4_math(<<<\sin(\omega x)>>>, <<<@math{sin (omega x) f(x)}>>>) 
over a finite interval,
where 
m4_math(\omega, @math{omega}) 
is a constant.
The rule evaluation component is based on the modified
Clenshaw-Curtis technique.  @code{quad_qawo} applies adaptive subdivision with
extrapolation, similar to @code{quad_qags}.

@code{quad_qawo} computes the integral using the Quadpack QAWO
routine:

m4_displaymath(
<<<\int_a^b f(x) \, w(x) \, dx>>>,
<<<@math{integrate (f(x)*w(x), x, a, b)}>>>)

The weight function @math{w} is selected by @var{trig}:

@table @code
@item cos
m4_math(w(x) = \cos\omega x, @math{w(x) = cos (omega x)})
@item sin
m4_math(w(x) = \sin\omega x, @math{w(x) = sin (omega x)})
@end table

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit}/2 is the
maximum number of subintervals to use.  Default is 200.
@item maxp1
Maximum number of Chebyshev moments.  Must be greater than 0.  Default
is 100.
@item limlst
Upper bound on the number of cycles.  Must be greater than or equal to
3.  Default is 10.
@end table

@code{quad_qawo} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 6
if the input is invalid.

@end table

Examples:

@c ===beg===
@c quad_qawo (x^(-1/2)*exp(-2^(-2)*x), x, 1d-8, 20*2^2, 1, cos);
@c rectform (integrate (x^(-1/2)*exp(-2^(-alpha)*x) * cos(x), x, 0, inf));
@c ev (%, alpha=2, numer);
@c ===end===
@example
@group
(%i1) quad_qawo (x^(-1/2)*exp(-2^(-2)*x), x, 1d-8, 20*2^2, 1, cos);
(%o1) [1.3760433898776214, 4.7271075942489915e-11, 765, 0]
@end group
@group
(%i2) rectform (integrate (x^(-1/2)*exp(-2^(-alpha)*x) * cos(x), x, 0, inf));
         alpha + 1
         --------- - 1
             2                    2 alpha
        2              sqrt(sqrt(2        + 1) %pi + %pi)
(%o2)   -------------------------------------------------
                             2 alpha
                       sqrt(2        + 1)
@end group
@group
(%i3) ev (%, alpha=2, numer);
(%o3)                   1.376043390090716
@end group
@end example

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qaws}
@deffn  {Function} quad_qaws @
@fname{quad_qaws} (@var{f(x)}, @var{x}, @var{a}, @var{b}, @var{alpha}, @var{beta}, @var{wfun}, [@var{epsrel}, @var{epsabs}, @var{limit}]) @
@fname{quad_qaws} (@var{f}, @var{x}, @var{a}, @var{b}, @var{alpha}, @var{beta}, @var{wfun}, [@var{epsrel}, @var{epsabs}, @var{limit}])

Integration of @math{w(x) f(x)} over a finite interval, where @math{w(x)} is a
certain algebraic or logarithmic function.  A globally adaptive subdivision
strategy is applied, with modified Clenshaw-Curtis integration on the
subintervals which contain the endpoints of the interval of integration.

@code{quad_qaws} computes the integral using the Quadpack QAWS routine:

m4_displaymath(
<<<\int_a^b f(x) \, w(x) \, dx>>>,
<<<@math{integrate (f(x)*w(x), x, a, b)}>>>)

The weight function @math{w} is selected by @var{wfun}:

@table @code
@item 1
m4_math(w(x) = (x - a)^\alpha (b - x)^\beta, @math{w(x) = (x - a)^alpha (b - x)^beta})
@item 2
m4_math(w(x) = (x - a)^\alpha (b - x)^\beta \log(x - a),
@math{w(x) = (x - a)^alpha (b - x)^beta log(x - a)})
@item 3
m4_math(w(x) = (x - a)^\alpha (b - x)^\beta \log(b - x), @math{w(x) = (x - a)^alpha (b - x)^beta log(b - x)})
@item 4
m4_math(w(x) = (x - a)^\alpha (b - x)^\beta \log(x - a) \log(b - x),
@math{w(x) = (x - a)^alpha (b - x)^beta log(x - a) log(b - x)})
@end table

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit}is the
maximum number of subintervals to use.  Default is 200.
@end table

@code{quad_qaws} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 6
if the input is invalid.

@end table

Examples:

@c ===beg===
@c quad_qaws (1/(x+1+2^(-4)), x, -1, 1, -0.5, -0.5, 1, 'epsabs=1d-9);
@c integrate ((1-x*x)^(-1/2)/(x+1+2^(-alpha)), x, -1, 1);
@c ev (%, alpha=4, numer);
@c ===end===
@example
@group
(%i1) quad_qaws (1/(x+1+2^(-4)), x, -1, 1, -0.5, -0.5, 1, 'epsabs=1d-9);
(%o1)  [8.750097361672843, 1.2761903591126173e-8, 130, 0]
@end group
@group
(%i2) integrate ((1-x*x)^(-1/2)/(x+1+2^(-alpha)), x, -1, 1);
                            alpha
                           2      %pi
(%o2)                 --------------------
                            alpha + 1
                      sqrt(2          + 1)
@end group
@group
(%i3) ev (%, alpha=4, numer);
(%o3)                   8.75009736167283
@end group
@end example

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c THERE ARE OPTIONAL ARGUMENTS WHICH MAKES LISTING THE VARIANTS A LITTLE TEDIOUS
@c NEED A MORE CONVENIENT (AND NONAMBIGUOUS) NOTATION FOR OPTIONAL ARGUMENTS

@c -----------------------------------------------------------------------------
@anchor{quad_qagp}
@deffn  {Function} quad_qagp @
@fname{quad_qagp} (@var{f(x)}, @var{x}, @var{a}, @var{b}, @var{points}, [@var{epsrel}, @var{epsabs}, @var{limit}]) @
@fname{quad_qagp} (@var{f}, @var{x}, @var{a}, @var{b}, @var{points}, [@var{epsrel}, @var{epsabs}, @var{limit}])

Integration of a general function over a finite interval.
@code{quad_qagp} implements globally adaptive interval subdivision with
extrapolation (de Doncker, 1978) by the Epsilon algorithm (Wynn, 1956).

@code{quad_qagp} computes the integral

m4_displaymath(
<<<\int_a^b f(x) \, dx>>>,
<<<@math{integrate (f(x), x, a, b)}>>>)

The function to be integrated is @math{f(x)}, with
dependent variable @math{x}, and the function is to be integrated
between the limits @math{a} and @math{b}.

The integrand may be specified as the name of a Maxima or Lisp function or
operator, a Maxima lambda expression, or a general Maxima expression.

To help the integrator, the user must supply a list of points where
the integrand is singular or discontinuous.  The list is provided by
@var{points}.  It may be an empty list.  The elements of the list must
be between @var{a} and @var{b}, exclusive.  An error is thrown if
there are elements out of range.  The list points may be in any order. 

The keyword arguments are optional and may be specified in any order.
They all take the form @code{key=val}.  The keyword arguments are:

@table @code
@item epsrel
Desired relative error of approximation.  Default is 1d-8.
@item epsabs
Desired absolute error of approximation.  Default is 0.
@item limit
Size of internal work array.  @var{limit} is the
maximum number of subintervals to use.  Default is 200.
@end table

@code{quad_qagp} returns a list of four elements:

@itemize
@item
an approximation to the integral,
@item
the estimated absolute error of the approximation,
@item
the number integrand evaluations,
@item
an error code.
@end itemize

The error code (fourth element of the return value) can have the values:

@table @code
@item 0
no problems were encountered;
@item 1
too many sub-intervals were done;
@item 2
excessive roundoff error is detected;
@item 3
extremely bad integrand behavior occurs;
@item 4
failed to converge
@item 5
integral is probably divergent or slowly convergent
@item 6
if the input is invalid.
@end table

@c NEED CROSS REFS HERE -- EITHER CROSS REF A QUADPACK OVERVIEW, OR CROSS REF EACH OF THE quad_* FUNCTIONS

Examples:

@c ===beg===
@c quad_qagp(x^3*log(abs((x^2-1)*(x^2-2))),x,0,3,[1,sqrt(2)]);
@c quad_qags(x^3*log(abs((x^2-1)*(x^2-2))), x, 0, 3);
@c ===end===
@example
@group
(%i1) quad_qagp(x^3*log(abs((x^2-1)*(x^2-2))),x,0,3,[1,sqrt(2)]);
(%o1) [52.740748383471434, 2.6247632689546663e-7, 1029, 0]
@end group
@group
(%i2) quad_qags(x^3*log(abs((x^2-1)*(x^2-2))), x, 0, 3);
(%o2)  [52.74074847951494, 4.088443219529836e-7, 1869, 0]
@end group
@end example

The integrand has singularities at @code{1} and @code{sqrt(2)} so we supply
these points to @code{quad_qagp}.  We also note that @code{quad_qagp} is
more accurate and more efficient that @mrefdot{quad_qags}

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{quad_control}
@deffn  {Function} quad_control (@var{parameter}, [@var{value}])

Control error handling for quadpack.  The parameter should be one of
the following symbols:

@table @code
@item current_error
The current error number
@item control
Controls if messages are printed or not.  If it is set to zero or
less, messages are suppressed.
@item max_message
The maximum number of times any message is to be printed.
@end table

If @var{value} is not given, then the current value of the
@var{parameter} is returned.  If @var{value} is given, the value of
@var{parameter} is set to the given value.

@opencatbox{Categories:}
@category{Numerical methods}
@category{Package quadpack}
@closecatbox
@end deffn

