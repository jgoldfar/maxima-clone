@menu
* Introduction to cobyla::
* Functions and Variables for cobyla::
* Examples for cobyla::  
@end menu

@node Introduction to cobyla, Functions and Variables for cobyla, Package cobyla, Package cobyla
@section Introduction to cobyla

@code{fmin_cobyla} is a Common Lisp translation (via @code{f2cl}) of the
Fortran constrained optimization routine COBYLA by Powell[1][2][3].  

COBYLA minimizes an objective function @math{F(X)} subject to @math{M} inequality
constraints of the form 
m4_math(g(X) \ge 0, @math{g(X) >= 0}) 
on @math{X},
where @math{X} is a vector of variables that has @math{N} components.

Equality constraints @math{g(X) = 0} can often be implemented by a pair of inequality 
constraints 
m4_math(g(X) \ge 0, @math{g(X)>=0}) 
and 
m4_mathdot(-g(X) \ge 0, @math{-g(X)>= 0})  
Maxima's interface to COBYLA
allows equality constraints and internally converts the equality
constraints to a pair of inequality constraints.

The algorithm employs linear approximations to the
objective and constraint functions, the approximations being formed by
linear interpolation at @math{N+1} points in the space of the variables.
The interpolation points are regarded as vertices of a simplex. The
parameter @var{RHO} controls the size of the simplex and it is reduced
automatically from @var{RHOBEG} to @var{RHOEND}. For each @var{RHO} the subroutine tries
to achieve a good vector of variables for the current size, and then
@var{RHO} is reduced until the value @var{RHOEND} is reached. Therefore, @var{RHOBEG} and
@var{RHOEND} should be set to reasonable initial changes to and the required   
accuracy in the variables respectively, but this accuracy should be
viewed as a subject for experimentation because it is not guaranteed.
The routine treats each constraint individually when calculating
a change to the variables, rather than lumping the constraints together
into a single penalty function. The name of the subroutine is derived
from the phrase Constrained Optimization BY Linear Approximations.


References:

[1] Fortran Code is from @url{http://plato.asu.edu/sub/nlores.html#general}

[2] M. J. D. Powell, "A direct search optimization method that models the objective and constraint functions by linear interpolation," in Advances in Optimization and Numerical Analysis, eds. S. Gomez and J.-P. Hennart (Kluwer Academic: Dordrecht, 1994), p. 51-67. 

[3] M. J. D. Powell, "Direct search algorithms for optimization calculations," Acta Numerica 7, 287-336 (1998).  Also available as University of Cambridge, Department of Applied Mathematics and Theoretical Physics,  Numerical Analysis Group, Report NA1998/04 from @url{https://web.archive.org/web/20160607190705/http://www.damtp.cam.ac.uk:80/user/na/reports.html}

@opencatbox{Categories:}
@category{Numerical methods} 
@category{Optimization}
@category{Share packages}
@category{Package cobyla}
@closecatbox

@node Functions and Variables for cobyla, Examples for cobyla, Introduction to cobyla, Package cobyla
@section Functions and Variables for cobyla

@anchor{fmin_cobyla}
@deffn {Function} fmin_cobyla @
@fname{fmin_cobyla} (@var{F}, @var{X}, @var{Y}) @
@fname{fmin_cobyla} (@var{F}, @var{X}, @var{Y}, optional_args)

Returns an approximate minimum of the expression @var{F} with respect
to the variables @var{X}, subject to an optional set of constraints.
@var{Y} is a list of initial guesses for @var{X}.

@var{F} must be ordinary expressions, not names of functions or lambda expressions.

@code{optional_args} represents additional arguments,
specified as @code{@var{symbol} = @var{value}}.
The optional arguments recognized are:

@table @code
@item constraints
List of inequality and equality constraints that must be satisfied by
@var{X}.  The inequality constraints must be actual inequalities of
the form 
m4_math(g(X) \ge h(X), @math{g(@var{X}) >= h(@var{X})}) 
or 
m4_mathdot(g(X) \le h(X), @math{g(@var{X}) <= h(@var{X})})  
The equality constraints must be of the
form 
m4_mathdot(g(X) = h(X), @math{g(@var{X}) = h(@var{X})}) 
@item rhobeg
Initial value of the internal @var{RHO} variable which controls 
the size of simplex.  (Defaults to 1.0)
@item rhoend 
The desired final value rho parameter.  It is approximately
 the accuracy in the variables. (Defaults to 1d-6.)
@item iprint
 Verbose output level.  (Defaults to 0)
@itemize
@item
0 - No output
@item
1 - Summary at the end of the calculation
@item
2 - Each new value of RHO and SIGMA is printed, including 
 the vector of variables, some function information when RHO is reduced.
@item
3 - Like 2, but information is printed when F(X) is computed.
@end itemize
@item maxfun
The maximum number of function evaluations.  (Defaults to 1000).
@end table

On return, a vector is given:
@enumerate
@item
The value of the variables giving the minimum.  This is a list of
elements of the form @code{@var{var} = @var{value}} for each of the
variables listed in @var{X}.
@item
The minimized function value
@item
The number of function evaluations.
@item
Return code with the following meanings
 @itemize @w{}
 @item
 0 - No errors.
 @item
 1 - Limit on maximum number of function evaluations reached.
 @item
 2 - Rounding errors inhibiting progress.
 @item
 -1 - @var{MAXCV} value exceeds @var{RHOEND}.  This indicates that the
  constraints were probably not satisfied.  User should investigate
  the value of the constraints.

 @end itemize
@end enumerate

@var{MAXCV} stands for ``MAXimum Constraint Violation'' and is the
value of @math{max(0.0, -c1(x), -c2(x),...-cm(x))} where @math{ck(x)}
denotes the k'th constraint function.  (Note that maxima allows
constraints of the form @math{f(x) = g(x)}, which are internally
converted to @math{f(x)-g(x) >= 0} and @math{g(x)-f(x) >= 0} which is
required by COBYLA).

@code{load("fmin_cobyla")} loads this function.

@end deffn

@anchor{bf_fmin_cobyla}
@deffn {Function} bf_fmin_cobyla @
@fname{bf_fmin_cobyla} (@var{F}, @var{X}, @var{Y}) @
@fname{bf_fmin_cobyla} (@var{F}, @var{X}, @var{Y}, optional_args)

This function is identical to @code{fmin_cobyla}, except that bigfloat
operations are used, and the default value for @var{rhoend} is
@code{10^(fpprec/2)}. 

See @mref{fmin_cobyla} for more information.

@code{load("bf_fmin_cobyla")} loads this function.

@end deffn

@node Examples for cobyla, , Functions and Variables for cobyla, Package cobyla
@section Examples for cobyla

Minimize 
m4_math(x_1 x_2, @math{x_1 x_2}) 
with 
m4_mathdot(1-x_1^2-x_2^2 \ge 0, @math{1-x1^2-x2^2 >= 0})  
The theoretical solution is
m4_displaymath(
<<<\eqalign{
x_1 &= {1\over \sqrt{2}} \cr
x_2 &= -{1\over \sqrt{2}}
}>>>,
<<<
@math{x_1 = 1/sqrt(2)}

@math{x_2 = -1/sqrt(2)}
>>>
)

@c ===beg===
@c load("fmin_cobyla")$
@c fmin_cobyla(x1*x2, [x1, x2], [1,1], 
@c             constraints = [x1^2+x2^2<=1], iprint=1);
@c ===end===
@example
(%i1) load("fmin_cobyla")$
@group
(%i2) fmin_cobyla(x1*x2, [x1, x2], [1,1],
            constraints = [x1^2+x2^2<=1], iprint=1);
   Normal return from subroutine COBYLA

   NFVALS =   64   F =-5.000000E-01    MAXCV = 2.000122E-12
   X = 7.071076E-01  -7.071060E-01
(%o2) [[x1 = 0.707107555323284, x2 = - 0.7071060070503778], 
                                     - 0.4999999999998015, 64, 0]
@end group
@end example

Here is the same example but the constraint
is 
m4_math(x_1^2+x_2^2 \le -1, @math{x1^2+x2^2 <= -1}) 
which
is impossible over the reals.

@example
@group
(%i1) fmin_cobyla(x1*x2, [x1, x2], [1,1],
         constraints = [x1^2+x2^2 <= -1], iprint=1);
@end group
   Normal return from subroutine COBYLA

   NFVALS =   65   F = 3.016417E-13    MAXCV = 1.000000E+00
   X =-3.375179E-07  -8.937057E-07
(%o1) [[x1 = - 3.375178983064622e-7, x2 = - 8.937056510780022e-7], 
                                                3.016416530564557e-13, 65, - 1]
(%i2) subst(%o1[2], [x1^2+x2^2 <= -1]);
(%o2)                 [- 6.847914590915444e-13 <= - 1]
@end example

We see the return code (@code{%o1[4]}) is -1 indicating that the
constraints may not be satisfied.  Substituting the solution into the
constraint equation as shown in @code{%o2} shows that the constraint
is, of course, violated.

There are additional examples in the share/cobyla/ex directory and in
share/cobyla/rtest_cobyla.mac.

