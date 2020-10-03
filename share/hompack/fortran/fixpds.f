      SUBROUTINE FIXPDS(N,Y,IFLAG,ARCTOL,EPS,TRACE,A,NDIMA,NFE,
     $     ARCLEN,YP,YPOLD,QR,LENQR,PIVOT,PP,WORK,WT,PHI,P,
     $     PAR,IPAR)
C
C SUBROUTINE  FIXPDS  FINDS A FIXED POINT OR ZERO OF THE
C N-DIMENSIONAL VECTOR FUNCTION F(X), OR TRACKS A ZERO CURVE
C OF A GENERAL HOMOTOPY MAP RHO(A,X,LAMBDA).  FOR THE FIXED
C POINT PROBLEM F(X) IS ASSUMED TO BE A C2 MAP OF SOME BALL
C INTO ITSELF.  THE EQUATION  X = F(X)  IS SOLVED BY
C FOLLOWING THE ZERO CURVE OF THE HOMOTOPY MAP
C
C  LAMBDA*(X - F(X)) + (1 - LAMBDA)*(X - A)  ,
C
C STARTING FROM LAMBDA = 0, X = A.  THE CURVE IS PARAMETERIZED
C BY ARC LENGTH S, AND IS FOLLOWED BY SOLVING THE ORDINARY
C DIFFERENTIAL EQUATION  D(HOMOTOPY MAP)/DS = 0  FOR
C Y(S) = (X(S), LAMBDA(S)).
C
C FOR THE ZERO FINDING PROBLEM F(X) IS ASSUMED TO BE A C2 MAP
C SUCH THAT FOR SOME R > 0,  X*F(X) >= 0  WHENEVER NORM(X) = R.
C THE EQUATION  F(X) = 0  IS SOLVED BY FOLLOWING THE ZERO CURVE
C OF THE HOMOTOPY MAP
C
C   LAMBDA*F(X) + (1 - LAMBDA)*(X - A)
C
C EMANATING FROM LAMBDA = 0, X = A.
C
C  A  MUST BE AN INTERIOR POINT OF THE ABOVE MENTIONED BALLS.
C
C FOR THE CURVE TRACKING PROBLEM RHO(A,X,LAMBDA) IS ASSUMED TO
C BE A C2 MAP FROM E**M X E**N X [0,1) INTO E**N, WHICH FOR
C ALMOST ALL PARAMETER VECTORS A IN SOME NONEMPTY OPEN SUBSET
C OF E**M SATISFIES
C
C  RANK [D RHO(A,X,LAMBDA)/D LAMBDA , D RHO(A,X,LAMBDA)/DX] = N
C
C FOR ALL POINTS (X,LAMBDA) SUCH THAT RHO(A,X,LAMBDA)=0.  IT IS
C FURTHER ASSUMED THAT
C
C           RANK [ D RHO(A,X0,0)/DX ] = N  .
C
C WITH A FIXED, THE ZERO CURVE OF RHO(A,X,LAMBDA) EMANATING
C FROM  LAMBDA = 0, X = X0  IS TRACKED UNTIL  LAMBDA = 1  BY
C SOLVING THE ORDINARY DIFFERENTIAL EQUATION
C D RHO(A,X(S),LAMBDA(S))/DS = 0  FOR  Y(S) = (X(S), LAMBDA(S)),
C WHERE S IS ARC LENGTH ALONG THE ZERO CURVE.  ALSO THE HOMOTOPY
C MAP RHO(A,X,LAMBDA) IS ASSUMED TO BE CONSTRUCTED SUCH THAT
C
C              D LAMBDA(0)/DS > 0  .
C
C THIS CODE IS BASED ON THE ALGORITHM IN L. T. WATSON, A
C GLOBALLY CONVERGENT ALGORITHM FOR COMPUTING FIXED POINTS OF
C C2 MAPS, APPL. MATH. COMPUT., 5 (1979) 297-311.
C
C
C FOR THE FIXED POINT AND ZERO FINDING PROBLEMS, THE USER
C MUST SUPPLY A SUBROUTINE  F(X,V)  WHICH EVALUATES F(X) AT X
C AND RETURNS THE VECTOR F(X) IN V, AND A SUBROUTINE
C  FJACS(X,QR,LENQR,PIVOT)  WHICH EVALUATES THE (SYMMETRIC)
C JACOBIAN MATRIX OF F(X) AT X, AND RETURNS THE SYMMETRIC
C JACOBIAN MATRIX IN PACKED SKYLINE STORAGE FORMAT IN QR.  LENQR
C AND PIVOT DESCRIBE THE DATA STRUCTURE IN QR.  FOR THE CURVE
C TRACKING PROBLEM, THE USER MUST SUPPLY A SUBROUTINE
C  RHOA(V,LAMBDA,X,PAR,IPAR)  WHICH GIVEN (X,LAMBDA) RETURNS A
C PARAMETER VECTOR A IN V SUCH THAT RHO(A,X,LAMBDA)=0, AND A
C SUBROUTINE  RHOJS(A,LAMBDA,X,QR,LENQR,PIVOT,PP,PAR,IPAR)  WHICH
C RETURNS IN QR THE SYMMETRIC N X N JACOBIAN MATRIX [D RHO/DX]
C EVALUATED AT (A,X,LAMBDA) AND STORED IN PACKED SKYLINE FORMAT,
C AND RETURNS IN PP THE VECTOR -(D RHO/D LAMBDA) EVALUATED AT
C (A,X,LAMBDA).  LENQR AND PIVOT DESCRIBE THE DATA STRUCTURE IN QR.
C *** NOTE THE MINUS SIGN IN THE DEFINITION OF PP. ***
C
C
C FUNCTIONS AND SUBROUTINES DIRECTLY OR INDIRECTLY CALLED BY FIXPDS:
C  D1MACH , F (OR  RHOA ), FJACS (OR  RHOJS ), FODEDS , GMFADS ,
C  MFACDS , MULTDS , PCGDS , QIMUDS , ROOT , SINTRP , SOLVDS ,
C  STEPDS , AND THE BLAS FUNCTIONS  DAXPY , DCOPY , DDOT , DNRM2 ,
C  DSCAL , IDAMAX .  ONLY  D1MACH  CONTAINS MACHINE DEPENDENT
C  CONSTANTS.  NO OTHER MODIFICATIONS BY THE USER ARE REQUIRED.
C
C ***WARNING:  THIS SUBROUTINE IS GENERALLY MORE ROBUST THAN  FIXPNS
C AND  FIXPQS , BUT MAY BE SLOWER THAN THOSE SUBROUTINES BY A
C FACTOR OF TWO.
C
C
C ON INPUT:
C
C N  IS THE DIMENSION OF X, F(X), AND RHO(A,X,LAMBDA).
C
C Y  IS AN ARRRAY OF LENGTH  N + 1.  (Y(1),...,Y(N)) = A  IS THE
C    STARTING POINT FOR THE ZERO CURVE FOR THE FIXED POINT AND
C    ZERO FINDING PROBLEMS.  (Y(1),...,Y(N)) = X0  FOR THE CURVE
C    TRACKING PROBLEM.
C
C IFLAG  CAN BE -2, -1, 0, 2, OR 3.  IFLAG  SHOULD BE 0 ON THE
C    FIRST CALL TO  FIXPDS  FOR THE PROBLEM  X=F(X), -1 FOR THE
C    PROBLEM  F(X)=0, AND -2 FOR THE PROBLEM  RHO(A,X,LAMBDA)=0.
C    IN CERTAIN SITUATIONS  IFLAG  IS SET TO 2 OR 3 BY  FIXPDS,
C    AND  FIXPDS  CAN BE CALLED AGAIN WITHOUT CHANGING  IFLAG.
C
C ARCTOL  IS THE LOCAL ERROR ALLOWED THE ODE SOLVER WHEN
C    FOLLOWING THE ZERO CURVE.  IF  ARCTOL .LE. 0.0  ON INPUT
C    IT IS RESET TO  .5*DSQRT(EPS).  NORMALLY  ARCTOL  SHOULD
C    BE CONSIDERABLY LARGER THAN  EPS.
C
C EPS  IS THE LOCAL ERROR ALLOWED THE ODE SOLVER WHEN VERY
C    NEAR THE FIXED POINT(ZERO).  EPS  IS APPROXIMATELY THE
C    MIXED ABSOLUTE AND RELATIVE ERROR IN THE COMPUTED FIXED
C    POINT(ZERO).
C
C TRACE  IS AN INTEGER SPECIFYING THE LOGICAL I/O UNIT FOR
C    INTERMEDIATE OUTPUT.  IF  TRACE .GT. 0  THE POINTS COMPUTED ON
C    THE ZERO CURVE ARE WRITTEN TO I/O UNIT  TRACE .
C
C A(1:NDIMA) CONTAINS THE PARAMETER VECTOR  A .  FOR THE FIXED POINT
C    AND ZERO FINDING PROBLEMS, A  NEED NOT BE INITIALIZED BY THE
C    USER, AND IS ASSUMED TO HAVE LENGTH  N.  FOR THE CURVE
C    TRACKING PROBLEM, A  HAS LENGTH  NDIMA  AND MUST BE INITIALIZED
C    BY THE USER.
C
C NDIMA  IS THE DIMENSION OF  A , AND IS USED ONLY FOR THE CURVE
C    TRACKING PROBLEM.
C
C YP(1:N+1) IS A WORK ARRAY CONTAINING THE CURRENT TANGENT
C    VECTOR TO THE ZERO CURVE.
C
C YPOLD(1:N+1) IS A WORK ARRAY CONTAINING THE PREVIOUS TANGENT
C    VECTOR TO THE ZERO CURVE.
C
C QR(1:LENQR)  IS A WORK ARRAY CONTAINING THE (SYMMETRIC) JACOBIAN
C    MATRIX WITH RESPECT TO X, IN THE PACKED SKYLINE STORAGE FORMAT.
C
C LENQR  IS THE DIMENSION OF  QR .
C
C PIVOT(1:N+2), PP(1:N), AND WORK(1:6*(N+1)+LENQR) ARE ALL WORK
C    ARRAYS USED BY  FODEDS  TO CALCULATE THE TANGENT VECTOR YP.
C
C WT(1:N+1), PHI(1:N+1,1:16), AND P(1:N+1) ARE ALL WORK ARRAYS
C    USED BY THE ODE SUBROUTINE  STEPDS  .
C
C PAR(1:*) AND IPAR(1:*) ARE ARRAYS FOR (OPTIONAL) USER PARAMETERS,
C    WHICH ARE SIMPLY PASSED THROUGH TO THE USER WRITTEN SUBROUTINES
C    RHOA, RHOJS.
C
C Y, ARCTOL, EPS, ARCLEN, NFE, AND IFLAG SHOULD ALL BE
C VARIABLES IN THE CALLING PROGRAM.
C
C
C ON OUTPUT:
C
C N  AND  TRACE  ARE UNCHANGED.
C
C (Y(1),...,Y(N)) = X, Y(N+1) = LAMBDA, AND Y IS AN APPROXIMATE
C    ZERO OF THE HOMOTOPY MAP.  NORMALLY LAMBDA = 1 AND X IS A
C    FIXED POINT(ZERO) OF F(X).  IN ABNORMAL SITUATIONS LAMBDA
C    MAY ONLY BE NEAR 1 AND X IS NEAR A FIXED POINT(ZERO).
C
C IFLAG =
C  -2   CAUSES  FIXPDS  TO INITIALIZE EVERYTHING FOR THE PROBLEM
C       RHO(A,X,LAMBDA) = 0 (USE ON FIRST CALL).
C
C  -1   CAUSES  FIXPDS  TO INITIALIZE EVERYTHING FOR THE PROBLEM
C       F(X) = 0 (USE ON FIRST CALL).
C
C   0   CAUSES  FIXPDS  TO INITIALIZE EVERYTHING FOR THE PROBLEM
C       X = F(X) (USE ON FIRST CALL).
C
C   1   NORMAL RETURN.
C
C   2   SPECIFIED ERROR TOLERANCE CANNOT BE MET.  EPS HAS BEEN
C       INCREASED TO A SUITABLE VALUE.  TO CONTINUE, JUST CALL
C       FIXPDS  AGAIN WITHOUT CHANGING ANY PARAMETERS.
C
C   3   STEPDS  HAS BEEN CALLED 1000 TIMES.  TO CONTINUE, CALL
C       FIXPDS  AGAIN WITHOUT CHANGING ANY PARAMETERS.
C
C   4   JACOBIAN MATRIX DOES NOT HAVE FULL RANK AND/OR THE CONJUGATE
C       GRADIENT ITERATION FOR THE KERNEL OF THE JACOBIAN MATRIX
C       FAILED TO CONVERGE.  THE ALGORITHM HAS FAILED (THE ZERO
C       CURVE OF THE HOMOTOPY MAP CANNOT BE FOLLOWED ANY FURTHER).
C
C   5   EPS  (OR  ARCTOL ) IS TOO LARGE.  THE PROBLEM SHOULD BE
C       RESTARTED BY CALLING  FIXPDS  WITH A SMALLER  EPS  (OR
C       ARCTOL ) AND  IFLAG = 0 (-1, -2).
C
C   6   I - DF(X)  IS NEARLY SINGULAR AT THE FIXED POINT (DF(X) IS
C       NEARLY SINGULAR AT THE ZERO, OR  D RHO(A,X,LAMBDA)/DX  IS
C       NEARLY SINGULAR AT  LAMBDA = 1 ).  ANSWER MAY NOT BE
C       ACCURATE.
C
C   7   ILLEGAL INPUT PARAMETERS, A FATAL ERROR.
C
C ARCTOL = EPS AFTER A NORMAL RETURN (IFLAG = 1).
C
C EPS  IS UNCHANGED AFTER A NORMAL RETURN (IFLAG = 1).  IT IS
C    INCREASED TO AN APPROPRIATE VALUE ON THE RETURN IFLAG = 2.
C
C A  WILL (NORMALLY) HAVE BEEN MODIFIED.
C
C NFE  IS THE NUMBER OF FUNCTION EVALUATIONS (= NUMBER OF
C    JACOBIAN EVALUATIONS).
C
C ARCLEN  IS THE LENGTH OF THE PATH FOLLOWED.
C
C
      DOUBLE PRECISION AOLD,ARCLEN,ARCTOL,CURSW,CURTOL,EPS,
     1  EPSSTP,EPST,H,HOLD,S,S99,SA,SB,SOUT,SQNP1,XOLD,Y1SOUT
      INTEGER IFLAG,IFLAGC,ITER,IVC,J,JW,K,KGI,KOLD,
     1  KSTEPS,LCODE,LENQR,LIMIT,LIMITD,N,NDIMA,NFE,NFEC,NP1,TRACE
      LOGICAL START,CRASH,ST99
C
C *****  ARRAY DECLARATIONS.  *****
C
C ARRAYS NEEDED BY THE ODE SUBROUTINE  STEPDS .
      DOUBLE PRECISION Y(N+1),WT(N+1),PHI(N+1,16),P(N+1),YP(N+1),
     1     ALPHAS(12),W(12),G(13),GI(11)
      INTEGER IV(10)
C
C ARRAYS NEEDED BY  FIXPDS , FODEDS , AND  PCGDS .
      DOUBLE PRECISION YPOLD(N+1),A(N),QR(LENQR),PP(N),
     1     WORK(6*(N+1)+LENQR),PAR(1)
      INTEGER PIVOT(N+2),IPAR(1)
C
C *****  END OF DIMENSIONAL INFORMATION.  *****
C
      SAVE
      EXTERNAL FODEDS
C
C LIMITD  IS AN UPPER BOUND ON THE NUMBER OF STEPS.  IT MAY BE
C CHANGED BY CHANGING THE FOLLOWING PARAMETER STATEMENT:
      PARAMETER (LIMITD=1000)
C
C
C :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :
      IF (N .LE. 0  .OR.  EPS .LE. 0.0 ) IFLAG=7
      IF (IFLAG .GE. -2  .AND.  IFLAG .LE. 0) GO TO 10
      IF (IFLAG .EQ. 2) GO TO 35
      IF (IFLAG .EQ. 3) GO TO 30
C ONLY VALID INPUT FOR  IFLAG  IS -2, -1, 0, 2, 3.
      IFLAG=7
      RETURN
C
C *****  INITIALIZATION BLOCK.  *****
C
10    ARCLEN=0.0
      S=0.0
      IF (ARCTOL .LE. 0.0) ARCTOL=.5*SQRT(EPS)
      NFEC=0
      IFLAGC=IFLAG
      NP1=N+1
      SQNP1=SQRT(DBLE(NP1))
C
C SWITCH FROM THE TOLERANCE  ARCTOL  TO THE (FINER) TOLERANCE  EPS  IF
C THE CURVATURE OF ANY COMPONENT OF  Y  EXCEEDS  CURSW.
C
      CURSW=10.0
C
      ST99=.FALSE.
      START=.TRUE.
      CRASH=.FALSE.
      HOLD=1.0
      H=.1
      EPSSTP=ARCTOL
      KSTEPS=0
C SET INITIAL CONDITIONS FOR ORDINARY DIFFERENTIAL EQUATION.
      YPOLD(NP1)=1.0
      YP(NP1)=1.0
      Y(NP1)=0.0
      WORK(2*NP1)=0.0
      WORK(3*NP1)=0.0
      DO 20 J=1,N
        YPOLD(J)=0.0
        YP(J)=0.0
        WORK(NP1+J)=0.0
        WORK(2*NP1+J)=0.0
20    CONTINUE
C LOAD  A  FOR THE FIXED POINT AND ZERO FINDING PROBLEMS.
      IF (IFLAGC .GE. -1) THEN
        CALL DCOPY(N,Y,1,A,1)
      ENDIF
30    LIMIT=LIMITD
C
C *****  END OF INITIALIZATION BLOCK.  *****
C
C
C *****  MAIN LOOP.  *****
C
35    DO 150 ITER=1,LIMIT
      IF (Y(NP1) .LT. 0.0) THEN
40      ARCLEN=ARCLEN+S
        IFLAG=5
        RETURN
      ENDIF
50    IF (S .LE. 7.0*SQNP1) GO TO 80
C ARC LENGTH IS GETTING TOO LONG, THE PROBLEM WILL BE
C RESTARTED WITH A DIFFERENT  A  VECTOR.
      ARCLEN=ARCLEN+S
      S=0.0
60    START=.TRUE.
      CRASH=.FALSE.
C COMPUTE A NEW  A  VECTOR.
      IF (IFLAGC .EQ. -2) THEN
        DO 63 JW=1,NDIMA
          QR(JW)=A(JW)
63      CONTINUE
        CALL RHOA(A,Y(NP1),Y,PAR,IPAR)
        DO 65 JW=1,NDIMA
          AOLD=QR(JW)
C IF NEW AND OLD  A  DIFFER BY TOO MUCH, TRACKING SHOULD NOT CONTINUE.
          IF (ABS(A(JW)-AOLD) .GT. 1.0+ABS(AOLD)) THEN
            ARCLEN=ARCLEN+S
            IFLAG=5
            RETURN
          ENDIF
65      CONTINUE
      ELSE
        CALL F(Y,YP)
        DO 70 JW=1,N
          AOLD=A(JW)
          IF (IFLAGC .EQ. -1) THEN
            A(JW)=Y(NP1)*YP(JW)/(1.0 - Y(NP1)) + Y(JW)
          ELSE
            A(JW)=(Y(JW) - Y(NP1)*YP(JW))/(1.0 - Y(NP1))
          ENDIF
C IF NEW AND OLD  A  DIFFER BY TOO MUCH, TRACKING SHOULD NOT CONTINUE.
          IF (ABS(A(JW)-AOLD) .GT. 1.0+ABS(AOLD)) THEN
            ARCLEN=ARCLEN+S
            IFLAG=5
            RETURN
          ENDIF
70      CONTINUE
      ENDIF
      GO TO 100
80    IF (Y(NP1) .LE. .99  .OR. ST99) GO TO 100
C WHEN LAMBDA REACHES .99, THE PROBLEM WILL BE RESTARTED WITH
C A NEW  A  VECTOR.
90    ST99=.TRUE.
      EPSSTP=EPS
      ARCTOL=EPS
      GO TO 60
C
C SET DIFFERENT ERROR TOLERANCE FOR HIGH CURVATURE COMPONENTS OF THE
C TRAJECTORY Y(S).
100   CURTOL=CURSW*HOLD
      EPST=EPS/EPSSTP
      DO 110 JW=1,NP1
        IF (ABS(YP(JW)-YPOLD(JW)) .LE. CURTOL) THEN
          WT(JW)=(ABS(Y(JW))+1.0)
        ELSE
          WT(JW)=(ABS(Y(JW))+1.0)*EPST
        ENDIF
110   CONTINUE
C
C TAKE A STEP ALONG THE CURVE.
      CALL STEPDS(FODEDS,NP1,Y,S,H,EPSSTP,WT,START,HOLD,K,KOLD,CRASH,
     +     PHI,P,YP,ALPHAS,W,G,KSTEPS,XOLD,IVC,IV,KGI,GI,
     +     YPOLD,A,QR,LENQR,PIVOT,PP,WORK,NFEC,IFLAGC,PAR,IPAR)
C PRINT LATEST POINT ON CURVE IF REQUESTED.
      IF (TRACE .GT. 0) THEN
        WRITE (TRACE,117) ITER,NFEC,S,Y(NP1),(Y(JW),JW=1,N)
117     FORMAT(/' STEP',I5,3X,'NFE =',I5,3X,'ARC LENGTH =',F9.4,3X,
     $  'LAMBDA =',F7.4,5X,'X vector:'/1P,(1X,6E12.4))
      ENDIF
      NFE=NFEC
C CHECK IF THE STEP WAS SUCCESSFUL.
      IF (IFLAGC .EQ. 4) THEN
        ARCLEN=ARCLEN+S
        IFLAG=4
        RETURN
      ENDIF
120   IF (CRASH) THEN
C RETURN CODE FOR ERROR TOLERANCE TOO SMALL.
        IFLAG=2
C CHANGE ERROR TOLERANCES.
        EPS=EPSSTP
        IF (ARCTOL .LT. EPS) ARCTOL=EPS
C CHANGE LIMIT ON NUMBER OF ITERATIONS.
        LIMIT=LIMIT-ITER
        RETURN
      ENDIF
C
130   IF (Y(NP1) .GE. 1.0) THEN
        IF (ST99) GO TO 160
C
C IF LAMBDA .GE. 1.0 BUT THE PROBLEM HAS NOT BEEN RESTARTED
C WITH A NEW  A  VECTOR, BACK UP AND RESTART.
C
        S99=S-.5*HOLD
C GET AN APPROXIMATE ZERO Y(S) WITH  Y(NP1)=LAMBDA .LT. 1.0  .
135     CALL SINTRP(S,Y,S99,WT,YP,NP1,KOLD,PHI,IVC,IV,KGI,GI,
     $     ALPHAS,G,W,XOLD,P)
        IF (WT(NP1) .LT. 1.0) GO TO 140
        S99=.5*(S-HOLD+S99)
        GO TO 135
C
140     CALL DCOPY(NP1,WT,1,Y,1)
        CALL DCOPY(NP1,YP,1,YPOLD,1)
        S=S99
        GO TO 90
      ENDIF
C
150   CONTINUE
C
C *****  END OF MAIN LOOP.  *****
C
C LAMBDA HAS NOT REACHED 1 IN 1000 STEPS.
      IFLAG=3
      RETURN
C
C
C USE INVERSE INTERPOLATION TO GET THE ANSWER AT LAMBDA = 1.0 .
C
160   SA=S-HOLD
      SB=S
      LCODE=1
170   CALL ROOT(SOUT,Y1SOUT,SA,SB,EPS,EPS,LCODE)
C ROOT  FINDS S SUCH THAT Y(NP1)(S) = LAMBDA = 1 .
      IF (LCODE .GT. 0) GO TO 190
      CALL SINTRP(S,Y,SOUT,WT,YP,NP1,KOLD,PHI,IVC,IV,KGI,GI,
     $     ALPHAS,G,W,XOLD,P)
      Y1SOUT=WT(NP1)-1.0
      GO TO 170
190   IFLAG=1
C SET IFLAG = 6 IF  ROOT  COULD NOT GET  LAMBDA = 1.0  .
      IF (LCODE .GT. 2) IFLAG=6
      ARCLEN=ARCLEN+SA
C LAMBDA(SA) = 1.0 .
      CALL SINTRP(S,Y,SA,WT,YP,NP1,KOLD,PHI,IVC,IV,KGI,GI,
     $     ALPHAS,G,W,XOLD,P)
C
      CALL DCOPY(NP1,WT,1,Y,1)
C
      RETURN
      END