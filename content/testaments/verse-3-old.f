C     VERSE 3, OLD TESTAMENT: ARRAYS BY ELEMENT.
C     FIXED SIZES, EXPLICIT LOOPS, EVERY ELEMENT VISITED BY HAND.
      PROGRAM ARRAYS
      INTEGER N
      PARAMETER (N = 5)
      REAL A(N), B(N), C(N)
      INTEGER I
      DO 20 I = 1, N
         A(I) = REAL(I)
         B(I) = 2.0 * REAL(I)
   20 CONTINUE
      DO 30 I = 1, N
         C(I) = A(I) + B(I)
   30 CONTINUE
      WRITE (*, *) C
      END
