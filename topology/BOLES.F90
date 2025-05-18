PROGRAM BOLES
IMPLICIT NONE 
INTEGER :: DIM , NDAT , i 
PARAMETER ( DIM = 2 , NDAT = 30000 )
DOUBLE PRECISION :: X (DIM) , Y (DIM), A , B , XDAT (NDAT) , YDAT (NDAT) , r , d 

A = -3.d0 
B = 3.d0 
r = 1.d0 
X = [1.D0 , 1.D0]

CALL uniforme ( NDAT , XDAT , A , B )
CALL uniforme ( NDAT , YDAT , A , B )

OPEN ( 21, FILE = 'BOLAchev.dat')
 DO i = 1, NDAT 
  Y = [ XDAT(i),YDAT(i) ]
  CALL distance_chev ( DIM , X , Y , d )
  IF (d.LT.r) THEN 
   WRITE (21,*) Y (1), Y (2)
  END IF 
 END DO 
CLOSE (21)

OPEN ( 22, FILE = 'BOLAeu.dat')
 DO i = 1, NDAT 
  Y = [ XDAT(i),YDAT(i) ]
  CALL distance_euclidean  ( DIM , X , Y , d )
  IF (d.LT.r) THEN 
   WRITE (22,*) Y (1), Y (2)
  END IF 
 END DO 
CLOSE (22)

OPEN ( 23, FILE = 'BOLAromb.dat')
 DO i = 1, NDAT 
  Y = [ XDAT(i) , YDAT(i) ]
  CALL distance ( DIM , X , Y , d )
  IF (d.LT.r) THEN 
   WRITE (23,*) Y (1), Y (2)
  END IF 
 END DO 
CLOSE (23)


END PROGRAM










SUBROUTINE uniforme ( ndat , xdat, a , b  )
  IMPLICIT NONE 
  DOUBLE PRECISION :: xdat (ndat), a , b 
  INTEGER :: ndat, i 

  DO i = 1, ndat 
   xdat (i) = a + (b-a)*RAND()
  END DO  

  END SUBROUTINE 

SUBROUTINE distance_chev ( dim , x , y , d )
  IMPLICIT NONE 
  INTEGER :: dim , i 
  DOUBLE PRECISION :: x (dim) , y (dim) , d , d1
  d = 0.d0 
  d1 = 0.d0 
  DO i = 1, dim 
   d1 = abs ( x(i) - y(i))
   IF (d1.GT.d) THEN 
    d = d1
   END IF 
  END DO 
END SUBROUTINE 

SUBROUTINE distance_euclidean ( dim , x, y , d )
  IMPLICIT NONE 
  INTEGER :: dim , i 
  DOUBLE PRECISION :: x (dim) , y (dim) , d 
  d = 0.d0 
  DO i = 1, dim 
   d = d + (x(i)-y(i))**2 
  END DO 
  d = SQRT (d)
END SUBROUTINE 

SUBROUTINE distance ( dim , x, y , d )
  IMPLICIT NONE 
  INTEGER :: dim , i 
  DOUBLE PRECISION :: x (dim) , y (dim) , d 
  d = 0.d0 
  DO i = 1, dim 
   d = d + abs (x(i)-y(i))
  END DO 
END SUBROUTINE 



