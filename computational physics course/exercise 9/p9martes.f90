PROGRAM P9
IMPLICIT NONE 
DOUBLE PRECISION :: h, Lx, Ly, CF (4), T0, P(2), w , Tini (3), x, y
INTEGER :: Nx, Ny , i, j , k, iX, jX , iteracions 
DOUBLE PRECISION, DIMENSION (90, 66) :: T  
DOUBLE PRECISION Tit (50000)
COMMON /TEMP_P/ Tit, iteracions 
COMMON P   

EXTERNAL fun 
EXTERNAL nofont 

! PAS 
 h = 0.5d0 

! TAMANY DEL RECTANGLE 
 Lx = 44.5d0 
 Ly = 32.5d0 

! TAMANY DE LA MATRIU 
 Nx = Lx / h + 1 
 Ny = Ly / h + 1 

! CF = [ T(0,y) , T (x,Ly) , T (Lx,y) , T (x,0) ]
 CF = [ 0.5d0 , 25.3d0 , 11.2d0 , 17.d0 ]

! PUNT A ESTUDIAR
 P(1) = 25.5d0 
 P(2) = 13.5d0 

 iX = P(1)/h + 1
 jX = P(2)/h + 1

! CT SOBRERELAX 
 w = 1.55d0 

! TEMPERATURA INTERIOR INICIAL 
 Tini = [15.d0, 220.d0, 1280.d0 ]

! ESCRIPTURA ESTUDI CONVERGENCIA
 OPEN (20, FILE = 'P9-22-23-dimarts.dat')
 WRITE (20,*) '# ESTUDI CONVERGÈNCIA'
 DO i = 1, 3
  T0 = Tini (i)
 
  DO j = 1, 3 
  CALL RESOL_PDE ( Nx , Ny , CF , h , fun , t0 , w, j , T )
   DO k = 1, iteracions
    WRITE (20,*) k, Tit(k) 
   END DO 
   WRITE (20,*)
   WRITE (20,*)
  END DO 
 
 END DO 
 WRITE (20,*)
 WRITE (20,*)

! ESCRIPTURA MATRIU FONT 
 WRITE (20,*) '# T(X,Y)'

 DO i = 1, Nx 
  x = (i-1)*h 
  DO j = 1, Ny
   y = (j-1)*h 
   WRITE (20,*) x, y , T(i,j)
  END DO 
  WRITE (20,*)
 END DO 

 WRITE (20,*)
 WRITE (20,*)

! ESCRIPTURA MATRIU NO FONT 
 WRITE (20,*) '# T(X,Y) NO FONT'
 CALL RESOL_PDE ( Nx , Ny , CF , h , nofont , Tini (2) , w, 3 , T )

 DO i = 1, Nx 
  x = (i-1)*h 
  DO j = 1, Ny 
   y = (j-1)*h 
   WRITE (20,*) x, y , T(i,j)
  END DO 
  WRITE (20,*)
 END DO 

 CLOSE (20)

END PROGRAM


SUBROUTINE nofont ( x , y , fu )
 IMPLICIT NONE 
 DOUBLE PRECISION :: x, y, fu 
 fu = 0.d0 
 END SUBROUTINE 

SUBROUTINE fun ( x , y , fu )
 IMPLICIT NONE 
 DOUBLE PRECISION :: x, y, fu, ro1, ro2, ro3 
 CALL fun1 ( x , y , ro1 )
 CALL fun2 ( x, y , ro2 )
 CALL fun3 ( x , y , ro3 )
 fu = ro1 + ro2 + ro3 
 END SUBROUTINE 

SUBROUTINE fun1 ( x , y , fu )
 IMPLICIT NONE 
 DOUBLE PRECISION :: x , y , fu , ro10, r 
 ro10 = 10.d0 
 r = SQRT ( ( x-22.5d0 )**2 + ( y-8.d0)**2 ) 
 fu = ro10 * exp ( ((-(r-4.d0 )**2)) / ((0.7d0)**2) )

 END SUBROUTINE

SUBROUTINE fun2 ( x , y , fu )
 IMPLICIT NONE 
 DOUBLE PRECISION :: x , y , fu 
 IF ( x.LE.(35.d0).AND.x.GE.(29.d0).AND.y.LE.(22.d0).AND.y.GE.(18.d0)) THEN 
  fu = 7.d0 
 ELSE 
  fu = 0.d0 
 END IF 

 END SUBROUTINE 

SUBROUTINE fun3 ( x , y , fu )
 IMPLICIT NONE 
 DOUBLE PRECISION :: x , y , fu , ro30, r 
 ro30 = 5.5d0 
 r = SQRT ( ( x-10.5d0 )**2 + ( y-22.d0)**2 ) 
 fu = ro30 * exp ( ((-(r-5.d0 )**2)) / ((1.2d0)**2) )

 END SUBROUTINE

SUBROUTINE JACOBI ( Nx , Ny , CF , h , fun , t0 , tol, Tk1 )
 IMPLICIT NONE 
 INTEGER :: Nx, Ny , i , j , k , iteracions, ip, jp  
 DOUBLE PRECISION :: CF (4) , h , tol , T0 , x, y , ro, P(2)
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: Tk
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: Tk1 
 DOUBLE PRECISION  Tit(50000)
 COMMON /TEMP_P/ Tit, iteracions 
 COMMON P 
 EXTERNAL fun

 ip = P(1)/h + 1
 jp = P(2)/h + 1

 Tit = 0.d0

 CALL INI_MATRIU ( Nx , Ny , CF , T0 , Tk )
 CALL INI_MATRIU ( Nx , Ny , CF , T0 , Tk1 )

 ! PRIMERA ITERACIÓ 

 k = 1
 
 DO i = 2, Nx - 1
  x = (i-1)*h 
  DO j = 2, Ny - 1 
   y = (j-1)*h
   CALL fun ( x , y , ro )
   Tk1 (i,j) = (1.d0/4.d0) * ( Tk (i+1,j) + Tk (i-1,j) + Tk (i,j+1) + Tk (i,j-1) + (h**2)*ro)
  END DO 
 END DO 
 
 
 DO WHILE (ABS ( Tk1 (Nx-1,Ny-1)- Tk (Nx-1,Ny-1)).GT.tol ) 
  k = k + 1
  Tk = Tk1 
  DO i = 2, Nx - 1
   x = (i-1)*h 
   DO j = 2, Ny - 1 
    y = (j-1)*h
    CALL fun ( x , y , ro )
    Tk1 (i,j) = (1.d0/4.d0) * ( Tk (i+1,j) + Tk (i-1,j) + Tk (i,j+1) + Tk (i,j-1) + (h**2)*ro)
   END DO 
  END DO
  Tit (k) = Tk1 (ip,jp)
 END DO  
 iteracions = k 
 END SUBROUTINE 


SUBROUTINE GAUSS_SEIDEL ( Nx , Ny , CF , h , fun , t0 , tol, Tk1  )
 IMPLICIT NONE 
 INTEGER :: Nx, Ny , i , j , k , iteracions , ip, jp 
 DOUBLE PRECISION :: CF (4) , h , tol , T0 , x, y , ro , P(2)
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: Tk
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: Tk1 
 DOUBLE PRECISION  Tit(50000)
 COMMON /TEMP_P/ Tit, iteracions 
 COMMON P  
 EXTERNAL fun

 ip = P(1)/h + 1
 jp = P(2)/h + 1

 Tit = 0.d0

 CALL INI_MATRIU ( Nx , Ny , CF , T0 , Tk )
 CALL INI_MATRIU ( Nx , Ny , CF , T0 , Tk1 )

 ! PRIMERA ITERACIÓ
 k = 1 
 DO i = 2, Nx - 1
  x = (i-1)*h 
  DO j = 2, Ny - 1 
   y = (j-1)*h
   CALL fun ( x , y , ro )
   Tk1 (i,j) = (1.d0/4.d0) * ( Tk (i+1,j) + Tk1 (i-1,j) + Tk (i,j+1) + Tk1 (i,j-1) + (h**2)*ro)
  END DO 
 END DO 

 

 DO WHILE (ABS ( Tk1 (Nx-1,Ny-1)- Tk (Nx-1,Ny-1)).GT.tol)
  k = k + 1
  Tk = Tk1 
  DO i = 2, Nx - 1
   x = (i-1)*h 
   DO j = 2, Ny - 1 
    y = (j-1)*h
    CALL fun ( x , y , ro )
    Tk1 (i,j) = (1.d0/4.d0) * ( Tk (i+1,j) + Tk1 (i-1,j) + Tk (i,j+1) + Tk1 (i,j-1) + (h**2)*ro)
   END DO 
  END DO  
  Tit (k) = Tk1 (ip,jp)
 END DO 
 iteracions = k 

 END SUBROUTINE 


SUBROUTINE SOBRERELAX ( Nx , Ny , CF , h , fun , t0 , w, tol, Tk1 )
 IMPLICIT NONE 
 INTEGER :: Nx, Ny , i , j , k,  iteracions, ip, jp 
 DOUBLE PRECISION :: CF (4) , h , tol , T0 , x, y , ro , w , P(2)
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: Tk
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: Tk1 
 DOUBLE PRECISION  Tit(50000)
 COMMON /TEMP_P/ Tit, iteracions 
 COMMON P 
 EXTERNAL fun

 ip = P(1)/h + 1
 jp = P(2)/h + 1

 Tit = 0.d0

 CALL INI_MATRIU ( Nx , Ny , CF , T0 , Tk )
 CALL INI_MATRIU ( Nx , Ny , CF , T0 , Tk1 )

 ! PRIMERA ITERACIÓ 
 k = 1
 DO i = 2, Nx - 1
  x = (i-1)*h 
  DO j = 2, Ny - 1 
   y = (j-1)*h
   CALL fun ( x , y , ro )
   Tk1 (i,j) = Tk (i,j) + w*(1.d0/4.d0) * ( Tk (i+1,j) + Tk1 (i-1,j) + Tk (i,j+1) + Tk1 (i,j-1) -4.d0*Tk(i,j) + (h**2)*ro)
  END DO 
 END DO 



 DO WHILE (ABS ( Tk1 (Nx-1,Ny-1)- Tk (Nx-1,Ny-1)).GT.tol)
  k = k + 1
  Tk = Tk1 
  DO i = 2, Nx - 1
   x = (i-1)*h 
   DO j = 2, Ny - 1 
    y = (j-1)*h
    CALL fun ( x , y , ro )
    Tk1 (i,j) = Tk (i,j) + w*(1.d0/4.d0)*( Tk (i+1,j) + Tk1 (i-1,j) + Tk (i,j+1) + Tk1 (i,j-1) -4.d0*Tk(i,j) + (h**2)*ro)
   END DO 
  END DO 
  Tit (k) = Tk1 (ip,jp)
 END DO 

 iteracions = k 

 END SUBROUTINE 


SUBROUTINE INI_MATRIU ( Nx , Ny , CF , t0 , MATRIU )
 IMPLICIT NONE 
 DOUBLE PRECISION :: CF (4), t0 
 INTEGER :: Nx, Ny , i, j 
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: MATRIU

  DO i = 1, Nx 
  DO j = 1, Ny 
   IF (i.EQ.1) THEN 
    MATRIU (i,j) = CF (1)
   ELSEIF (j.EQ.Ny) THEN 
    MATRIU (i,j) = CF (2)
   ELSEIF (i.EQ.Nx) THEN 
    MATRIU (i,j) = CF (3)
   ELSEIF (j.EQ.1) THEN 
    MATRIU (i,j) = CF (4)
   ELSE 
    MATRIU (i,j) = t0
   END IF 
  END DO 
 END DO 

 END SUBROUTINE


SUBROUTINE RESOL_PDE ( Nx , Ny , CF , h , fun , t0 , w, icontrol , T )
 IMPLICIT NONE 
 INTEGER :: Nx, Ny, icontrol , iteracions , i 
 DOUBLE PRECISION :: CF (4) , h , T0 , w , tol 
 DOUBLE PRECISION, DIMENSION (Nx, Ny) :: T 
 EXTERNAL fun
 
 tol = 10.d0**(-10)

 IF (icontrol.EQ.1) THEN 
  CALL JACOBI ( Nx , Ny , CF , h , fun , t0 , tol, T )
 ELSEIF (icontrol.EQ.2) THEN 
  CALL GAUSS_SEIDEL ( Nx , Ny , CF , h , fun , t0 , tol, T )
 ELSEIF (icontrol.EQ.3) THEN 
  CALL SOBRERELAX ( Nx , Ny , CF , h , fun , t0 , w, tol, T )
 ELSE 
  WRITE (*,*) " ESCULL UN NOMBRE DEL 1 AL 3, 1-J, 2-GS, 3-SX "
 END IF 



  
 
 END SUBROUTINE 



