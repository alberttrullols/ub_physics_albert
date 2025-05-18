PROGRAM PRAC6
IMPLICIT NONE 
INTEGER:: NDAMAX1a , NDAMAX1bc , NDAMAX2
EXTERNAL STAT
EXTERNAL funcio1b 
PARAMETER ( NDAMAX1a = 45000 , NDAMAX1bc = 1000000 , NDAMAX2 = 250000 )
DOUBLE PRECISION :: UNI ( NDAMAX1a ),  UPS ( NDAMAX1a ), DOWNS ( NDAMAX1a ) , DADESbc ( NDAMAX1bc ), GDADESbc ( NDAMAX1bc ) 
DOUBLE PRECISION :: XA, XB, fu, mitjana, I1, I2 , desvest1, desvest2, pi, L, M, PSI2DADESbc ( NDAMAX2 )
INTEGER :: ISEED, i, N 
COMMON / CONSTANTS / pi, L 

ISEED = 20400656
CALL SRAND (ISEED)


! APARTAT 1: INTEGRALS DE MONTECARLO 1D 
!---------------------------------------------------------------------------------------------------------------

! APARTAT 1a : CÀLCUL NUMERO QUARKS DE VALENCIA DINS EL PROTÓ nu i nd

   ! GENEREM MOSTRA DE N_MAX NUMEROS ALEATORIS DISTRIBUITS UNIFORMEMENT ENTRE 0 I 1 
   XA = 0.d0 
   XB = 1.d0 

   CALL uniforme ( NDAMAX1a , UNI , XA , XB )
   
   ! GENEREM UNA MOSTRA DE N_MAX NUMEROS ALEAORIS AVALUATS A LES FUNCIONS UP, DOWN

   DO i = 1, NDAMAX1a 
    CALL up ( UNI (i) , fu ) 
    UPS (i) = fu
   END DO 

   DO i = 1, NDAMAX1a 
    CALL down ( UNI (i) , fu ) 
    DOWNS (i) = fu
   END DO 

   OPEN ( 20 , FILE = 'P6-22-23-res.dat' )
   WRITE ( 20 , * ) '# APARTAT1a : CÀLCUL INTEGRALS nu i nd' 
   DO N = 150, NDAMAX1a, 150 
    CALL STAT ( N , UPS , mitjana ,  desvest1 )
    I1 = ( XB - XA ) * mitjana 
    CALL STAT ( N , DOWNS , mitjana ,  desvest2 )
    I2 = ( XB - XA ) * mitjana 
    WRITE ( 20 , * ) N, I1, desvest1/((N)**0.5d0), I2, desvest2/((N)**0.5d0)
   END DO 

   WRITE ( 20 , * ) ' '
   WRITE ( 20 , * ) ' '

!---------------------------------------------------------------------------------------------------------------
   
! APARTAT 1b : GENERACIÓ DE 1000000 VALORS ALEATORIS SOTA DISTRIBUCIÓ p (x) amb x entre -L i L
   pi = 4.d0 * atan (1.d0) 
   L = pi
   XA = -L
   XB = L 
   M = 0.4d0 
   
   CALL acceptrebuig ( NDAMAX1bc , DADESbc , XA , XB , M , funcio1b )

!---------------------------------------------------------------------------------------------------------------

! APARTAT 1c : CÀLCUL INTEGRAL I2 AMB SAMPLEIG D'IMPORTÀNCIA 
   DO i = 1, NDAMAX1bc 
    CALL funcio1c ( DADESbc (i) , fu )
    GDADESbc (i) = fu 
   END DO


   WRITE ( 20 , * ) '# APARTAT1c : CÀLCUL INTEGRAL I2' 
   DO N = 10000, NDAMAX1bc, 10000 
    CALL STAT ( N , GDADESbc , mitjana ,  desvest2 )
    I2 = mitjana 
    WRITE ( 20 , * ) N, I2, desvest2/((N)**0.5d0)
   END DO 

   WRITE ( 20 , * ) ' '
   WRITE ( 20 , * ) ' '
  

!---------------------------------------------------------------------------------------------------------------

! APARTAT 2: FERMIONS
   DO i = 1, NDAMAX2 
    CALL psi2 ( DADESbc (i) , DADESbc (i+250000) , DADESbc (i+500000) , fu )  
    PSI2DADESbc (i) = fu 
   END DO 

   WRITE ( 20 , * ) '# APARTAT3 : CÀLCUL INTEGRAL I3' 
   DO N = 10000, NDAMAX2, 10000 
    CALL STAT ( N , PSI2DADESbc , mitjana ,  desvest1 )
    I1 = mitjana 
    WRITE ( 20 , * ) N, I1, desvest1/((N)**0.5d0)
   END DO 

   CLOSE (20)

END PROGRAM


! SUBRUTINES 
  SUBROUTINE up ( x , fu )
  IMPLICIT NONE
  DOUBLE PRECISION :: x , fu 
  fu = (( 5.109d0 ) / x )*(( 1.d0 - x )**3)*( x**(0.8002d0))
  END SUBROUTINE


  SUBROUTINE down ( x , fu )
  IMPLICIT NONE
  DOUBLE PRECISION :: x , fu 
  fu = (( 3.058d0 ) / x )*(( 1.d0 - x )**4)*( x**(0.803d0))
  END SUBROUTINE


  SUBROUTINE uniforme ( ndat , xdat, a , b  )
  IMPLICIT NONE 
  DOUBLE PRECISION :: xdat (ndat), a , b 
  INTEGER :: ndat, i 

  DO i = 1, ndat 
   xdat (i) = a + (b-a)*RAND()
  END DO  

  END SUBROUTINE 


  SUBROUTINE funcio1b ( x , fu ) 
  IMPLICIT NONE 
  DOUBLE PRECISION :: x , fu, L, pi 
  COMMON / CONSTANTS / pi, L 
  fu = ( 1.d0/L )*(( sin(( pi * ( x-L) )/( 2.d0*L )))**2 )
  END SUBROUTINE

  SUBROUTINE funcio1c ( x , fu )
  IMPLICIT NONE 
  DOUBLE PRECISION :: x, fu , L, pi 
  COMMON / CONSTANTS / pi, L 
  fu = (sin (( 8.d0*pi*(x-L) )/( 2.d0*L )))**2
  END SUBROUTINE 

  SUBROUTINE psi2 ( x1 , x2 , x3 , fu )
  IMPLICIT NONE	
  DOUBLE PRECISION :: x1 , x2 , x3 , fu , pi, L , psi
  COMMON / CONSTANTS / pi, L 

  psi = ((cos ((pi/2*L)*(x1 - L)))-(cos ((pi/2*L)*(x2 - L))))*&
  ((cos ((pi/2*L)*(x2 - L)))-(cos ((pi/2*L)*(x3 - L))))

  fu = (L**3)*psi**2
  END SUBROUTINE 

  SUBROUTINE STAT ( ndat , dat , pro , desvest )
  IMPLICIT NONE 
  DOUBLE PRECISION :: dat (ndat), pro, var, desvest, suma
  INTEGER :: ndat, i 

  suma = 0.d0
  DO i = 1, ndat 
   suma = suma + dat (i)
  END DO 
  pro = (1.d0/ndat) * suma 

  suma = 0.d0 
  DO i = 1, ndat 
   suma = suma + (dat (i))**2 
  END DO 
  var = (suma/ndat ) - pro**2 
  desvest = SQRT (var)
  END SUBROUTINE



  SUBROUTINE acceptrebuig (ndat,xnums,a,b,M,fun)
  IMPLICIT NONE  
  DOUBLE PRECISION :: xnums(ndat), a, b, M, x, p, fu
  INTEGER :: ndat, i

  DO i = 1, ndat
   x = (b-a)*RAND () + a
   p = M*RAND ()
  CALL fun (x,fu)
  DO WHILE (fu.LT.p)
   x = (b-a)*RAND () + a
   p = M*RAND ()
   CALL fun (x,fu)
  END DO 
  xnums (i) = x
  END DO 
  END SUBROUTINE