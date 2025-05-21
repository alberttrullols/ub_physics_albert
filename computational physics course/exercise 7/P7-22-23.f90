PROGRAM EDOS 
IMPLICIT NONE 
INTEGER :: na , nb , nc , nd , nemax
PARAMETER ( na = 1900 , nb = 1900 , nc = 1900 , nd = 8000 , nemax = 25000)
DOUBLE PRECISION :: pi, t0, m, g, l, w, Tn, h, phi0, derphi0, E, V, K, Ea, Va, Ka, derphi0d1, derphi0d2 
DOUBLE PRECISION :: ta (na), phia_euler (na), derphia_euler (na), phia_adams (na), derphia_adams(na)
DOUBLE PRECISION :: tb (nb), phib_euler (nb), derphib_euler (nb), phib_adams (nb), derphib_adams(nb)
DOUBLE PRECISION :: tc (nc), phic_euler (nc), derphic_euler (nc), phic_adams (nc), derphic_adams(nc)
DOUBLE PRECISION :: td (nd), phid1_adams (nd), derphid1_adams(nd), phid2_adams (nd), derphid2_adams(nd)
DOUBLE PRECISION :: te (nemax),  phie_adams (nemax), derphie_adams(nemax)
INTEGER :: i , n , j ,  passos (4)
COMMON / CONSTANTS / m, g, l
DOUBLE PRECISION  :: EKIN, EPOT 
EXTERNAL funcio
DOUBLE PRECISION, DIMENSION (4,nemax) :: ENERGIES
DOUBLE PRECISION, DIMENSION (4,nemax) :: TEMPS

! CONSTANTS DE LA PRÀCTICA
 pi = 4.d0*ATAN (1.d0)
 t0 = 0.d0 
 m = 0.975d0
 g = 11.15d0 
 l = 1.14d0 
 w = SQRT (g/l)
 Tn = (2.d0*pi/w)


! APARTAT a) PETITES OSCIL·LACIONS
 
 ! SOTA LES CONDICIONS DEMANADES (phi petit, petites oscil·lacions) CRIDEM A LES DUES SUBRUTINES, EULER I ADAMS
 ! S'OBSERVA QUE LA COL·LECCIÓ DE PUNTS GENERADA AMB ADAMS S'AJUSTA MOLT MILLOR A LA FUNCIÓ ANALÍTICA SOLUCIÓ
 ! DE L'EQUACIÓ DIFERENCIAL QUAN CANVIEM sin(phi) per (phi)
 phi0 = 0.012
 derphi0 = 0.d0
 h = ((6.d0*Tn)-(t0))/na 
 CALL EULER ( h , t0 , phi0 , derphi0 , funcio , na , ta , phia_euler , derphia_euler)
 CALL ADAMS_BASHFORTH ( h , t0 , phi0 , derphi0 , funcio , na , ta , phia_adams , derphia_adams)
 OPEN (25, FILE = 'P7-22-23-res.dat')
 WRITE (25, *) '# PETITES OSCIL·LACIONS'
 DO i = 1, na 
  WRITE ( 25, * ) ta (i), phia_euler(i), phia_adams(i)
 END DO 
 WRITE (25,*) 
 WRITE (25,*)
 !-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

! APARTAT b) OSCIL·LACIONS GRANS

 ! BLOC ANÀLEG A L'ANTERIOR PERO AMB phi GRAN, OBTENIM DUES EVOLUCIONS MOLT DIFERENTS TANT EN FUNCIÓ DEL TEMPS COM
 ! AL COMPARAR LES TRAJECTÒRIES A L'ESPAI DE FASES 
 phi0 = pi - 0.015d0
 derphi0 = 0.d0
 h = ((6.d0*Tn)-(t0))/nb 
 CALL EULER ( h , t0 , phi0 , derphi0 , funcio , nb , tb , phib_euler , derphib_euler)
 CALL ADAMS_BASHFORTH ( h , t0 , phi0 , derphi0 , funcio , nb , tb , phib_adams , derphib_adams)
 WRITE (25, *) '# OSCIL·LACIONS GRANS'
 DO i = 1, nb 
  WRITE ( 25, * ) tb (i), phib_euler(i), phib_adams(i), derphib_euler(i), derphib_adams (i)
 END DO 

 WRITE (25,*) 
 WRITE (25,*)
 !-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

! APARTAT c) ENERGIES
 
 ! SOTA LES CONDICIONS DEMANADES GENEREM 4 COLECCIONS DE (2 PHI I 2 DERIVADA PHI) PHI I DERIVADA DE PHI. LES EMPREM PER A TROBAR
 ! LES ENERGIES CINÈTICA I TOTAL A PARTIR DE LES FUNCIONS EKIN I EPOT EN FUNCIÓ DEL TEMPS
 ! S'OBSERVA QUE EL MÈTODE D'ADAMS RETORNA UN RESULTAT MOLT MÉS SEMBLANT A UNA ENERGITA TOTAL CONSTANT A DIFERÈNCIA DEL MÈTODE EULER
 phi0 = pi - 0.035d0
 derphi0 = 0.2d0
 h = ((6.d0*Tn)-(t0))/nc 
 CALL EULER ( h , t0 , phi0 , derphi0 , funcio , nc , tc , phic_euler , derphic_euler)
 CALL ADAMS_BASHFORTH ( h , t0 , phi0 , derphi0 , funcio , nc , tc , phic_adams , derphic_adams)
 WRITE (25, *) '# ENERGIES'
 DO i = 1, nc 
  K = EKIN ( phic_euler (i) , derphic_euler (i)  )
  V = EPOT ( phic_euler (i)  , derphic_euler (i) )
  E = V + K 

  Ka = EKIN ( phic_adams (i) , derphic_adams (i) ) 
  Va = EPOT ( phic_adams (i) , derphic_adams (i) )
  Ea = Va + Ka

  WRITE ( 25, * ) tc (i), K, E, Ka, Ea
 END DO 
 WRITE (25,*) 
 WRITE (25,*)
 !-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

! APARTAT d) TRANSICIÓ 
 ! PER A DOS VALORS DE LA DERIVADA DE PHI GENEREM DOS COLECCIONS DE DADES AMB EL MÈTODE D'ADAMS, OBSERVEM LA TRANCISIÓ AL GRAFICAR
 ! A L'ESPAI DE FASES
 phi0 = 0.d0
 derphi0d1 = 2.d0*w + 0.07d0
 derphi0d2 = 2.d0*w - 0.07d0
 h = ((10.d0*Tn)-(t0))/nd
 CALL ADAMS_BASHFORTH ( h , t0 , phi0 , derphi0d1 , funcio , nd , td , phid1_adams , derphid1_adams) 
 CALL ADAMS_BASHFORTH ( h , t0 , phi0 , derphi0d2 , funcio , nd , td , phid2_adams , derphid2_adams) 

 WRITE (25,*) '#ESTUDI TRANSICIÓ'

 DO i = 1, nd 
  WRITE (25,*) phid1_adams(i), derphid1_adams(i), phid2_adams(i), derphid2_adams(i)
 END DO 

 WRITE (25,*)
 WRITE (25,*)
 !-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

! APARTAT e) CONVERGÈNCIA DEL MÈTODE 
 ! COMPAREM LA CONVERGÈNCIA DEL MÈTODE A PARTIR DE VEURE "COM DE CONSTANT" ÉS L'ENERGIA DEL PÈNDOL
 ! OBSERVEM QUE PER A N AL VOLTANT DE 1000, LA CONVERGÈNCIA ÉS BASTANT BONA, NO SERIA NECESSARI RECORRER A GRANS VALORS
 phi0 = 3.05d0
 derphi0 = 0.d0
 passos = [400,650,1250,25000]
 
 WRITE (25,*) '#RESULTATS ESTUDI CONVERGÈNCIA'
 
 ! PER A CADA VALOR DE LA LLISTA passos QUE CONTÉ EL NOMBRE DE PASSOS QUE VOLEM EMPRAR PER A EVALUAR EL MÈTODE
 DO j = 1, 4 
 n = passos (j)

 ! REDEFINIM EL PAS EN FUNCIÓ DEL NOMBRE DE PASSOS
 h = ((13.d0*Tn)-(t0))/n
 CALL ADAMS_BASHFORTH ( h , t0 , phi0 , derphi0 , funcio , n , te , phie_adams , derphie_adams )

  DO i = 1, n
   K = EKIN ( phie_adams (i) , derphie_adams (i)  )
   E = EPOT ( phie_adams (i)  , derphie_adams (i) ) + K 

   ! EMMAGATZEMEM A 2 MATRIUS ELS VALORS DE LA ENERGIA TOTAL PER A CADA TEMPS I ELS RESPECTIUS TEMPS
   ENERGIES (j,i) = E
   TEMPS (j,i) = te (i)
  END DO

 END DO 


! ESCRIVIM ELS VALORS DE E I t DE LES MATRIUS AL FITXER

 DO i = 1, nemax 
  IF (i.LE.400) THEN 
   WRITE (25,*) TEMPS (4,i) , ENERGIES (4,i),TEMPS (3,i) , ENERGIES (3,i), TEMPS (2,i) , ENERGIES (2,i),TEMPS (1,i) , ENERGIES (1,i)
  ELSEIF (i.LE.650.AND.i.GT.400) THEN 
   WRITE (25,*) TEMPS (4,i) , ENERGIES (4,i),TEMPS (3,i) , ENERGIES (3,i), TEMPS (2,i) , ENERGIES (2,i) 
  ELSEIF (i.LE.1250.AND.i.GT.650) THEN 
   WRITE (25,*) TEMPS (4,i) , ENERGIES (4,i),TEMPS (3,i) , ENERGIES (3,i)
  ELSE  
   WRITE (25,*) TEMPS (4,i) , ENERGIES (4,i)
  END IF
 END DO 


 CLOSE (25) 



END PROGRAM




SUBROUTINE ADAMS_BASHFORTH  ( h , x_0 , y_0 , dery_0, fun , n_max , x , y , dery )
 IMPLICIT NONE 
 DOUBLE PRECISION :: h , x_0 , y_0 , dery_0 , x (n_max) , y (n_max) , dery (n_max)
 DOUBLE PRECISION ::  y_ant1, y_ant2, x_ant2, x_ant1, dery_ant2, dery_ant1, f_ant1, f_ant2
 DOUBLE PRECISION :: y_current, x_current, dery_current
 INTEGER :: i, n_max

 ! LLEGENDA: TOT TERME AMB ant_j ON j = 1,2, REPRESENTA EL TERME ANTERIOR O DOS ITERACIONS ANTERIOR RESPECTIVAMENT
 ! SI CONSIDEREM _current COM A n+2, ant_1 ÉS n+1 I ant_2 ÉS n

 !INICIALITZEM AMB EULER ELS DOS PRIMERS TERMES DE x, y i dery
 y_ant2 = y_0
 x_ant2 = x_0
 
 CALL fun (x_ant2,y_ant2,f_ant2)

 dery_ant2 = dery_0 
 dery_ant1 = dery_ant2 + h*f_ant2

 y_ant1 = y_ant2 + h*dery_ant2

 x_ant1 = x_ant2 + h 

 x (1) = x_0
 y (1) = y_0
 dery (1) = dery_0
 y (2) = y_ant1
 x(2) = x_ant1
 dery (2) = dery_ant1

 DO i = 3, n_max 
  
  CALL fun ( x_ant1 , y_ant1 , f_ant1 )
  CALL fun ( x_ant2 , y_ant2 , f_ant2 )

  ! IMPLEMENTACIÓ ALGORISME ADAMS PER dery I y
  dery_current = dery_ant1 - (1.d0/2.d0)*h*f_ant2 + (3.d0/2.d0)*h*f_ant1 
  dery (i) = dery_current 

  y_current = y_ant1 - (1.d0/2.d0)*h*dery_ant2 + (3.d0/2.d0)*h*dery_ant1
  y(i) = y_current 

  x_current = x_ant1 + h 
  x (i) = x_current 

  ! REDEFINICIÓ DE CARA AL SEGÜENT PAS, ANOMENEM A CADA VARIABLE COM A LA UN PAS ANTERIOR
  dery_ant2 = dery_ant1
  dery_ant1 = dery_current 

  y_ant2 = y_ant1 
  y_ant1 = y_current

  x_ant2 = x_ant1 
  x_ant1 = x_current 

 END DO 

 END SUBROUTINE 

SUBROUTINE EULER ( h , x_0 , y_0 , dery_0, fun , n_max , x , y , dery )
 IMPLICIT NONE 
 DOUBLE PRECISION :: h, x_0, y_0, dery_0, x_current , y_current , dery_current , f_current, x_ant, y_ant, dery_ant,f_ant
 DOUBLE PRECISION :: x ( n_max ), y ( n_max ), dery ( n_max )
 INTEGER :: i, n_max 

 x_ant = x_0 
 x (1) = x_ant
 y_ant = y_0 
 y (1) = y_ant
 dery_ant = dery_0 
 dery (1) = dery_ant


 DO i = 2, n_max 

  CALL fun ( x_ant , y_ant , f_ant )
  
  dery_current = dery_ant + h*f_ant 
  dery (i) = dery_current

  y_current = y_ant + h*dery_ant
  y (i) = y_current 
  
  x_current = x_ant + h 
  x (i) = x_current
  


  x_ant = x_current
  y_ant = y_current 
  dery_ant = dery_current

 END DO  

 END SUBROUTINE 
 

SUBROUTINE funcio ( t , phi , fu )
 IMPLICIT NONE 
 DOUBLE PRECISION :: t , phi , m , g , l , fu
 COMMON / CONSTANTS / m, g, l 

 fu = -(g/l) * sin (phi)

 END SUBROUTINE 

DOUBLE PRECISION FUNCTION EKIN ( phi, derphi )
 IMPLICIT NONE 
 DOUBLE PRECISION :: phi, derphi, m, g, l 
 COMMON / CONSTANTS / m, g, l 
 EKIN = (0.5d0)*m*(l**2)*((derphi)**2)
 RETURN 
 END 

DOUBLE PRECISION FUNCTION EPOT ( phi , derphi )
 IMPLICIT NONE 
 DOUBLE PRECISION :: phi , derphi , m, g, l 
 COMMON / CONSTANTS / m, g, l  
 EPOT = -m*g*l*cos(phi)
 RETURN 
 END 