      program MC3

      implicit none
      integer*4 seed, i, j, L, a, b, MCTOT, N, suma, DE
      integer*4 MCD, MCINI, NSEED
      real*8 genrand_real2
      real*8 energ, MAGNE
      real*8 ene, mag, enebis 
      real*8 x,y,z,TEMP
      real*8 W(-8:8)
      real*8 sum, sume, sume2, summ, sumam, summ2
      real*8 enepro, enepro2, magpro, magproabs, magpro2, vare, varm
      real*8 resultat(1:9)
      real*8 Tmin, Tmax, dT
      real*4 TIME1, TIME2
      character*30 DATE

      integer (kind=2), dimension(:,:), allocatable :: S
      integer (kind=4), dimension (:), allocatable :: PBC

      character*128 NOM
      character*32 sL,sTmin,sTmax,sdT,sMCTOT

      call getarg (1, sL)
      call getarg (2, sTmin)
      call getarg (3, sTmax)
      call getarg (4, sdT)
      call getarg (5, sMCTOT)

      read (sL,*) L
      read (sTmin,*) Tmin
      read (sTmax,*) Tmax
      read (sdT,*) dT
      read (sMCTOT,*) MCTOT

      allocate(S(1:L,1:L))
      allocate(PBC(0:L+1))

    
      CALL CPU_TIME(TIME1)

      write(NOM,'("SIM-L",I0,"-MCTOT",I0)') L, MCTOT


      MCINI=2000
      MCD=20
      N=L*L

      OPEN (UNIT=12, FILE = TRIM (NOM)//"_res.out")



c INICI DEL BUCLE DE TEMPERATURES
      TEMP=TMin
      do while (TEMP.le.TMax)

      W=0.d0
      do DE = -8,8
       W(DE)=dexp(-dfloat(DE)/TEMP)
      end do       
      


c INICIALITZACIÓ VARIABLES PROMIG

      enepro=0.d0
      enepro2=0.d0
      magpro=0.d0
      magproabs=0.d0
      magpro2=0.d0



c BUCLE SEEDS
      NSEED = 50
      do seed = 1, NSEED
      call init_genrand(seed)

c INICIALITZACIÓ VARIABLES DE SUMA 
      sum=0.d0
      sume=0.d0
      sume2=0.d0

      summ=0.d0
      sumam=0.d0
      summ2=0.d0



c MATRIU S
      do i = 1, L
       do j = 1, L
        if (genrand_real2().lt.0.5d0) then
         S(i,j)=1
        else
         S(i,j)=-1
        endif
       end do
      end do

      call PERIODIC_BOUNDARY_CONDITIONS (L,PBC)



      ene= energ (S,L,PBC)   


c ALGORITME METROPOLIS

      do i = 1, MCTOT
       do j = 1, N
      
        x=genrand_real2()
        y=genrand_real2()

        a=ceiling(x*L)
        b=ceiling(y*L) 

        suma=S(a,PBC(b+1))+S(a,PBC(b-1))+S(PBC(a+1),b)+S(PBC(a-1),b)
        DE=2*S(a,b)*suma

        if (DE.le.0) then
         S(a,b)=-S(a,b)
         ene = ene + DE

        else
         z=genrand_real2()

         if (z.lt.W(DE)) then
          S(a,b)=-S(a,b)
          ene = ene + DE
         end if

        end if 
        end do

        if ((i.gt.MCINI).and.(MCD*(i/MCD).eq.i)) then
         mag=MAGNE(S,L)
         ene=energ(S,L,PBC)

         sum=sum+1.d0
         sume=sume+ene
         sume2=sume2+ene*ene

         summ=summ+mag
         sumam=sumam+abs(mag)
         summ2=summ2+mag*mag

        end if

      end do 

c càlcul dels promitjos

      enepro=enepro+sume/sum
      enepro2=enepro2+sume2/sum
      magpro=magpro+summ/sum
      magproabs=magproabs+sumam/sum
      magpro2=magpro2+summ2/sum

      end do

c NORMALITZACIÓ DELS PROMITJOS PER SEED


      enepro=enepro/NSEED
      enepro2=enepro2/NSEED
      magpro=magpro/NSEED
      magproabs=magproabs/NSEED
      magpro2=magpro2/NSEED
      

      vare=enepro2-(enepro*enepro)
      varm=magpro2-(magpro*magpro)



c VARIABLES PER PARTICULA
      enepro=enepro/N
      enepro2=enepro2/(N*N)
      magpro=magpro/N
      magproabs=magproabs/N
      magpro2=magpro2/(N*N)

      vare=vare/(N*N)
      varm=varm/(N*N)


      resultat(1)=N
      resultat(2)=TEMP
      resultat(3)=enepro
      resultat(4)=enepro2
      resultat(5)=magpro
      resultat(6)=magproabs
      resultat(7)=magpro2
      resultat(8)=vare
      resultat(9)=varm


      write(12,*) resultat
      TEMP=TEMP+dT

      end do
      CLOSE(12)


      CALL CPU_TIME (TIME2)
      CALL FDATE(DATE)      

      write(*,*) DATE
      write(*,*) 'CPUTIME= ', TIME2-TIME1



      end












c definim subrutina que genera el vector PBC
       subroutine PERIODIC_BOUNDARY_CONDITIONS (L,PBC)
       implicit none
       integer*2 S(1:L,1:L)
       integer*4 i,j,L
       integer*4 PBC(0:L+1)

       PBC=0

       do i = 1, L
        PBC(i)=i
       end do 

       PBC(0)=L
       PBC(L+1)=1

       end

c definim la function energia
      real*8 function energ (S,L,PBC)
      implicit none
      integer*2 S(1:L,1:L)
      integer*4 i,j,L
      integer*4 PBC(0:L+1)
      real*8 ene

      ene=0.d0

      do i = 1,L
       do j = 1,L
        ene = ene - S(i,j)*S(PBC(i+1),j)-S(i,j)*S(i,PBC(j+1))
       end do
      end do 



      energ=ene
      return 
      end

c definim la funció MAGNE
      real*8 function MAGNE(S,L)
      integer*2 S(1:L,1:L)
      integer*4 i,j,L
      real*8 MAG

      MAG=0.d0
      do i=1,L
       do j=1,L
        mag= mag+S(i,j)
       end do
      end do

      MAGNE=MAG
      return 
      end

