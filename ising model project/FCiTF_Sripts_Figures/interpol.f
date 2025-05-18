       program interpol
       implicit none
       integer :: Num, i, M, K 
       real :: N, x, y, z, deltaT
       real :: polinomi

       real (kind=4), dimension (:), allocatable :: T
       real (kind=4), dimension (:), allocatable :: ep
       real (kind=4), dimension (:), allocatable :: ep2
       real (kind=4), dimension (:), allocatable :: mp
       real (kind=4), dimension (:), allocatable :: mpa
       real (kind=4), dimension (:), allocatable :: mp2
       real (kind=4), dimension (:), allocatable :: ve
       real (kind=4), dimension (:), allocatable :: vm
       real (kind=4), dimension (:), allocatable :: cv
       real (kind=4), dimension (:), allocatable :: susceptibilitat



      
       character*32 sNum, sfile

       call getarg (1, sNum)
       call getarg (2, sfile)

       read (sNum,*) Num

       allocate(T(1:Num))
       allocate(ep(1:Num))
       allocate(ep2(1:Num))
       allocate(mp(1:Num))
       allocate(mpa(1:Num))
       allocate(mp2(1:Num))
       allocate(ve(1:Num))
       allocate(vm(1:Num))
       allocate(cv(1:Num))
       allocate(susceptibilitat(1:Num))




       open (21, file = sfile)

       do i = 1, Num
        read (21,*) N, T(i), ep(i), ep2(i), mp(i), mpa(i), mp2(i), ve(i)
       end do 

       close (21)


       do i = 1, Num
        cv (i) = N*(ve(i)/(T(i)**2))
       end do 


       do i = 1, Num
        susceptibilitat (i) = N*((mp2(i)-(mpa(i))**2)/(T(i)))
       end do 

       x = T(1)
       deltaT= T(Num) - x 
       M = 1000

       open(22, file='interpol.out')

       
       do i = 1, M
        x = x + (deltaT/M)
        y = polinomi (T,cv,Num,x)
        z = polinomi (T,susceptibilitat,Num,x)

        write(22,*) x,y,z

       end do 
       close(22)

       end program

       REAL FUNCTION polinomi (x,y,N,xp)
       implicit none
       integer :: N, i, j
       real:: x(1:N), y(1:N)
       real :: xp, product, resultat

       resultat = 0.d0


       do i = 1, N
        product = y(i)
        do j = 1, N
         if (i.ne.j) then
          product=product*(xp-x(j))/(x(i)-x(j))
         end if
        end do
        resultat = resultat + product
       end do 

       polinomi=resultat
       return
       end 




