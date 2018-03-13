module phy_process
contains
subroutine light_decay()

use NPZD_CONVEY
use NPZD_INPUT
use phy_parameter
implicit none
integer :: i,j,k
real(kind=8) :: tmp

! no decay
!do i=1,LAYER
! do j=1,ITEM+1
! tmp=array_L(j,1)
! array_L(j,i)=tmp
! end do
!end do

! decay chen
 do i=1,XTOTAL
  do j=1,YTOTAL
   do k=1,LAYER(i,j)
 array_L(i,j,1)=array_L(i,j,1)*transferlight
 tmp=array_L(i,j,1)*exp(-kext*(k-1)*dh(i,j))
 array_L(i,j,k)=tmp
!write(*,*) array_L(j,i)
  end do
 end do
end do
!write(*,*) array_L

end subroutine

subroutine sinking(XITEM,YITEM)

use NPZD_CONVEY
use NPZD_input
use phy_parameter
implicit none
integer :: j,XITEM,YITEM
!,sink_controln,sink_controlp,sink_controlz,sink_controld
real(kind=8) :: N1(LAYER(XITEM,YITEM)),P1(LAYER(XITEM,YITEM)),Z1(LAYER(XITEM,YITEM)),D1(LAYER(XITEM,YITEM))
real(kind=8) :: N2(LAYER(XITEM,YITEM)),P2(LAYER(XITEM,YITEM)),Z2(LAYER(XITEM,YITEM)),D2(LAYER(XITEM,YITEM))
!real(kind=8) :: N3(LAYER),P3(LAYER),Z3(LAYER),D3(LAYER)

!sink_controln = 1
!sink_controlp = 1
!sink_controlz = 1
!sink_controld = 1



!read data
do j=1,LAYER(XITEM,YITEM)
N1(j)=array_N(XITEM,YITEM,j)
!N1(j)=array_N(time+1,j)
P1(j)=array_P(XITEM,YITEM,j)
!P1(j)=array_P(time+1,j)
Z1(j)=array_Z(XITEM,YITEM,j)
!Z1(j)=array_Z(time+1,j)
D1(j)=array_D(XITEM,YITEM,j)
!D1(j)=array_D(time+1,j)
end do

do j=1,LAYER(XITEM,YITEM)
N2(j)=0
P2(j)=0
Z2(j)=0
D2(j)=0
end do

!#########################################################

!write(*,*) "Z2=",Z2
!write(*,*) "Z2(2)=",Z2(2)
!write(*,*) "Z1=",Z1
!write(*,*)  sink_controlz
!write(*,*)  Wz
!N2(1)=N1(1)-Wn*(dt/dh)*((N2(1)+N2(2))/2)
!P3(1)=P2(1)-sink_controlp*Wp*(dt/dh)*((P2(1)+P2(2))/2)
!Z3(1)=Z2(1)-sink_controlz*Wz*(dt/dh)*((Z2(1)+Z2(2))/2)
!D3(1)=D2(1)-sink_controld*Wd*(dt/dh)*((D2(1)+D2(2))/2)
!call sink_control(N3(1),P3(1),Z3(1),D3(1),sink_controln,sink_controlp,sink_controlz,sink_controld)



!upwind scheme
N2(1)=N1(1)-Wn*(dt/dh(XITEM,YITEM))*(N1(1)-0)
P2(1)=P1(1)-Wp*(dt/dh(XITEM,YITEM))*(P1(1)-0)
Z2(1)=Z1(1)-Wz*(dt/dh(XITEM,YITEM))*(Z1(1)-0)
D2(1)=D1(1)-Wd*(dt/dh(XITEM,YITEM))*(D1(1)-0)

Write(*,*) "#############SINKING##################"
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",1,"N=",N2(1)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",1,"P=",P2(1)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",1,"Z=",Z2(1)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",1,"D=",D2(1)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",1,"SUM=",N2(1)+P2(1)+Z2(1)+D2(1)
write(*,*) "#############SINKING##################"





do j=2,LAYER(XITEM,YITEM)-1
!N3(j)=N1(j)-sink_controln*Wn*(dt/dh)*(N2(j+1)-N2(j-1))
!P3(j)=P1(j)-sink_controlp*Wp*(dt/dh)*(P2(j+1)-P2(j-1))
!Z3(j)=Z1(j)-sink_controlz*Wz*(dt/dh)*(Z2(j+1)-Z2(j-1))
!D3(j)=D1(j)-sink_controld*Wd*(dt/dh)*(D2(j+1)-D2(j-1))

!call sink_control(N3(j),P3(j),Z3(j),D3(j),sink_controln,sink_controlp,sink_controlz,sink_controld)

N2(j)=N1(j)-Wn*(dt/dh(XITEM,YITEM))*(N1(j)-N1(j-1))
P2(j)=P1(j)-Wp*(dt/dh(XITEM,YITEM))*(P1(j)-P1(j-1))
Z2(j)=Z1(j)-Wz*(dt/dh(XITEM,YITEM))*(Z1(j)-Z1(j-1))
D2(j)=D1(j)-Wd*(dt/dh(XITEM,YITEM))*(D1(j)-D1(j-1))


Write(*,*) "#############SINKING##################"
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",j,"N=",N2(j)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",j,"P=",P2(j)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",j,"Z=",Z2(j)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",j,"D=",D2(j)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",j,"SUM=",N2(j)+P2(j)+Z2(j)+D2(j)
write(*,*) "#############SINKING##################"



end do

!N(LAYER)=N1(LAYER)+sink_controln*Wn*(dt/dh)*((N2(LAYER)+N2(LAYER-1))/2)
!3(LAYER)=P1(LAYER)+sink_controlp*Wp*(dt/dh)*((P2(LAYER)+P2(LAYER-1))/2)
!3(LAYER)=Z1(LAYER)+sink_controlz*Wz*(dt/dh)*((Z2(LAYER)+Z2(LAYER-1))/2)
!3(LAYER)=D1(LAYER)+sink_controld*Wd*(dt/dh)*((D2(LAYER)+D2(LAYER-1))/2)

!write(*,*) "Z3(LAYER)=",time,Z3(LAYER)
!write(*,*) "Z1(LAYER)=",time,Z1(LAYER)
!write(*,*) "Z2(LAYER)=",time,Z2(LAYER)
!write(*,*) "Z2(LAYER-1)=",time,Z2(LAYER-1)
!do j=1,LAYER
!write(*,*) time,Z3(j)
!end do
!call sink_control(N3(LAYER),P3(LAYER),Z3(LAYER),D3(LAYER),sink_controln,sink_controlp,sink_controlz,sink_controld)
!do j=1,LAYER
!write(*,*) time,Z3(j)
!end do

!upwind scheme


N2(LAYER(XITEM,YITEM))=sum(N1)-sum(N2)

P2(LAYER(XITEM,YITEM))=sum(P1)-sum(P2)

Z2(LAYER(XITEM,YITEM))=sum(Z1)-sum(Z2)

D2(LAYER(XITEM,YITEM))=sum(D1)-sum(D2)


Write(*,*) "#############SINKING##################"
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",LAYER,"N=",N2(LAYER)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",LAYER,"P=",P2(LAYER)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",LAYER,"Z=",Z2(LAYER)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",LAYER,"D=",D2(LAYER)
!Write(*,*) "TIME=",TSTART+(time-1)*dt,"LAYER=",LAYER,"SUM=",N2(LAYER)+P2(LAYER)+Z2(LAYER)+D2(LAYER)
write(*,*) "#############SINKING##################"

write(*,*) "SUM=",sum(N2)+sum(P2)+sum(Z2)+sum(D2)

!do j=1,LAYER

!if (N3(j).le.0) then
!sink_controln = 0
!N3(j) = sink_min
!end if

!if (P3(j).le.0) then
!sink_controlp = 0
!P3(j) = sink_min
!end if

!if (Z3(j).le.0) then
!sink_controlz = 0
!Z3(j) = sink_min
!end if

!if (D3(j).le.0) then
!sink_controld = 0
!D3(j) = sink_min
!end if

!end do



!#######################################

!write(*,*) "Z3=",Z3
do j=1,LAYER(XITEM,YITEM)

   array_N(XITEM,YITEM,j)=N2(j)
   array_P(XITEM,YITEM,j)=P2(j)
   array_Z(XITEM,YITEM,j)=Z2(j)
   array_D(XITEM,YITEM,j)=D2(j)
end do








!deallocate(N1)
!deallocate(N2)
!deallocate(N3)
!deallocate(P1)
!deallocate(P2)
!deallocate(P3)
!deallocate(Z1)
!deallocate(Z2)
!deallocate(Z3)
!deallocate(D1)
!deallocate(D2)
!deallocate(D3)









end subroutine


subroutine sink_control(N,P,Z,D,sink_controln,sink_controlp,sink_controlz,sink_controld)

use phy_parameter
implicit none
real(kind=8) :: N,P,Z,D
integer :: sink_controln,sink_controlp,sink_controlz,sink_controld


sink_controln = 1
sink_controlp = 1
sink_controlz = 1
sink_controld = 1



if (N.le.0) then
sink_controln = 0
N = sink_min
end if

if (P.le.0) then
sink_controlp = 0
P = sink_min
end if

if (Z.le.0) then
sink_controlz = 0
Z = sink_min
end if

if (D.le.0) then
sink_controld = 0
D = sink_min
end if



end subroutine



subroutine mixing(XITEM,YITEM)
use NPZD_CONVEY
use NPZD_input
use phy_parameter
implicit none
integer :: i,j,XITEM,YITEM
!real(kind=8) :: mix_dt
real(kind=8) ::  S_N,S_P,S_Z,S_D
real(kind=8) ::  BELOW_N(LAYER(XITEM,YITEM)),BELOW_P(LAYER(XITEM,YITEM)),BELOW_Z(LAYER(XITEM,YITEM)),BELOW_D(LAYER(XITEM,YITEM))
real(kind=8) ::  MAIN_N(LAYER(XITEM,YITEM)),MAIN_P(LAYER(XITEM,YITEM)),MAIN_Z(LAYER(XITEM,YITEM)),MAIN_D(LAYER(XITEM,YITEM))
real(kind=8) ::  ABOVE_N(LAYER(XITEM,YITEM)),ABOVE_P(LAYER(XITEM,YITEM)),ABOVE_Z(LAYER(XITEM,YITEM)),ABOVE_D(LAYER(XITEM,YITEM))
real(kind=8) ::  N1(LAYER(XITEM,YITEM)),P1(LAYER(XITEM,YITEM)),Z1(LAYER(XITEM,YITEM)),D1(lAYER(XITEM,YITEM))
real(kind=8) ::  N2(LAYER(XITEM,YITEM)),P2(LAYER(XITEM,YITEM)),Z2(LAYER(XITEM,YITEM)),D2(lAYER(XITEM,YITEM))
!intial value

do i=1,LAYER(XITEM,YITEM)

N1(i)=array_N(XITEM,YITEM,i)
P1(i)=array_P(XITEM,YITEM,i)
Z1(i)=array_Z(XITEM,YITEM,i)
D1(i)=array_D(XITEM,YITEM,i)

!debug option

!N1(i)=array_N(time,i)
!P1(i)=array_P(time,i)
!Z1(i)=array_Z(time,i)
!D1(i)=array_D(time,i)

end do

!write(*,*) N1

!dh = 1 !space step
!dt = 1 !time step
!A  = 2 ! diffuse coefficient

!mix_dt=1
S_N  = KV(XITEM,YITEM)*((dt)/(dh(XITEM,YITEM)**2))
S_P  = KV(XITEM,YITEM)*((dt)/(dh(XITEM,YITEM)**2))
S_Z  = KV(XITEM,YITEM)*((dt)/(dh(XITEM,YITEM)**2))
S_D  = KV(XITEM,YITEM)*((dt)/(dh(XITEM,YITEM)**2))
!S  = A*((mix_dt)/(dh*dh))

BELOW_N(1)= 0
BELOW_N(LAYER(XITEM,YITEM))= (-1)*S_N
ABOVE_N(1) = (-1)*S_N
!ABOVE(1) = S
ABOVE_N(LAYER(XITEM,YITEM)) = 0
MAIN_N(1) = 1+S_N
MAIN_N(LAYER(XITEM,YITEM)) = 1+S_N
!MAIN(1)=1-S


BELOW_P(1)= 0
BELOW_P(LAYER(XITEM,YITEM))= (-1)*S_P
ABOVE_P(1) = (-1)*S_P
!ABOVE(1) = S
ABOVE_P(LAYER(XITEM,YITEM)) = 0
MAIN_P(1) = 1+S_P
MAIN_P(LAYER(XITEM,YITEM)) = 1+S_P
!MAIN(1)=1-S


BELOW_Z(1)= 0
BELOW_Z(LAYER(XITEM,YITEM))= (-1)*S_Z
ABOVE_Z(1) = (-1)*S_Z
!ABOVE(1) = S
ABOVE_Z(LAYER(XITEM,YITEM)) = 0
MAIN_Z(1) = 1+S_Z
MAIN_Z(LAYER(XITEM,YITEM)) = 1+S_Z
!MAIN(1)=1-S

BELOW_D(1)= 0
BELOW_D(LAYER(XITEM,YITEM))= (-1)*S_D
ABOVE_D(1) = (-1)*S_D
!ABOVE(1) = S
ABOVE_D(LAYER(XITEM,YITEM)) = 0
MAIN_D(1) = 1+S_D
MAIN_D(LAYER(XITEM,YITEM)) = 1+S_D
!MAIN(1)=1-S


do i=2,LAYER(XITEM,YITEM)-1
BELOW_N(i) = (-1)*S_N
MAIN_N(i)  = 1+2*S_N
ABOVE_N(i) = (-1)*S_N
end do

do i=2,LAYER(XITEM,YITEM)-1
BELOW_P(i) = (-1)*S_P
MAIN_P(i)  = 1+2*S_P
ABOVE_P(i) = (-1)*S_P
end do

do i=2,LAYER(XITEM,YITEM)-1
BELOW_Z(i) = (-1)*S_Z
MAIN_Z(i)  = 1+2*S_Z
ABOVE_Z(i) = (-1)*S_Z
end do

do i=2,LAYER(XITEM,YITEM)-1
BELOW_D(i) = (-1)*S_D
MAIN_D(i)  = 1+2*S_D
ABOVE_D(i) = (-1)*S_D
end do


!write(*,*) BELOW
!write(*,*) MAIN
!write(*,*) ABOVE



call solve_tradiag(BELOW_N,MAIN_N,ABOVE_N,N1,N2,LAYER(XITEM,YITEM))
call solve_tradiag(BELOW_P,MAIN_P,ABOVE_P,P1,P2,LAYER(XITEM,YITEM))
call solve_tradiag(BELOW_Z,MAIN_Z,ABOVE_Z,Z1,Z2,LAYER(XITEM,YITEM))
call solve_tradiag(BELOW_D,MAIN_D,ABOVE_D,D1,D2,LAYER(XITEM,YITEM))

 !do j=1,LAYER
 ! N1(j)=N2(j);
 ! P1(j)=P2(j);
 ! Z1(j)=Z2(j);
 ! D1(j)=D2(j);
 !end do
!end do

do j=1,LAYER(XITEM,YITEM)

   array_N(XITEM,YITEM,j)=N2(j)
   array_P(XITEM,YITEM,j)=P2(j)
   array_Z(XITEM,YITEM,j)=Z2(j)
   array_D(XITEM,YITEM,j)=D2(j)
end do

write(*,*) "##############mixing##########"
!write(*,*) "time=",time,N2
!write(*,*) "time=",time,P2
!write(*,*) "time=",time,Z2
!write(*,*) "time=",time,D2

!write(*,*) N2

end subroutine



subroutine solve_tradiag(a,bb,c,d,x,n)
implicit none
! a - sub-diagonal below the main diagonal
! b - the main diagonal
! c - suo-diagonal
! d - right part
! x - the answer
! n -number of equations

integer :: n
real(kind=8),dimension(n) :: a,b,c,d,bb
real(kind=8),dimension(n) :: x
!real(kind=8),dimension(n) :: cp,dp
!real(8) :: m

integer i

do i=1,n
b(i)=bb(i)
end do




!write(*,*) "BELOW=",a
!write(*,*) "MAIN=",b
!write(*,*) "ABOVE=", c
!write(*,*) "N1=",d






! initialize c-prime and d-prime
!cp(1) = c(1)/b(1)
!dp(1) = d(1)/b(1)
! solve vectors c-prime and d-prime
do i=2,n
 !write(*,*) "b(i)=",b(i)
 !write(*,*) "a(i)=",a(i)
 !write(*,*) "c(i-1)=",c(i-1)
 !write(*,*) "d(i)=",d(i-1),d(i)
 b(i)=b(i)-((a(i)/b(i-1))*c(i-1))
 d(i)=d(i)-((a(i)/b(i-1))*d(i-1))

!write(*,*) "i=",i,"b=",b(i)
!write(*,*) "i=",i,"d=",d(i)
end do


!do i= 2,n
! m=b(i)-cp(i-1)*a(i)
! cp(i)=c(i)/m
! dp(i)=(d(i)-dp(i-1)*a(i))/m
!end do

!intialize x
x(n)=d(n)/b(n)

!write(*,*) x(n)

!solve for x from the vectors c-prime and d-prime

do i=n-1,1,-1
   x(i)=(d(i)-(c(i)*x(i+1)))/b(i)


! do i = n-1,1,-1
! x(i)=dp(i)-cp(i)*x(i+1)
 end do

!write(*,*) x

end subroutine solve_tradiag

end module
