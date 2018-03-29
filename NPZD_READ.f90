!program NPZD_read
subroutine NPZD_read

use NPZD_CONVEY
use NPZD_input
use bio_parameter
use phy_process
implicit none
!character(20) :: NPZD_in,NPZD_T_in,NPZD_L_in,NPZD_out
!real(kind=8) :: TSTART,TEND,dt
real(kind=8) :: START_TIME,END_TIME,TIME_STEP
integer :: i,j,error
real :: ITEM1
namelist / NPZD_SELECT/ BIO_MODEL,NPZD_SECONDS
!namelist / NPZD_IO/ INPDIR,OUTDIR
!namelist / NPZD_RESTART/ RESTART_ON,RESTART_INTERVAL
!namelist / NPZD_data/ NPZD_in,NPZD_T_in,NPZD_L_in,NPZD_out
namelist / NPZD_time/ START_TIME,END_TIME,TIME_STEP
namelist / NPZD_bioprocess/ L_function,N_function,PM_function,ZM_function,R_function,G_function,PR_function,ZR_function,T_function 

open(33,file="NPZD.nml")
read(33,nml=NPZD_SELECT)
!read(33,nml=NPZD_IO)
!read(33,nml=NPZD_RESTART)
!read(33,nml=NPZD_data) 
read(33,nml=NPZD_time)
read(33,nml=NPZD_bioprocess)

WRITE(*,*) BIO_MODEL

! allocate Item
TSTART=START_TIME
TEND=END_TIME
dt=TIME_STEP
!write(*,*) TSTART
!write(*,*) TEND
!write(*,*) dt
if (TEND .le. TSTART) then
write(*,*) "error  NPZD_time,program terminated"
stop
!exit
end if
ITEM1=(TEND-TSTART)/dt
!DDAY =(TEND-TSTART)!/86400 ! in second divide 86400
!write(*,*),"ITEM1=",ITEM1
ITEM=ceiling(ITEM1)
write(*,*) "ITEM=",ITEM


if(.not.allocated( dh)) then
allocate(dh(XTOTAL,YTOTAL))
write(*,*) "ALLOCATED dh Array Successful!"
end if

!calculate dh
do i=1,XTOTAL
 do j=1,YTOTAL
  dh(i,j)=DEPTH(i,j)/LAYER
 end do
end do

write(*,*) "test 1 success"






!write(*,*) " How many days=",DDAY
write(*,*) "Iteration Number=",ITEM
!write(*,*) "LAYER=",LAYER

! read initial field

!do i=1,LAYER
!  read(44,*,iostat=error) array_N(1,i),array_P(1,i),array_Z(1,i),array_D(1,i)
!end do
!close(44)

!write(*,*) "N,P,Z,D data read successful"
 
! read T
!open(55,file=trim(INPDIR)//trim(NPZD_T_in))
!do i=1,LAYER
!  read(55,*,iostat=error) (array_T(j,i),j=1,DDAY+1)
!end do
!do i=1,Layer
!write(*,*) (array_T(j,i),j=1,2)
!end do
!close(55)

!write(*,*) "T read successful"

! read L
!open(66,file=trim(INPDIR)//trim(NPZD_L_in))
!  do i=1,ITEM
!  read(66,*,iostat=error) array_L
  !write(*,*) array_L(2,1)
!  end do
! remain calculateing the light intensity decay with depth undone

call light_decay()



if (NPZD_SECONDS) then
 call day2seconds()
end if



write(*,*) "L read successful"
end subroutine
