subroutine transfer (time)
!this subroutine is used for from transfer data from PCPM to NPZD model

use PCPM_CONVEY
use NPZD_CONVEY
implicit none
integer :: real(kind=8) time
integer :: i,j,k


! allocate array in NPZD model
XTOTAL = size(PCPM_N,1) !x
YTOTAL = size(PCPM_N,2) !y
LAYER  = size(PCPM_N,3) !z

if(.not.allocated( array_N)) then 
allocate(array_N(XTOTAL,YTOTAL,LAYER))
write(*,*),"ALLOCATED N Array Successful!"
end if

if(.not.allocated( array_P)) then
allocate(array_P(XTOTAL,YTOTAL,LAYER))
write(*,*),"ALLOCATED P Array Successful!"
end if

if(.not.allocated( array_Z)) then
allocate(array_Z(XTOTAL,YTOTAL,LAYER))
write(*,*),"ALLOCATED Z Array Successful!"
end if

if(.not.allocated( array_D)) then
allocate(array_D(XTOTAL,YTOTAL,LAYER))
write(*,*),"ALLOCATED D Array Successful!"
end if

if(.not.allocated( array_T)) then
allocate(array_T(XTOTAL,YTOTAL,LAYER))
write(*,*),"ALLOCATED T Array Successful!"
end if

if(.not.allocated( array_L)) then
allocate(array_L(XTOTAL,YTOTAL,LAYER))
write(*,*),"ALLOCATED L Array Successful!"
end if

if(.not.allocated( array_KV)) then
allocate(KV(XTOTAL,YTOTAL))
write(*,*),"ALLOCATED KV Array Successful!"
end if


if(.not.allocated( array_DEPTH)) then
allocate(DEPTH(XTOAL,YTOTAL))
write(*,*),"ALLOCATED DEPTH Array Successful!"
end if

!Transfer data from PCPM to NPZD
do i =1,XTOTAL
  do j=1,YTOTAL
   do k=1,LAYER
     array_N(i,j,k)=PCPM_N(i,j,k,time)
     array_P(i,j,k)=PCPM_P(i,j,k,time)
     array_Z(i,j,k)=PCPM_Z(i,j,k,time)
     array_D(i,j,k)=PCPM_D(i,j,k,time)
     array_T(i,j,k)=PCPM_T(i,j,k,time)
     array_L(i,j,k)=PCPM_L(i,j,k,time)
   end do
  end do
end do
     
do i=1,XTOTAL
 do j=1,YTOTAL
 KV(i,j)=PCPM_KV(i,j,time)
 DEPTH(i,j)=PCPM_DEPTH(i,j,time)
 end do
end do
  


!Call NPZD
call MAIN()
!Transfer data from NPZD to PCPM
 
do i =1,XTOTAL
  do j=1,YTOTAL
   do k=1,LAYER
   PCPM_N(i,j,k,time)=array_N(i,j,k)  
   PCPM_P(i,j,k,time)=array_P(i,j,k)
   PCPM_Z(i,j,k,time)=array_Z(i,j,k)
   PCPM_D(i,j,k,time)=array_D(i,j,k) 
    end do
  end do
end do







end subroutine
