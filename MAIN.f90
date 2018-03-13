program MAIN

use NPZD_CONVEY
use NPZD_input
use bio_parameter
implicit none
integer :: i,j,k
real(kind=8) :: N,P,Z,D

write(*,*) "NPZD model start"
write(*,*) "Start to read input file"
call NPZD_READ

do i= 1,ITEM
 do j =1,XTOTAL
  do k=1,YTOTAL
!do i=1,ITEM
call NPZD_BIOLOGY(j,k)

call NPZD_PHYSICS(j,k)
 


  end do
 end do
end do
!write program

  

end 

