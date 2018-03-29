subroutine MAIN

use NPZD_CONVEY
use NPZD_input
use bio_parameter
implicit none
integer :: i,j,kk
real(kind=8) :: N,P,Z,D

! READ NPZD NAMELIST ONLY ONCE
if (NPZDREAD_CONTROL == 1) then 

write(*,*) "NPZD model start"
write(*,*) "Start to read input file"
call NPZD_READ

NPZDREAD_CONTROL=2
end if


do i= 1,ITEM 
 do j =1,XTOTAL
  do kk=1,YTOTAL
!do i=1,ITEM
call NPZD_BIOLOGY(j,kk)

call NPZD_PHYSICS(j,kk)
 


  end do
 end do
end do
!write program

  

end subroutine 

