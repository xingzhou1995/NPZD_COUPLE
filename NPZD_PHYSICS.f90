subroutine NPZD_PHYSICS(XITEM,YITEM)

use NPZD_input
use phy_parameter
use phy_process
implicit none
integer :: i,j,XITEM,YITEM


!call sinking(XITEM,YITEM)

call mixing(XITEM,YITEM)


end subroutine
