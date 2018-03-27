module  NPZD_input

!time and space
implicit none
save
integer :: ITEM    ! how many iteration steps
!integer :: DDAY    ! how many days 
! NPZD_time and space
real(kind=8) :: TSTART,TEND,dt
real(kind=8),allocatable :: dh(:,:)
!NPZD_IO
character(20) :: INPDIR,OUTDIR

! NPZD start and forcing file
character(20) :: NPZD_in,NPZD_T_in,NPZD_L_in,NPZD_out

!real(kind=8),allocatable :: array_N(:,:,:)
!real(kind=8),allocatable :: array_P(:,:,:)
!real(kind=8),allocatable :: array_Z(:,:,:)
!real(kind=8),allocatable :: array_D(:,:,:)

! forcing
!real(kind=8),allocatable :: array_T(:,:,:)
!real(kind=8),allocatable :: array_L(:,:,:)
!real(kind=8),allocatable :: KV(:,:)     ! diffuse coefficent

logical(kind=4) :: NPZD_SECONDS
logical(kind=4) :: RESTART_ON
real(kind=8) :: RESTART_INTERVAL

contains
subroutine day2seconds()

use bio_parameter
use phy_parameter

!bio parameter
r=r/86400; !p mortality
dd=dd/86400 !z mortality
gammap=gammap/86400 !phyto respiration coefficient
gammaz=gammaz/86400 !zoo   respiration coefficient
dr=dr/86400         !remineralization rate of detritus
GMAX=GMAX/86400     !maximum grazing rate by Z
upmax=upmax/86400   !maxium growth rate for P

!phy parameter
 !diffusion coefficent
A_N=A_N/86400
A_P=A_P/86400
A_Z=A_Z/86400
A_D=A_D/86400
 !sinking velocity
Wn=Wn/86400
Wp=Wp/86400
Wz=Wz/86400
Wd=Wd/86400
end subroutine




end module
