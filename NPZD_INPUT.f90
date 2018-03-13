module  NPZD_input

!time and space
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


logical(kind=4) :: RESTART_ON
real(kind=8) :: RESTART_INTERVAL


end module
