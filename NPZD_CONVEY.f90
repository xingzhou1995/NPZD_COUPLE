module NPZD_CONVEY

!this module using for convey the data from PCPM to NPZD model

integer :: XTOTAL  ! how many grids in X direction
integer :: YTOTAL  ! how many grids in Y direction
integer,allocatable :: LAYER(:,:)   ! how many layers in depth

! initial field
real(kind=8),allocatable :: array_N(:,:,:)
real(kind=8),allocatable :: array_P(:,:,:)
real(kind=8),allocatable :: array_Z(:,:,:)
real(kind=8),allocatable :: array_D(:,:,:)

! forcing
real(kind=8),allocatable :: array_T(:,:,:) !temperature
real(kind=8),allocatable :: array_L(:,:,:) !light
real(kind=8),allocatable :: KV(:,:)     ! diffuse coefficent

!depth
real(kind=8),allocatable :: DEPTH(:,:)


end module 
