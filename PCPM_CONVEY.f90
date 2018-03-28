module PCPM_CONVEY

implicit none


real(kind=8),allocatable :: PCPM_N(:,:,:,:)
real(kind=8),allocatable :: PCPM_P(:,:,:,:)
real(kind=8),allocatable :: PCPM_Z(:,:,:,:)
real(kind=8),allocatable :: PCPM_D(:,:,:,:)

! forcing
real(kind=8),allocatable :: PCPM_T(:,:,:,:) !temperature
real(kind=8),allocatable :: PCPM_L(:,:,:,:) !light
real(kind=8),allocatable :: PCPM_KV(:,:,:)     ! diffuse coefficent

!depth
real(kind=8),allocatable :: PCPM_DEPTH(:,:,:)

end module
