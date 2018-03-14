subroutine transfer
!this subroutine is used for from transfer data from PCPM to NPZD model


use NPZD_CONVEY
implicit none

! allocate array in NPZD model
XTOTAL=10
YTOTAL=10
LAYER=130

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




end subroutine
