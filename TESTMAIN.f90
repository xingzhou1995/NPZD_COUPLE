program TESTMAIN

!this main program is using for tested  subroutine NPZD


use PCPM_CONVEY
integer :: i
integer :: x,y,z,t





x=5
y=5
z=130
t=100

allocate(PCPM_N(x,y,z,t))
allocate(PCPM_P(x,y,z,t))
allocate(PCPM_Z(x,y,z,t))
allocate(PCPM_D(x,y,z,t))
allocate(PCPM_T(x,y,z,t))
allocate(PCPM_L(x,y,z,t))
allocate(PCPM_KV(x,y,t))
allocate(PCPM_DEPTH(x,y,t))

do i=1,t

call transfer(i)

end



 
