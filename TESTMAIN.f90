program TESTMAIN

!this main program is using for tested  subroutine NPZD


use PCPM_CONVEY
integer :: i,j,k
integer :: x,y,z,t





x=5
y=5
z=10
t=100

allocate(PCPM_N(x,y,z,t))
PCPM_N=0.0
allocate(PCPM_P(x,y,z,t))
PCPM_P=0.0
allocate(PCPM_Z(x,y,z,t))
PCPM_Z=0.0
allocate(PCPM_D(x,y,z,t))
PCPM_D=0.0
allocate(PCPM_T(x,y,z,t))
PCPM_T=18.0
allocate(PCPM_L(x,y,z,t))
PCPM_L=100.0
allocate(PCPM_KV(x,y,t))
PCPM_KV=1e-3
allocate(PCPM_DEPTH(x,y,t))
PCPM_DEPTH=20







do i=1,t

call transfer(i)

end do

end



 
