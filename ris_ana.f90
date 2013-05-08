!
! Written by TC, 2013
!

program ris_ana
  USE mconst
  IMPLICIT NONE

  ! dummy
  integer ios,i
  character*128 str
  logical ok

  !

  character*128 isofile(16)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  isofile=''
  i=1
  do
    call getarg(i,isofile(i))
    if(len_trim(isofile(i))==0)exit
    i=i+1
  enddo
  if(i==1)then
    do
      write(*,'(a,i3,a)') "Enter name", i, " (blank to proceed)"
      read(5,'(A128)') isofile(i)
      if(len_trim(isofile(i))==0)exit
      i=i+1
    enddo
  endif
  niso=i-1
  isofile(1:niso)=adjustl(isofile(1:niso))

  call lodiso(isofile(1:niso))



end program ris_ana
