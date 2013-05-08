!
! Written by TC, 2013 (with many lines from GRASP)
!

program ris_ana
  USE mconst
  IMPLICIT NONE

  ! dummy
  integer ios,i,j,k,k1,k2,k3,fi,bl1,bl2
  real(kind(1d0)) w1,w2,w3,w4,w5,w6
  real(kind(1d0)) F,M
  character*128 str,vstr(16,2)
  logical ok

  !

  character*128 fname(16,2)
  real(kind(1d0)) en(64,8,16,2)
  integer nblock(2)
  CHARACTER g92mix*6

  !
  real(kind(1d0)) ua2cminv, ua2Ghz, u2uam,a0fm

  !
  character*4 JFraction(45)
  character*1 PlusMinus(3)
  DATA PlusMinus/'-', ' ', '+'/
  DATA JFraction/'  0 ', ' 1/2', '  1 ', ' 3/2', '  2 ', ' 5/2',     &
                 '  3 ', ' 7/2', '  4 ', ' 9/2', '  5 ', '11/2', &
                 '  6 ', '13/2', '  7 ', '15/2', '  8 ', '17/2', &
                 '  9 ', '19/2', ' 10 ', &
                 '21/2', ' 11 ', '23/2', ' 12 ', '25/2', &
                 ' 13 ', '27/2', ' 14 ', '29/2', ' 15 ', '31/2', &
                 ' 16 ', '33/2', ' 17 ', '35/2', ' 18 ', '37/2', &
                 ' 19 ', '39/2', ' 20 ', '41/2', ' 21 ', '43/2', &
                 ' 22 '/

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  fname=''
  en=0.d0
  nblock=0

  ! Read infos

  write(*,*) "Enter names corresponding to decreasing A"
  write(*,*) "It will be assumed that the names for lower and upper states &
              correspond to the same isotopes"
  write(*,*) "Entering 2 isotopes leads to the determination of M as a function of F"
  write(*,*) "If you enter 3 or more isotopes, the program determines M and F for &
              all possible triplets of isotopes"
  do fi=1,2
    i=1
    do
      if(fi==1)then
        write(*,'(a,i3,a)') "Enter name", i, " for lower state (blank to proceed)"
      else
        write(*,'(a,i3,a)') "Enter name", i, " for upper state (blank to proceed)"
      endif
      read(5,'(A128)') str
      if(len_trim(str)==0)then
        if(i>1)then
          exit
        else
          cycle
        endif
      endif
      fname(i,fi)=str
      i=i+1
    enddo
    i=i-1
    fname(1:i,fi)=adjustl(fname(1:i,fi))
    if(fi==1)niso=i
  enddo
  !!!!!!!!!!!!!!!!!!

  vstr=''
  do fi=1,2
    do i=1,niso
      vstr(i,fi)=trim(fname(i,fi))//'.iso' 
    enddo
  enddo
  call lodiso(vstr(1:niso,:))

  en=0.d0
  do fi=1,2
    do i=1,niso
      OPEN (3, FILE = trim(fname(i,fi))//'.cm', FORM = 'UNFORMATTED', STATUS = 'OLD')
      READ (3) g92mix
      IF (g92mix .NE. 'G92MIX') THEN
        WRITE (0,*) 'Not a mixing coefficient file, skipping "', &
                      trim(fname(i,fi)), '"'

        CLOSE (3)
        CYCLE
      ENDIF

      READ (3) k1, k1, k1, k1, k1, nblock(fi)
      do j=1,nblock(fi)
        READ (3) k1,k2,k3
        if (k1/=j)then
          WRITE (0,*) 'jblock .NE. nb, stopping...'
          CLOSE (3)
          STOP
        endif
        IF (k3 .LE. 0) CYCLE
        READ(3)
        READ(3) w1, (en(k,j,i,fi), k=1,k3)
        READ(3)
        en(:k3,j,i,fi)=en(:k3,j,i,fi)+w1
      enddo
      CLOSE(3)
    enddo
  enddo
  write(*,*) en(1,1,1:4,1)
  write(*,*) en(1,1,1:4,2)

  !
  ! Get energy conversion factors from nist
  !

  ua2cminv=2.194746313708d5
  ua2Ghz=6.579683920729d6
  u2uam=1d0/5.4857990946d-4
  a0fm=0.52917721092d5

  write(*,*) "Calling NIST to update energy conversion factors"

  call SYSTEM('rm -f conv.dat ; touch conv.dat ')

  call SYSTEM('&
    wget -q --output-document="bid" &
      "http://physics.nist.gov/cgi-bin/cuu/Convert?exp=0'//achar(38)//'num='//achar(38)//&
        'From=hr'//achar(38)//'To=minv'//achar(38)//'Action=Only+show+factor";&
    str1=$( grep -A1 "Concise form" bid | tail -1 );&
    str1=${str1#*<b>};&
    str1=${str1%</b>*};&
    echo ${str1} | &
      sed "s/'//achar(38)//'nbsp;\|([0-9]*)\|<\/sup>//g;&
           s/x /*/;&
           s/<sup>/^/" | bc -l >> conv.dat ;&
  ')
  call SYSTEM('&
    wget -q --output-document="bid" &
      "http://physics.nist.gov/cgi-bin/cuu/Convert?exp=0'//achar(38)//'num='//achar(38)//&
        'From=hr'//achar(38)//'To=hz'//achar(38)//'Action=Only+show+factor";&
    str=$( grep -A1 "Concise form" bid | tail -1 );&
    str=${str#*<b>};&
    str=${str%</b>*};&
    echo ${str} | &
      sed "s/'//achar(38)//'nbsp;\|([0-9]*)\|<\/sup>//g;&
           s/x /*/;&
           s/<sup>/^/" | bc -l >> conv.dat ;&
  ')

  call SYSTEM('&
    wget -q --output-document="bid" &
      "http://physics.nist.gov/cgi-bin/cuu/Value?meu";&
    str=$( grep -A1 "Concise form" bid | tail -1 );&
    str=${str#*<font size=\"4\">};&
    str=${str%</sup>*};&
    echo ${str} | &
      sed "s/'//achar(38)//'nbsp;\|([0-9]*)//g;&
           s/x/*/;&
           s/<sup>/^/" | bc -l >> conv.dat ;&
  ')

  call SYSTEM('&
    wget -q --output-document="bid" &
      "http://physics.nist.gov/cgi-bin/cuu/Value?bohrrada0";&
    str=$( grep -A1 "Concise form" bid | tail -1 );&
    str=${str#*<font size=\"4\">};&
    str=${str%</sup>*};&
    echo ${str} | &
      sed "s/'//achar(38)//'nbsp;\|([0-9]*)//g;&
           s/x/*/;&
           s/<sup>/^/" | bc -l >> conv.dat ;&
  ')

  !
  !

  open(17,file="conv.dat",STATUS="old",FORM="FORMATTED")
  read(17,*) ua2cminv
  read(17,*) ua2Ghz
  read(17,*) u2uam
  read(17,*) a0fm
  close(17)
  ua2cminv=ua2cminv*1d-2
  ua2Ghz=ua2Ghz*1d-9
  u2uam=1.d0/u2uam
  a0fm=a0fm*1d15
  write(*,*) "ua2cminv=",ua2cminv,"ua2Ghz=",ua2Ghz
  write(*,*) "u2uam=",u2uam, "a0fm=",a0fm
  !
  !
  !

  do i = 1,niso
    do j=i+1,niso
      write(*,*) "=============================================="
      write(*,'(2(a,f6.1))') "Isotopes: ", A(i,1), " -",A(j,1)
      write(*,*) "(Rrms=",Rrms(i,1),Rrms(j,1), ")"
      do bl1=1,nblock(1)
        do bl2=1,nblock(2)
          k1=1
          do while(en(k1,bl1,i,1)/=0)
            k2=1
            do  while(en(k2,bl2,j,1)/=0)
              w1=en(k2,bl2,i,2)-en(k1,bl1,i,1)
              w2=en(k2,bl2,j,2)-en(k1,bl1,j,1)
              write(*,*)
              write(*,*) "Transition energies (a.u.):", w1,w2
              write(*,*) "Isotope shift (a.u.):", w1-w2
              write(*,*)
              write(*,*) "Transition energies (cm-1):", ua2cminv*w1,ua2cminv*w2
              write(*,*) "Isotope shift (cm-1):", ua2cminv*(w1-w2)
              write(*,*)
              write(*,*) "Transition energies (GHz):", ua2Ghz*w1,ua2Ghz*w2
              write(*,*) "Isotope shift (GHz):", ua2Ghz*(w1-w2)
              write(*,*)
              write(*,*)
              w3=EMNAMU(i,1)*EMNAMU(j,1)/(EMNAMU(i,1)-EMNAMU(j,1))
              write(*,*) "a.u.: M= ",w3*u2uam*(Rrms(i,1)**2-Rrms(j,1)**2)/a0fm**2, &
                                     "F +",w3*u2uam*(w1-w2)
              write(*,*) "GHz amu: M= ",ua2Ghz*w3*(Rrms(i,1)**2-Rrms(j,1)**2)/a0fm**2, &
                                        "F +",ua2Ghz*w3*(w1-w2)
              write(*,*)
              k2=k2+1
            enddo
            k1=k1+1
          enddo
        enddo
      enddo
    enddo
  enddo

  if(niso<3)STOP

  write(*,*) "=============================================="
  write(*,*) "=============================================="

  do i = 1,niso
    do j=i+1,niso
      do k=j+1,niso
        write(*,*) "=============================================="
        write(*,'(2(a,f6.1))') "Isotopes: ", A(i,1), " -",A(j,1)
        write(*,'(2(a,f6.1))') "          ", A(i,1), " -",A(k,1)
        write(*,*) "(Rrms=",Rrms(i,1),Rrms(j,1), Rrms(k,1), ")"
        do bl1=1,nblock(1)
          do bl2=1,nblock(2)
            k1=1
            do while(en(k1,bl1,i,1)/=0)
              k2=1
              do  while(en(k2,bl2,j,1)/=0)
                w1=en(k2,bl2,i,2)-en(k1,bl1,i,1)
                w2=en(k2,bl2,j,2)-en(k1,bl1,j,1)
                w3=en(k2,bl2,k,2)-en(k1,bl1,k,1)
                write(*,*)
                write(*,*)
                w4=(EMNAMU(i,1)*EMNAMU(j,1))/(EMNAMU(i,1)-EMNAMU(j,1))
                w5=(EMNAMU(i,1)-EMNAMU(k,1))/(EMNAMU(i,1)*EMNAMU(k,1))*w4
                w6=Rrms(i,1)**2-Rrms(k,1)**2-(Rrms(i,1)**2-Rrms(j,1)**2)*w5
                F=(w1-w3-(w1-w2)*w5)/w6
                M=((w1-w2) - F*(Rrms(i,1)**2-Rrms(j,1)**2))*w4
                write(*,*) "a.u.: F=   ",F*a0fm**2
                write(*,*) "a.u.: M=   ",M*u2uam
                write(*,*) "a.u.: Msms=",(M-(w1-w2)*w4)*u2uam
                write(*,*)
                write(*,*) "MHz fm^-2: F= ",F*ua2Ghz*1d3
                write(*,*) "GHz uma: M=   ",M*ua2Ghz
                write(*,*) "GHz uma: Msms=",(M-(w1-w2)*w4)*ua2Ghz
                write(*,*)
                k2=k2+1
              enddo
              k1=k1+1
            enddo
          enddo
        enddo
      enddo
    enddo
  enddo



  STOP

end program ris_ana
