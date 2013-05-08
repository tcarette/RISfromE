!***********************************************************************
!                                                                      *
      SUBROUTINE LODISO(isof)
!                                                                      *
!   Loads the data from the  .iso  file.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 29 Sep 1992   *
!                                                                      *
!***********************************************************************
!
      character(len=*), intent(in) :: isof(niso,2)
      integer i,n
      real(kind(1d0)), external :: ESTRMS
!
!-----------------------------------------------------------------------


     allocate(A(niso,2))
     allocate(APARM(niso,2))
     allocate(CPARM(niso,2))
     allocate(RRMS(niso,2))
     allocate(EMNAMU(niso,2))
     allocate(EMN   (niso,2))
     allocate(NPARM(niso,2))

!
!   Read and echo pertinent information from  .iso  file
!

      AUMAMU = EMEAMU
!*
!*   Calculate conversion factor for Fermis to Bohr radii
!*
      FMTOAU = 1.0D-13/AINFCM

!*

    do n =1,2
      do i=1,niso

        write(*,*) "Parse isodata file ",isof(i,n)
        open(22,file=isof(i,n),STATUS='old',ACTION='read',FORM='formatted')

!   Atomic number
!
        READ (22,*)
        READ (22,*) Z
!
!   Nuclear geometry
!
        READ (22,*)
        READ (22,*) A(i,n)
        if(A(i-1,n)<=A(i,n))then
          write(*,*) "You will obey!"
        endif
        READ (22,*)
        READ (22,*) APARM(i,n)
        READ (22,*)
        READ (22,*) CPARM(i,n)
!
        IF (A(i,n) .NE. 0.D0) THEN
           NPARM(i,n) = 2
!         PARM(1) = CPARM*FMTOAU
!         PARM(2) = APARM*FMTOAU
        ELSE
           NPARM(i,n) = 0
        ENDIF
!
!   Nuclear mass
!
        READ (22,*)
        READ (22,*) EMNAMU(i,n)
!
        IF (EMNAMU(i,n) .NE. 0.D0) THEN
           EMN(i,n) = EMNAMU(i,n)/AUMAMU
        ELSE
           EMN(i,n) = 0.D0
        ENDIF
!
!   Nuclear spin and moments
!
!      READ (22,*)
!      READ (22,*) SQN
!      READ (22,*)
!      READ (22,*) DMOMNM
!      READ (22,*)
!      READ (22,*) QMOMB

        close(22)

        RRMS(i,n)=ESTRMS(APARM(i,n),CPARM(i,n))

      enddo
    enddo

      RETURN
      END SUBROUTINE LODISO
