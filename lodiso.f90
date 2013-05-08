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
      IMPLICIT REAL*8          (A-H,O-Z)
      character(len=*), intent(in) :: isof(niso)
      integer i
      real(kind(1d0)), external :: ESTRMS
!
!-----------------------------------------------------------------------


     allocate(A(niso))
     allocate(APARM(niso))
     allocate(CPARM(niso))
     allocate(RRMS(niso))
     allocate(EMNAMU(niso))
     allocate(EMN   (niso))
     allocate(NPARM(niso))

!
!   Read and echo pertinent information from  .iso  file
!

      AUMAMU = EMEAMU
!*
!*   Calculate conversion factor for Fermis to Bohr radii
!*
      FMTOAU = 1.0D-13/AINFCM

!*

      do i=1,niso

        open(22,file=isof(i),STATUS='old',ACTION='read',FORM='formatted')

!   Atomic number
!
        READ (22,*) Z
!
!   Nuclear geometry
!
        READ (22,*)
        READ (22,*) A(i)
        READ (22,*)
        READ (22,*) APARM(i)
        READ (22,*)
        READ (22,*) CPARM(i)
!
        IF (A(i) .NE. 0.D0) THEN
           NPARM(i) = 2
!         PARM(1) = CPARM*FMTOAU
!         PARM(2) = APARM*FMTOAU
        ELSE
           NPARM(i) = 0
        ENDIF
!
!   Nuclear mass
!
        READ (22,*)
        READ (22,*) EMNAMU(i)
!
        IF (EMNAMU(i) .NE. 0.D0) THEN
           EMN(i) = EMNAMU(i)/AUMAMU
        ELSE
           EMN(i) = 0.D0
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

        RRMS(i)=ESTRMS(APARM(i),CPARM(i))

      enddo

      RETURN
      END SUBROUTINE LODISO
