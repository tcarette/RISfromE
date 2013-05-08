module mconst
  IMPLICIT NONE
  !  Nuclear parameters
  integer niso
  real(kind(1d0)) Z
  real(kind(1d0)), allocatable :: A(:),APARM(:),CPARM(:),EMN(:),EMNAMU(:),RRMS(:)
  integer, allocatable :: NPARM(:)
  !*
  !*   Physical constants from http://physics.nist.gov (2010)
  !*
  REAL(KIND(1D0)), PARAMETER :: &
             AINFCM =       0.52917720859D-08, &
             ALFAI  =     137.035999084D+00,    &
             CCMPS  =       2.99792458D+10,    &
             EESU   =       4.803204272D-10,    &
             EMEG   =       9.10938215D-28,    &
             EMEAMU =       5.4857990943D-04,    &
             EMPAMU =       1.00727646677D+00, &
             HBARES =       1.054571628D-27,    &
             RINFEV =      13.60569193D+00,    &
             RINFK  =  109737.31568527D+00

  real(kind(1d0)) AUMAMU,FMTOAU

  !*

contains

  INCLUDE 'lodiso.f90'

end module mconst
