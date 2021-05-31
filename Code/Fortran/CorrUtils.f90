! Combile both with:
! gfortran -g -fbacktrace -ffpe-trap=zero,overflow,underflow -fmax-errors=5 LMath.f90 CorrUtils.f90 SimpleCmdLine.f90 OneDayCorr30MinMeansRand.f90 -o OneDayCorr30MinMeansRand && gfortran -g -fbacktrace -ffpe-trap=zero,overflow,underflow -fmax-errors=5 LMath.f90 CorrUtils.f90 SimpleCmdLine.f90 OneDayCorrRand.f90 -o OneDayCorrRand

MODULE CorrUtils
    IMPLICIT NONE
    INTEGER, PARAMETER :: STR_COUNT = 10
    INTEGER, PARAMETER :: HALF_HRS_PER_DAY = 48
    INTEGER, PARAMETER :: MINS_STEP = 60
    INTEGER, PARAMETER :: MINS_PER_DAY = 2880

    TYPE :: CorrUtilsType
        INTEGER, DIMENSION( 3 ) :: date
        CHARACTER( len=256 ), DIMENSION( STR_COUNT ), PRIVATE :: elements, elemnts2
    CONTAINS
        PROCEDURE :: CUT_getFileOut
        PROCEDURE :: CUT_getFileIn
        PROCEDURE :: CUT_distkm
        PROCEDURE, PRIVATE :: CUT_diskm2
        PROCEDURE, PRIVATE :: CUT_loadElements
        PROCEDURE, PRIVATE :: CUT_joinStrings
        PROCEDURE, PRIVATE :: CAT_datestrng
        PROCEDURE, PRIVATE :: CAT_numberstrng
    END TYPE

    INTERFACE CorrUtilsType
        PROCEDURE CUT_constructor
    END INTERFACE

CONTAINS

    FUNCTION CUT_distkm(self, lat1,long1,lat2,long2) RESULT( answer )
        USE :: LMath, ONLY : LMathPI, LMathDegToRad
        IMPLICIT NONE
        CLASS( CorrUtilsType ), INTENT( in ) :: self
        REAL :: answer
        REAL, INTENT( in ) ::  lat1, long1, lat2, long2

        REAL :: theta1, theta2, phi1, phi2, pi, cosdis, erad

        pi = LMathPI
        erad = 111. /LMathDegToRad ! *180/pi
        theta1 = lat1 *LMathDegToRad !pi/180.
        theta2 = lat2 *LMathDegToRad !pi/180.
        phi1 = long1 *LMathDegToRad !pi/180.
        phi2 = long2 *LMathDegToRad !pi/180.
        cosdis = sin( theta1 ) *sin( theta2 ) +cos( theta1 ) *cos( theta2 ) *cos( phi1-phi2 )
        IF ((cosdis > 1.0).or.(cosdis < -1.0))THEN
            answer = self%CUT_diskm2( lat1, long1, lat2, long2 )
            RETURN
        END IF
        answer = erad *acos( cosdis )
    END

    FUNCTION CUT_diskm2( self, lat1, long1, lat2, long2 ) RESULT( answer )
        USE :: LMath, ONLY : LMathPI
        IMPLICIT NONE
        CLASS( CorrUtilsType ), INTENT( in ) :: self
        REAL :: answer
        REAL, INTENT( in ) :: lat1, long1, lat2, long2
        REAL :: degkm, dx, dy
        degkm = 111.0
        dx = ( lat1-lat2 ) *degkm
        dy = ( long1-long2 ) *degkm *cos( LMathPI *( lat1 +lat2 ) /360.)
        answer = sqrt( dx**2 +dy**2)
    END

    FUNCTION CUT_getFileIn( self, j ) RESULT( answer )
        IMPLICIT NONE
        CLASS( CorrUtilsType ) :: self
        CHARACTER( len=256 ) :: answer
        INTEGER, INTENT( in ) :: j

        self%elements( 8 ) = self%CAT_numberstrng( j )
        answer = self%CUT_joinStrings( self%elements, 8 )
    END FUNCTION

    FUNCTION CUT_getFileOut( self, refinement ) RESULT( answer )
        IMPLICIT NONE
        CLASS( CorrUtilsType ) :: self
        CHARACTER( len=256 ) :: answer
        CHARACTER( len=* ), OPTIONAL :: refinement
        IF( present( refinement )) THEN
            self%elemnts2( 5 ) = refinement
        ELSE
            self%elemnts2( 5 ) = ''
        END IF
        answer = self%CUT_joinStrings( self%elemnts2, 7 )
    END FUNCTION

    SUBROUTINE CUT_loadElements( self )
        USE :: SimpleCmdLine
        IMPLICIT NONE
        CLASS( CorrUtilsType ), INTENT( inout ) :: self

        CHARACTER( len=STR_COUNT ) :: cdate, cyear, cmonth, cday
        INTEGER :: i

        IF( SCL_isArgumentPresent( '-i' )) THEN
            CALL SCL_getArgument( self%elements( 1 ), '-i' )
            CALL SCL_getArgument( cyear, '-y' )
            CALL SCL_getArgument( cmonth, '-m' )
            CALL SCL_getArgument( cday, '-d' )
            CALL SCL_getArgument( self%elemnts2(1), '-o' )
            self%elements( 6 )='/TEC_Data'
            self%elements( 7 )='/igs.'
            self%elemnts2( 6 )='.'
            self%elemnts2( 7 )='csv'
        ELSE
            CALL getarg( 1, cyear )
            CALL getarg( 2, cmonth )
            CALL getarg( 3, cday )
            self%elements( 1 )='/seis/prj' ! '/scratch/TEC/'
            self%elements( 2 )='/tec' ! 'GPSTK/DATA/'
            self%elements( 3 )='/DATA' ! eqname
            self%elements( 4 )='/'
            self%elements( 6 )='/TEC_Data'
            self%elements( 7 )='/igs.'
            self%elemnts2( 1 )= '/seis/prj/tec/DATA' ! '/scratch/TEC/GPSTK/DATA/' ! '/seis/prj/csep/'
            self%elemnts2( 2 )= '/' ! eqname ! '/seis/prj/csep/'
            self%elemnts2( 3 )='Correlation/'
            self%elemnts2( 5 )='.'
            self%elemnts2( 6 )='corr'
        END IF
        READ( cyear, * ) self%date( 1 )
        READ( cmonth, * ) self%date( 2 )
        READ( cday, * ) self%date( 3 )
        cdate = self%CAT_datestrng()
        self%elements( 5 ) = cdate
        self%elemnts2( 4 ) = cdate
!        PRINT *,'CUT_loadElements:'
!        DO i=1, STR_COUNT
!            PRINT *,' i:',i,' elements(i):',self%elements( i )
!            PRINT *,' i:',i,' elemnts2(i):',self%elemnts2( i )
!        END DO
    END SUBROUTINE

    FUNCTION CUT_constructor( ) RESULT( self )
        TYPE( CorrUtilsType ) :: self
        INTEGER :: i
        self%date = [2011, 4, 2]
        DO i=1, STR_COUNT
            self%elements( i ) = ''
            self%elemnts2( i ) = ''
        END DO
        CALL self%CUT_loadElements()
    END FUNCTION

    FUNCTION CUT_joinStrings( self, elements, n ) RESULT( answer )
        IMPLICIT NONE
        CLASS( CorrUtilsType ), INTENT( in ) :: self
        CHARACTER(len=*), DIMENSION(STR_COUNT), INTENT( in ) :: elements
        INTEGER, INTENT( in ) :: n
        CHARACTER(len=256) :: answer
        INTEGER i
        answer = ''
        DO i=1, n
            answer = trim( answer ) // trim( elements( i ))
        END DO
!        PRINT *, 'CUT_joinStrings using n=', n, ' items:'
!        DO i=1, n
!            PRINT *, 'i=', i, ' elements(i)=', elements(i)
!        END DO
!        PRINT *, 'CUT_joinStrings result:', answer
    END FUNCTION

    FUNCTION CAT_datestrng( self ) RESULT( answer )
        IMPLICIT NONE
        CLASS( CorrUtilsType ), INTENT( in ) :: self
        CHARACTER*10 :: answer

        CHARACTER*4 cyear
        CHARACTER*2 cmonth, cday
        CHARACTER*1 dash
        WRITE( cyear, 21 ) self%date( 1 )
21      FORMAT( i4 )
22      FORMAT( i2 )
23      FORMAT( '0', i1 )
        DATA dash/'-'/
        IF( self%date( 2 ) > 9 )THEN
            WRITE( cmonth, 22 ) self%date( 2 )
        ELSE
            WRITE( cmonth, 23 ) self%date( 2 )
        END IF
        IF( self%date( 3 ) > 9 )THEN
            WRITE( cday, 22 ) self%date( 3 )
        ELSE
            WRITE( cday, 23 ) self%date( 3 )
        END IF
        WRITE( answer, 24 )cyear, dash, cmonth, dash, cday
24      FORMAT( a4, a1, a2, a1, a2 )
    END

    FUNCTION CAT_numberstrng( self, n ) RESULT( answer )
        CLASS( CorrUtilsType ), INTENT( in ) :: self
        CHARACTER( len=4 ) :: answer
        INTEGER, INTENT( in ) :: n

        IF ( n > 999 )THEN
            WRITE( answer, '( i4 )' ) n
        ELSE IF( n > 99 )THEN
            WRITE( answer, "( '0', i3 )" ) n
        ELSE IF( n > 9 )THEN
            WRITE( answer, "( '00', i2 )" ) n
        ELSE
            WRITE( answer, "( '000', i1 )" ) n
        END IF
    END
END MODULE
