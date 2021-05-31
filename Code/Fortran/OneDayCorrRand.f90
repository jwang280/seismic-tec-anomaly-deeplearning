! Compile with:
! gfortran -g -fbacktrace -ffpe-trap=zero,overflow,underflow -fmax-errors=5 LMath.f90 CorrUtils.f90 SimpleCmdLine.f90 OneDayCorrRand.f90 -o OneDayCorrRand
!
! Run with:
! ./OneDayCorrRand -i /tmp/TEC/ -y 2018 -m 02 -d 15 -o /tmp/TEC/Correlation/
!
PROGRAM OneDayCorrRand
    USE :: CorrUtils
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : output_unit, real64

    IMPLICIT NONE

    INTEGER, DIMENSION( 10 ) :: lg
    INTEGER, DIMENSION( 450 ) :: nd ! factors are 2, 3, 3, 5, 5 - do I assume 2*3*5 and 3*5 for latitude and longitude?
    INTEGER :: i, j, k, k2, fcount, jmax, jmean, unitEleven, unitTen
    CHARACTER( len=256 ) :: filein, fileout, maxtecfile, meantecfile
    REAL, DIMENSION( 450 ) :: lat, long, meantec, corr, xbarwiggle, sig1, sig2, xbar ! ( 450 )
    REAL, DIMENSION( MINS_PER_DAY, 450 ) :: tec, xwiggle ! ( MINS_PER_DAY, 450 )
    LOGICAL :: fileex, unitop ! , problemReadingInput
    INTEGER :: fileCount, errNumber

    TYPE( CorrUtilsType ) :: cutObj
    cutObj = CUT_constructor()

    fileout = cutObj%CUT_getFileOut()

!    WRITE( output_unit, * ) 'OneDayCorrRand fileout: ', fileout
    OPEN( newunit=unitEleven, file=fileout )
    DO k=1, 450
        meantec( k ) = 0.
    END DO

    fileCount = 0
    DO j=1, MINS_PER_DAY
        filein = cutObj%CUT_getFileIn( j )
        INQUIRE( file=filein, exist=fileex )
        IF( fileex ) THEN
            fileCount = 1 +fileCount
        ELSE
            PRINT *, 'Missing input file:', filein
        END IF
    END DO

    fcount = 0
    DO j=1, MINS_PER_DAY
        filein = cutObj%CUT_getFileIn( j )
        INQUIRE( file=filein, exist=fileex )
        IF ( fileex ) THEN
            fcount = fcount +1
            OPEN( newunit=unitTen, file=filein, status='old' )
            DO k=1, 450
                READ( unitTen, *, iostat=errNumber) lat( k ), long( k ), tec( fcount, k ) ! was err=101, end=102
                ! calculate correlation matrix for this day
                IF( 0 == errNumber ) THEN
                    meantec( k ) = meantec( k ) +tec( fcount, k ) /fileCount
                ELSE
                    PRINT *, 'ERROR:', errNumber, ' reading from input file.'
                    EXIT
                END IF
            END DO
            CLOSE( unitTen )
        END IF
    END DO

    DO k=1, 450
        nd( k ) = 0.
        xbar( k ) = 0.
        DO j=1, fileCount
            xbar( k ) = xbar( k ) +tec( j, k ) /fileCount
            xwiggle( j, k ) = 0.
        END DO
        DO k2=1, 450
            IF(( k2 /= k ) .and. ( cutObj%CUT_distkm( lat( k ), long( k ), lat( k2 ), long( k2 )) < 250 )) THEN
                nd( k ) = nd( k ) +1
                DO j=1, fileCount
                    xwiggle( j, k ) = xwiggle( j, k ) +tec( j, k2 )
                END DO
            END IF
        END DO
    END DO

!    WRITE( output_unit, * ) 'OneDayCorrRand flag1c', k, j, k2
    DO k=1, 450
        xbarwiggle( k ) = 0.0
        DO j=1, fileCount
            xwiggle( j, k ) = xwiggle( j, k ) /nd( k )
            xbarwiggle( k ) = xbarwiggle( k ) +xwiggle( j, k ) /fileCount
        END DO
    END DO

    DO k=1, 450
        sig1( k ) = 0.
        sig2( k ) = 0.
        DO j=1, fileCount
            sig1( k ) = sig1( k ) +( tec( j, k ) -xbar( k ))**2 /( fileCount -1)
            sig2( k ) = sig2( k ) +( xwiggle( j, k ) -xbarwiggle( k ))**2 /( fileCount -1)
        END DO
        sig1( k ) = sqrt( sig1( k ))
        sig2( k ) = sqrt( sig2( k ))
    END DO

    DO k=1, 450
        corr( k ) = 0.0
        DO j=1, fileCount
            corr( k ) = corr( k ) &
                    +( tec( j, k ) -xbar( k )) *( xwiggle( j, k ) -xbarwiggle( k )) /(( fileCount-1) *sig1( k ) *sig2( k ))
        END DO
    END DO

!    WRITE( output_unit, * ) 'OneDayCorrRand fcount', fcount
    WRITE( unitEleven, "('lat, lon, corr_k, nd_k')")
    DO k=1, 450
        WRITE( unitEleven, "(f0.6,',',f0.6,',',f0.8,',',I0)" ) lat( k ), long( k ), corr( k ), nd( k )
    END DO

    CLOSE( unitTen )
    CLOSE( unitEleven )
END
