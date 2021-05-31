! Compile with:
! gfortran -g -fbacktrace -ffpe-trap=zero,overflow,underflow -fmax-errors=5 LMath.f90 CorrUtils.f90 SimpleCmdLine.f90 OneDayCorr30MinMeansRand.f90 -o OneDayCorr30MinMeansRand
!
! Run with:
! ./OneDayCorr30MinMeansRand -i /tmp/TEC/ -y 2018 -m 02 -d 15 -o /tmp/TEC/Correlation/
!

PROGRAM OneDayCorr30MinMeansRand
    USE :: CorrUtils
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : output_unit, real64

    IMPLICIT NONE

    INTEGER, DIMENSION( 10 ) :: lg
    INTEGER, DIMENSION( 450 ) :: nd
    INTEGER :: i, j, j2, j3, k, k2, fcount, jmax, jmean, unitEleven, unitTen
    CHARACTER( len=256 ) :: filein, fileout, maxtecfile, meantecfile
    REAL, DIMENSION( 450 ) :: lat, long, meantec, corr, xbarwiggle, sig1, sig2, xbar
    REAL, DIMENSION( MINS_PER_DAY, 450 ) :: tec, xwiggle
    LOGICAL fileex, unitop, problemReadingInput
    REAL, DIMENSION( HALF_HRS_PER_DAY, 450 ) :: mean30min

    TYPE( CorrUtilsType ) :: cutObj
    cutObj = CUT_constructor()

    fileout = cutObj%CUT_getFileOut( '-30MinMeans' )

    WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand filout: ', fileout
    OPEN( newunit=unitEleven, file=fileout )
    fcount = 0
    DO k=1,450
        meantec(k) = 0.
    END DO

    problemReadingInput = .false.
OUTER: DO j=60, MINS_PER_DAY, MINS_STEP
        j3 = j/MINS_STEP
        DO j2=( 1 +j-MINS_STEP ), j
            filein = cutObj%CUT_getFileIn( j2 )
!            WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand filein:', filein
            INQUIRE( file=filein, exist=fileex )
            IF( fileex ) THEN
!                WRITE( output_unit, * ) 'Input file exists'
                fcount = fcount +1
                INQUIRE( file=filein, number=unitTen, opened=unitop )
                IF( unitop ) CLOSE( unitTen )
                OPEN( newunit=unitTen, file=filein, status='old')
                DO k=1,450
                    READ( unitTen, *, err=101, END=102 ) lat(k), long(k),tec(j2,k)
                    mean30min( j3, k ) = mean30min( j3, k ) +tec( j2, k ) /MINS_STEP
                END DO
101             CONTINUE
                CLOSE( unitTen )
            ELSE
                problemReadingInput = .true.
                EXIT OUTER
!                GOTO 103
!                EXIT - from inner loop
            END IF
103     END DO
        meantec( k ) = meantec( k ) +mean30min( j3, k )/HALF_HRS_PER_DAY
    END DO OUTER
102 CONTINUE

    ! calculate local correlation index
!    WRITE(output_unit,*) 'OneDayCorr30MinMeansRand flag1'
    DO k=1, 450
        nd( k ) = 0.
        xbar( k ) = 0.
        ! calculate correlation matrix for this day
        DO j3=1, HALF_HRS_PER_DAY
            xbar( k ) = xbar( k ) +mean30min( j3, k ) /HALF_HRS_PER_DAY
            xwiggle( j3, k ) = 0.
        END DO
!        WRITE( output_unit, * ) 'flag1a', k, j3
        DO k2=1, 450
            IF(( k2 /= k).AND.(cutObj%CUT_distkm( lat( k ), long( k ), lat( k2 ), long( k2 )) < 250 )) THEN
                nd( k ) = nd( k ) +1
                DO j3=1, HALF_HRS_PER_DAY
                    xwiggle( j3, k ) = xwiggle( j3, k ) +mean30min( j3, k2 )
                END DO
!                WRITE( output_unit, * ) 'flag1b', k, j, k2
            END IF
        END DO
    END DO
    WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand flag1c', k, j, k2
    DO k=1, 450
        xbarwiggle( k ) = 0.0
        DO j3=1, 48
            xwiggle( j3, k ) = xwiggle( j3, k ) /nd( k )
            xbarwiggle( k ) = xbarwiggle( k ) +xwiggle( j3, k )/HALF_HRS_PER_DAY
        END DO
    END DO

!    WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand flag2'
    DO k=1, 450
        sig1( k ) = 0.
        sig2( k ) = 0.
        DO j3=1, HALF_HRS_PER_DAY
            sig1( k ) = sig1( k ) +( mean30min( j3, k ) -xbar( k ))**2 /( HALF_HRS_PER_DAY -1 )
            sig2( k ) = sig2( k ) +( xwiggle( j3, k ) -xbarwiggle( k ))**2/( HALF_HRS_PER_DAY -1)
        END DO
        sig1( k ) = sqrt( sig1( k ))
        sig2( k ) = sqrt( sig2( k ))
    END DO

!    WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand flag3'
    DO k=1, 450
        corr( k ) = 0.0
        DO j3=1, HALF_HRS_PER_DAY
            IF(( sig1( k ) *sig2( k )) == 0.) THEN ! TODO: What?!
                ! WRITE(output_unit,*) cdate,'sigzero',lat(k),long(k),sig1(k),sig2(k)
                corr( k ) = 1.0
            ELSE
                corr( k ) = corr( k ) +( mean30min( j3, k ) -xbar( k )) *( xwiggle( j3, k ) -xbarwiggle( k )) &
                            /((HALF_HRS_PER_DAY -1) *sig1( k ) *sig2( k ))
            END IF
!            IF( corr( k ) < 0.99 ) WRITE( output_unit, * ) cutObj%date, lat( k ), long( k ), corr( k )
        END DO
    END DO
!    WRITE( output_unit, * ) 'flag4'
    WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand fcount', fcount

    WRITE( unitEleven, "('lat, lon, corr_k, nd_k')")
    DO k=1, 450
        WRITE( unitEleven, "(f0.6,',',f0.6,',',f0.8,',',I0)" ) lat( k ), long( k ), corr( k ), nd( k )
    END DO
!    WRITE( output_unit, * ) 'OneDayCorr30MinMeansRand flag5'
    CLOSE( unitTen )
    IF( problemReadingInput ) THEN
        CLOSE( unitEleven, status='delete' )
    ELSE
        CLOSE( unitEleven )
    END IF
END
