!--------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
! Find the command line arguments - very simple logic, not very smart, but sufficient for the computer interface

MODULE SimpleCmdLine
    IMPLICIT NONE
CONTAINS
    !----!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
    FUNCTION SCL_isArgumentPresent( searchArgS, searchArgL )
        IMPLICIT NONE
        LOGICAL :: SCL_isArgumentPresent
        CHARACTER( LEN=* ), INTENT( IN ) :: searchArgS
        CHARACTER( LEN=* ), INTENT( IN ), OPTIONAL :: searchArgL
        INTEGER :: i
        CHARACTER( LEN=132 ) :: cmdArg
        SCL_isArgumentPresent = .FALSE.
        DO i = 1, command_argument_count()
            CALL get_command_argument( i, cmdArg )
            IF( searchArgS == cmdArg .OR. present( searchArgL ) .AND. searchArgL == cmdArg ) THEN
                SCL_isArgumentPresent = .TRUE.
                EXIT
            END IF
        END DO
    END FUNCTION SCL_isArgumentPresent

    !----!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
    SUBROUTINE SCL_getArgument( suppliedArg, searchArgS, searchArgL )
        IMPLICIT NONE
        CHARACTER( LEN=* ), INTENT( IN ) :: searchArgS
        CHARACTER( LEN=* ), INTENT( IN ), OPTIONAL :: searchArgL
        CHARACTER( LEN=* ), INTENT( OUT ) :: suppliedArg
        INTEGER :: i
        INTEGER :: argNumber = 0
        CHARACTER( LEN=132 ) :: cmdArg
        DO i = 1, command_argument_count()
            CALL get_command_argument( i, cmdArg )
            IF( searchArgS == cmdArg .OR. present( searchArgL ) .AND. searchArgL == cmdArg ) THEN
                argNumber = i
                EXIT
            END IF
        END DO
        IF( 0 /= argNumber ) THEN
            CALL get_command_argument( 1+argNumber, suppliedArg )
        END IF
    END SUBROUTINE SCL_getArgument

END MODULE SimpleCmdLine
!--------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!---------!-!
