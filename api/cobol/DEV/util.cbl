        IDENTIFICATION DIVISION.
        PROGRAM-ID. util.

        DATA DIVISION.
        LINKAGE SECTION.
        01  LK-SQLCODE     PIC S9(9) COMP-5.
        01  LK-SQLSTATE    PIC X(5).
        01  LK-SQLERRMC    PIC X(70).

        PROCEDURE DIVISION USING LK-SQLCODE, LK-SQLSTATE, LK-SQLERRMC.

            DISPLAY "*** SQL ERROR ***"
            DISPLAY "SQLCODE: " LK-SQLCODE

            EVALUATE LK-SQLCODE
                WHEN +10
                    DISPLAY "Record not found"
                WHEN -1
                    DISPLAY "Connection failed"
                WHEN -20
                    DISPLAY "Internal error"
                WHEN -30
                    DISPLAY "PostgreSQL error"
                    DISPLAY "ERRCODE: "  LK-SQLSTATE
                    DISPLAY LK-SQLERRMC
                WHEN OTHER
                    DISPLAY "Undefined error"
                    DISPLAY "ERRCODE: "  LK-SQLSTATE
                    DISPLAY LK-SQLERRMC
            END-EVALUATE.

            EXIT PROGRAM.
