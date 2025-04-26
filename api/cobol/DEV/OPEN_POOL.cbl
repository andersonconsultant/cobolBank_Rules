      ******************************************************************
      *  Open Cobol ESQL (Ocesql) Test Program
      *
      *  Simple connection test
      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 FETCHTBL.
       AUTHOR.                     TSH.
       DATE-WRITTEN.               2024-03-20.

      ******************************************************************
       ENVIRONMENT                 DIVISION.
      ******************************************************************
       CONFIGURATION              SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  CURRENT-USER            PIC  X(30) VALUE SPACE.
       01  SALDO-RESULT             PIC 9(10)V99 VALUE 0.
       01  WS-STOP                 PIC  X VALUE SPACE.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
      *DISPLAY "*** TEST CONNECT STARTED ***".
           
      *    CONNECT
           MOVE "cobolbd"   TO   DBNAME.
           MOVE "server"    TO   USERNAME.
           MOVE "pwdbd123" TO   PASSWD.
           
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           
           IF SQLCODE NOT = ZERO 
              PERFORM ERROR-RTN
              STOP RUN
           END-IF.
           
      * DISPLAY "*** Connection successful! ***".

      *    SELECT saldo()
           EXEC SQL
               SELECT saldo() INTO :SALDO-RESULT
           END-EXEC.

           IF SQLCODE NOT = ZERO
              PERFORM ERROR-RTN
           ELSE
              DISPLAY "saldo\" SALDO-RESULT
           END-IF.
           
           DISPLAY "Pressione ENTER para encerrar..."
           ACCEPT WS-STOP.
           
           STOP RUN.

      ******************************************************************
       ERROR-RTN.
      ******************************************************************
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE " " NO ADVANCING.
           EVALUATE SQLCODE
              WHEN  +10
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE: "  SQLSTATE
                 DISPLAY SQLERRMC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE: "  SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
      ******************************************************************

