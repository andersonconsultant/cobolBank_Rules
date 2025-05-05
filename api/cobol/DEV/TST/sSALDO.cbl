      ******************************************************************
      *  Open Cobol ESQL (Ocesql) Test Program
      *
      *  Simple SELECT saldo() execution
      ******************************************************************
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 sSALDO.
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
       01  SALDO-RESULT           PIC 9(10)V99 VALUE 0.
       01  TRANSFER-RESULT        PIC X(500) VALUE SPACES.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-CMD                 PIC X(10) VALUE SPACE.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
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
           PERFORM WAIT-CMD.

       WAIT-CMD.
           DISPLAY "Digite 'saldo' ou 'extrato': ".
           ACCEPT WS-CMD.
           
           IF WS-CMD = "saldo"
              PERFORM GET-SALDO
              PERFORM WAIT-CMD
           ELSE IF WS-CMD = "extrato"
              PERFORM GET-EXTRATO
              PERFORM WAIT-CMD
           ELSE IF WS-CMD = "exit" OR WS-CMD = "quit"
              PERFORM FIM-PROGRAMA
           ELSE
              DISPLAY "Comando invalido"
              PERFORM WAIT-CMD
           END-IF.

       GET-SALDO.
      *    Execute SELECT saldo()
           EXEC SQL
               SELECT saldo() INTO :SALDO-RESULT
           END-EXEC.

           IF SQLCODE NOT = ZERO
              PERFORM ERROR-RTN
           ELSE
              DISPLAY "saldo\" SALDO-RESULT
           END-IF.

       GET-EXTRATO.
      *    Execute SELECT get_transfer()
           EXEC SQL
               SELECT get_transfer() INTO :TRANSFER-RESULT
           END-EXEC.

           IF SQLCODE NOT = ZERO
              PERFORM ERROR-RTN
           ELSE
              DISPLAY "transfer\" TRANSFER-RESULT
           END-IF.

       FIM-PROGRAMA.
      *    DISCONNECT from database
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
           DISPLAY "INSTANCIA FINALIZADA"
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