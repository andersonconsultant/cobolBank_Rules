       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOG-INSERT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DBNAME                  PIC X(20).
       01 USERNAME               PIC X(20).
       01 PASSWD                 PIC X(20).
       
       01 WS-LOG-DATA.
          05 WS-SESSION-ID       PIC X(10).
          05 WS-COMPONENT        PIC X(10).
          05 WS-ACTION          PIC X(20).
          05 WS-STATUS          PIC X(10).
          05 WS-RESPONSE-TIME   PIC 9(6).
          05 WS-MESSAGE         PIC X(100).
          05 WS-ENDPOINT        PIC X(50).
          05 WS-METHOD          PIC X(6).
          05 WS-VALUE           PIC 9(10)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       01 WS-ERROR-MESSAGE      PIC X(100).
       01 WS-SQLCODE            PIC S9(9) COMP.
       
       PROCEDURE DIVISION.
       
       MAIN-PARAGRAPH.
           PERFORM CONNECT-TO-DB.
           IF SQLCODE = ZERO
              PERFORM INSERT-LOG
              PERFORM DISCONNECT-DB
           END-IF.
           STOP RUN.
           
       CONNECT-TO-DB.
           MOVE "dbname" TO DBNAME.
           MOVE "user"   TO USERNAME.
           MOVE "pass"   TO PASSWD.
           
           EXEC SQL
               CONNECT TO :DBNAME USER :USERNAME USING :PASSWD
           END-EXEC.
           
           IF SQLCODE NOT = ZERO
              MOVE "Erro ao conectar ao banco" TO WS-ERROR-MESSAGE
              PERFORM ERROR-ROUTINE
           END-IF.
           
       INSERT-LOG.
           EXEC SQL
               INSERT INTO log_cobol (
                   session_id, component, action, status,
                   response_time, message, endpoint, method,
                   value_processed
               ) VALUES (
                   :WS-SESSION-ID, :WS-COMPONENT, :WS-ACTION, :WS-STATUS,
                   :WS-RESPONSE-TIME, :WS-MESSAGE, :WS-ENDPOINT, :WS-METHOD,
                   :WS-VALUE
               )
           END-EXEC.
           
           IF SQLCODE NOT = ZERO
              MOVE "Erro ao inserir log" TO WS-ERROR-MESSAGE
              PERFORM ERROR-ROUTINE
           END-IF.
           
       DISCONNECT-DB.
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
           
           IF SQLCODE NOT = ZERO
              MOVE "Erro ao desconectar" TO WS-ERROR-MESSAGE
              PERFORM ERROR-ROUTINE
           END-IF.
           
       ERROR-ROUTINE.
           DISPLAY "ERRO: " WS-ERROR-MESSAGE
           DISPLAY "SQLCODE: " SQLCODE
           STOP RUN. 