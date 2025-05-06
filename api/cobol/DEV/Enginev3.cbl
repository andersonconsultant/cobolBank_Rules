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
         01  WS-CONN-PID            PIC 9(5) VALUE 0.
         01  SQLCODE-PASS       PIC S9(9) COMP-5.
         01  SQLSTATE-PASS      PIC X(5).
         01  SQLERRMC-PASS      PIC X(70).

         EXEC SQL END DECLARE SECTION END-EXEC.

         01  WS-CMD                 PIC X(10) VALUE SPACE.
         01  WS-DB-STARTED          PIC X(1)  VALUE "N".

         EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
         PROCEDURE                   DIVISION.
      ******************************************************************
         MAIN-RTN.
            PERFORM WAIT-CMD.

         MENU-HELP.
            DISPLAY SPACE
            DISPLAY "=== COMANDOS DISPONIVEIS ==="
            DISPLAY "START   : Inicia conexao com o banco"
            DISPLAY "SALDO   : Consulta saldo atual"
            DISPLAY "EXTRATO : Lista ultimas transacoes"
            DISPLAY "HELP    : Mostra este menu de ajuda"
            DISPLAY "EXIT    : Finaliza o programa"
            DISPLAY "========================="
            DISPLAY SPACE
            PERFORM WAIT-CMD.

         START-SQL.
      *    CONNECT
            MOVE "cobolbd"   TO   DBNAME
            ACCEPT USERNAME FROM ENVIRONMENT "DB_USER".
            ACCEPT PASSWD FROM ENVIRONMENT "DB_PASSWORD".

            
            EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
            END-EXEC
            
            IF SQLCODE NOT = ZERO
            MOVE SQLCODE TO SQLCODE-PASS
            MOVE SQLSTATE TO SQLSTATE-PASS
            MOVE SQLERRMC TO SQLERRMC-PASS
            CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
            ELSE
               MOVE "S" TO WS-DB-STARTED
                  EXEC SQL
                  SELECT pg_backend_pid() INTO :WS-CONN-PID
                  END-EXEC

                  IF SQLCODE NOT = ZERO
                  MOVE SQLCODE TO SQLCODE-PASS
                  MOVE SQLSTATE TO SQLSTATE-PASS
                  MOVE SQLERRMC TO SQLERRMC-PASS
                  CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
                  ELSE
                     DISPLAY "1\STATUS\ID:" WS-CONN-PID
                     DISPLAY "#Conexao com o banco iniciada com sucesso"
                  END-IF
      *     INSERT ID           
                  EXEC SQL
                  INSERT INTO sessions_pool(id)
                  VALUES (pg_backend_pid())
                  END-EXEC
                  EXEC SQL
                  COMMIT
                  END-EXEC

                  IF SQLCODE NOT = ZERO
                  MOVE SQLCODE TO SQLCODE-PASS
                  MOVE SQLSTATE TO SQLSTATE-PASS
                  MOVE SQLERRMC TO SQLERRMC-PASS
                  CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
                  ELSE
                     DISPLAY "1\INSERT #ID IS ON BANK!" WS-CONN-PID
                  END-IF

            END-IF
            PERFORM WAIT-CMD.

         WAIT-CMD.
            DISPLAY "#Digite o comando: - Digite HELP para ajuda"
            ACCEPT WS-CMD
            
            IF WS-CMD = "START"
               IF WS-DB-STARTED = "S"
                  DISPLAY "1\STATUS"
                  DISPLAY "#Banco ja esta conectado"
                  PERFORM WAIT-CMD
               ELSE
                  PERFORM START-SQL
               END-IF
            ELSE IF WS-CMD = "SALDO"
               IF WS-DB-STARTED = "N"
                  DISPLAY "0\STATUS"
                  DISPLAY "#Banco nao conectado - Digite START primeiro"
                  PERFORM WAIT-CMD
               ELSE
                  PERFORM GET-SALDO
                  PERFORM WAIT-CMD
               END-IF
            ELSE IF WS-CMD = "EXTRATO"
               IF WS-DB-STARTED = "N"
                  DISPLAY "0\STATUS"
                  DISPLAY "Banco nao conectado"
                  DISPLAY "Digite START primeiro"
                  PERFORM WAIT-CMD
               ELSE
                  PERFORM GET-EXTRATO
                  PERFORM WAIT-CMD

      * TEST INIT
            ELSE IF WS-CMD = "UPDATE"
               IF WS-DB-STARTED = "N"
               DISPLAY "0\STATUS"
               DISPLAY "#Banco nao conectado"
               PERFORM UPD-SQL
               PERFORM WAIT-CMD            

               ELSE 
                  PERFORM UPD-SQL
                  PERFORM WAIT-CMD
      * TEST END
            ELSE IF WS-CMD = "HELP"
               PERFORM MENU-HELP
            ELSE IF WS-CMD = "EXIT" OR WS-CMD = "QUIT"
               PERFORM FIM-PROGRAMA
            ELSE
               DISPLAY "0\Comando invalido"
               DISPLAY "#Digite HELP para ajuda"
               PERFORM WAIT-CMD
            END-IF.

         UPD-SQL.
      *    Execute UPDATE updated_at()
            EXEC SQL
               UPDATE sessions_pool
               SET updated_at = now()
               WHERE id = pg_backend_pid(); 
            END-EXEC
               EXEC SQL
            COMMIT
            END-EXEC

            IF SQLCODE NOT = ZERO
            MOVE SQLCODE TO SQLCODE-PASS
            MOVE SQLSTATE TO SQLSTATE-PASS
            MOVE SQLERRMC TO SQLERRMC-PASS
            CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
            ELSE
               DISPLAY "1\INSERT #ID IS ON BANK!" WS-CONN-PID
            END-IF

            IF SQLCODE NOT = ZERO
            MOVE SQLCODE TO SQLCODE-PASS
            MOVE SQLSTATE TO SQLSTATE-PASS
            MOVE SQLERRMC TO SQLERRMC-PASS
            CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
            ELSE
               DISPLAY "1\UPDATED"
            END-IF.
      
         GET-SALDO.
      *    Execute SELECT saldo()
            EXEC SQL
               SELECT saldo() INTO :SALDO-RESULT
            END-EXEC

            IF SQLCODE NOT = ZERO
            MOVE SQLCODE TO SQLCODE-PASS
            MOVE SQLSTATE TO SQLSTATE-PASS
            MOVE SQLERRMC TO SQLERRMC-PASS
            CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
            ELSE
               DISPLAY "saldo\" SALDO-RESULT
            END-IF.

         GET-EXTRATO.
      *    Execute SELECT get_transfer()
            EXEC SQL
               SELECT get_transfer() INTO :TRANSFER-RESULT
            END-EXEC

            IF SQLCODE NOT = ZERO
            MOVE SQLCODE TO SQLCODE-PASS
            MOVE SQLSTATE TO SQLSTATE-PASS
            MOVE SQLERRMC TO SQLERRMC-PASS
            CALL 'util' USING SQLCODE-PASS, SQLSTATE-PASS, SQLERRMC-PASS
            ELSE
               DISPLAY "transfer\" TRANSFER-RESULT
            END-IF.

         FIM-PROGRAMA.
      *    DISCONNECT from database
            IF WS-DB-STARTED = "S"
               EXEC SQL
                     DISCONNECT ALL
               END-EXEC
               DISPLAY "0\STATUS" 
               DISPLAY "#Conexao com o banco finalizada"
            END-IF
            
            STOP RUN.
