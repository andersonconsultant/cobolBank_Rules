IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLCommands.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 hEnv   USAGE POINTER.
           77 hDbc   USAGE POINTER.
           77 USER-CHOICE PIC 9 VALUE 0.
           77 WS-ENV-VALUE PIC X(100).

       PROCEDURE DIVISION.
       MAIN.
           *> Aceita o valor da variável de ambiente "COB_LIBRARY_PATH"
           ACCEPT WS-ENV-VALUE FROM ENVIRONMENT "COB_LIBRARY_PATH".
           DISPLAY "COB_LIBRARY_PATH: " WS-ENV-VALUE UPON CONSOLE.
           DISPLAY FUNCTION CURRENT-DATE UPON CONSOLE
           DISPLAY "Escolha uma opção:"
           DISPLAY "1. Iniciar conexão"
           DISPLAY "2. Desconectar"
           ACCEPT USER-CHOICE

           EVALUATE USER-CHOICE
                WHEN 1
                    CALL "ALLOCATE" USING hEnv, hDbc
                WHEN 2
                    CALL "DISCONNECTSQL" USING hDbc
                WHEN OTHER
                    DISPLAY "Opção inválida." UPON CONSOLE
            END-EVALUATE.

           STOP RUN.
