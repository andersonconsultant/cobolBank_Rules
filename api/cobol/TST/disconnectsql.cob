       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISCONNECTSQL.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 SQLRC  PIC S9(4) COMP VALUE 0.
           77 hDbc   USAGE POINTER.
           77 hEnv   USAGE POINTER.
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Desconectando..." UPON CONSOLE.
           
           *> Desaloca handle de conexão (SQL_HANDLE_DBC = 2)
           CALL "SQLFreeHandle" USING 
                BY VALUE 2,        *> SQL_HANDLE_DBC
                BY REFERENCE hDbc
                GIVING SQLRC.
           IF SQLRC NOT = 0 THEN
               DISPLAY "Erro ao desalocar handle de conexão. SQLRC: " SQLRC UPON CONSOLE
               STOP RUN
           END-IF.
           
           *> Desaloca handle de ambiente (SQL_HANDLE_ENV = 1)
           CALL "SQLFreeHandle" USING 
                BY VALUE 1,        *> SQL_HANDLE_ENV
                BY REFERENCE hEnv
                GIVING SQLRC.
           IF SQLRC NOT = 0 THEN
               DISPLAY "Erro ao desalocar handle de ambiente. SQLRC: " SQLRC UPON CONSOLE
               STOP RUN
           ELSE
               DISPLAY "Desconexão realizada com sucesso." UPON CONSOLE
           END-IF.
           
           STOP RUN.
       