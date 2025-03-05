       $SET SQL(dbman=odbc, autocommit)
       *> Diretiva OpenESQL: usa ODBC e habilita autocommit

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALLOCATE.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Inclui a definição do SQL Communications Area
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       *> Variável para a string de conexão ODBC
       01 WS-SQL-CONN-STR        PIC X(256)
           VALUE "DSN=cobolbd;ClientEncoding=UTF8;SSLmode=disable".
       
       *> Variável para armazenar o resultado da query (nome do usuário)
       01 WS-USER                PIC X(50) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Conectando ao banco de dados via OpenESQL..."
           EXEC SQL
                CONNECT TO :WS-SQL-CONN-STR
           END-EXEC.
           
           IF SQLCODE NOT = 0 THEN
              DISPLAY "ERRO NA CONEXÃO: " SQLCODE " " SQLSTATE
              STOP RUN
           END-IF.
           
           DISPLAY "Conexão estabelecida com sucesso!"
           
           *> Executa a query para obter o nome do usuário atual
           EXEC SQL
                SELECT get_usr() INTO :WS-USER
           END-EXEC.
           
           IF SQLCODE = 0 THEN
              DISPLAY "Usuário atual: " WS-USER
           ELSE
              DISPLAY "ERRO NA QUERY: " SQLCODE " " SQLSTATE
           END-IF.
           
           EXEC SQL
                DISCONNECT CURRENT
           END-EXEC.
           
           STOP RUN.
