       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALLOCATE.
       
       ENVIRONMENT DIVISION.
       *> As diretivas de CONFIGURATION SECTION e SPECIAL-NAMES foram comentadas,
       *> pois você optou por fazer a linkagem direta com o ODBC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 SQLRC   PIC S9(4) COMP VALUE 0.
           77 hEnv    USAGE POINTER.
           77 hDbc    USAGE POINTER.
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Iniciando conexão..." UPON CONSOLE.
       
           *> Aloca handle de ambiente (SQL_HANDLE_ENV = 1)
           CALL "SQLAllocHandle" USING 
                BY VALUE 1,        *> Indica SQL_HANDLE_ENV
                BY REFERENCE NULL,
                BY REFERENCE hEnv
                GIVING SQLRC.
           IF SQLRC NOT = 0 THEN
               DISPLAY "Erro ao alocar handle de ambiente. SQLRC: " SQLRC UPON CONSOLE
               STOP RUN
           END-IF.
           
           DISPLAY "Handle de ambiente (hEnv) alocado: " hEnv UPON CONSOLE.
       
           *> Define a versão ODBC para SQL_OV_ODBC3
           CALL "SQLSetEnvAttr" USING 
                BY VALUE hEnv,     *> Passa o valor do handle de ambiente
                BY VALUE 200,      *> SQL_ATTR_ODBC_VERSION (geralmente 200 conforme os headers)
                BY VALUE 3,        *> SQL_OV_ODBC3 (geralmente 3)
                BY VALUE 0         *> StringLength (usualmente 0)
                GIVING SQLRC.
           IF SQLRC NOT = 0 THEN
               DISPLAY "Erro ao definir a versão ODBC. SQLRC: " SQLRC UPON CONSOLE
               STOP RUN
           END-IF.
       
           *> Aloca handle de conexão (SQL_HANDLE_DBC = 2)
           CALL "SQLAllocHandle" USING 
                BY VALUE 2,        *> Indica SQL_HANDLE_DBC
                BY VALUE hEnv,     *> Passa o handle de ambiente por valor
                BY REFERENCE hDbc,
                GIVING SQLRC.
           IF SQLRC NOT = 0 THEN
               DISPLAY "Erro ao alocar handle de conexão. SQLRC: " SQLRC UPON CONSOLE
               STOP RUN
           ELSE
               DISPLAY "Handle de conexão alocado com sucesso." UPON CONSOLE
               DISPLAY "Handle de conexão (hDbc) alocado: " hDbc UPON CONSOLE
           END-IF.
       
           STOP RUN.
       