*> COBOL Free-Format - Template para Manutenção
       *> Regras Importantes:
       *> 1. Não use ponto final ao fim de instruções dentro de blocos IF;
       *>    utilize-o somente após o END-IF.
       *> 2. Mantenha indentação e comentários consistentes.
       *> 3. Utilize "*> " para comentários explicativos.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALLOCATE.
       
       ENVIRONMENT DIVISION.
       *> CONFIGURATION SECTION e SPECIAL-NAMES estão comentadas.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 SQLRC          PIC S9(4) COMP VALUE 0.
           77 hEnv           USAGE POINTER.
           77 hDbc           USAGE POINTER.
           77 hStmt          USAGE POINTER.
           77 WS-USER        PIC X(50) VALUE SPACES.
           77 WS-SQL_C_CHAR  PIC S9(4) COMP VALUE 1.    *> Geralmente 1 para SQL_C_CHAR.
           77 WS-SQL_NTS     PIC S9(4) COMP VALUE -3.   *> Valor comum para SQL_NTS.
           77 WS-ERROR-MSG   PIC X(256) VALUE SPACES.   *> Buffer para mensagem de erro.
       
       PROCEDURE DIVISION.
           PERFORM MAIN
           STOP RUN.
       MAIN.
           DISPLAY "Iniciando conexao via ODBC..." UPON CONSOLE
       
           *> Aloca handle de ambiente (SQL_HANDLE_ENV = 1)
           CALL "SQLAllocHandle"
                USING BY VALUE 1            *> Indica SQL_HANDLE_ENV
                      BY REFERENCE NULL
                      BY REFERENCE hEnv
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
               DISPLAY "Erro ao alocar handle de ambiente. SQLRC: " SQLRC UPON CONSOLE
               STOP RUN
           END-IF
       
           DISPLAY "Handle de ambiente (hEnv) alocado: " hEnv UPON CONSOLE
       
           *> Configura a versão ODBC (SQL_ATTR_ODBC_VERSION = 200, SQL_OV_ODBC3 = 3)
           CALL "SQLSetEnvAttr"
                USING BY VALUE hEnv         *> Handle de ambiente
                      BY VALUE 200          *> SQL_ATTR_ODBC_VERSION
                      BY VALUE 3            *> SQL_OV_ODBC3
                      BY VALUE 0            *> StringLength (0)
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                DISPLAY "Erro ao definir a versao ODBC. SQLRC: " SQLRC UPON CONSOLE
                STOP RUN
           END-IF
       
           *> Aloca handle de conexao (SQL_HANDLE_DBC = 2)
           CALL "SQLAllocHandle"
                USING BY VALUE 2            *> Indica SQL_HANDLE_DBC
                      BY VALUE hEnv         *> Passa o handle de ambiente por valor
                      BY REFERENCE hDbc
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                DISPLAY "Erro ao alocar handle de conexao. SQLRC: " SQLRC UPON CONSOLE
                STOP RUN
           ELSE
                DISPLAY "Handle de conexao (hDbc) alocado com sucesso." UPON CONSOLE
           END-IF
       
           *> Conecta ao banco de dados usando as credenciais:
           *> DSN: cobolbd, USUÁRIO: server, SENHA: pwdbd123
           DISPLAY "Tentando conectar ao banco de dados..." UPON CONSOLE
           CALL "SQLConnect"
                USING BY VALUE hDbc
                      BY CONTENT "cobolbd"
                      BY CONTENT "server"
                      BY CONTENT "pwdbd123"
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                *> Obter mensagem de erro detalhada
                CALL "SQLError"
                     USING BY VALUE hEnv
                           BY VALUE hDbc
                           BY VALUE hStmt
                           BY REFERENCE WS-ERROR-MSG
                           BY VALUE 256
                     GIVING SQLRC
                DISPLAY "Erro na conexao com o banco. SQLRC: " SQLRC UPON CONSOLE
                DISPLAY "Mensagem de erro: " WS-ERROR-MSG UPON CONSOLE
                STOP RUN
           END-IF
       
           DISPLAY "Conexao com o banco estabelecida com sucesso." UPON CONSOLE
       
           *> Aloca handle de statement (SQL_HANDLE_STMT = 3)
           CALL "SQLAllocHandle"
                USING BY VALUE 3            *> Indica SQL_HANDLE_STMT
                      BY VALUE hDbc         *> Handle de conexao ativo
                      BY REFERENCE hStmt
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                DISPLAY "Erro ao alocar handle de statement. SQLRC: " SQLRC UPON CONSOLE
                STOP RUN
           END-IF
       
           *> Executa a query: SELECT current_user;
           CALL "SQLExecDirect"
                USING BY VALUE hStmt
                      BY CONTENT "SELECT current_user;"
                      BY VALUE WS-SQL_NTS
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                DISPLAY "Erro ao executar SQLExecDirect. SQLRC: " SQLRC UPON CONSOLE
                STOP RUN
           END-IF
       
           *> Busca a proxima linha do resultado (SQLFetch)
           CALL "SQLFetch"
                USING BY VALUE hStmt
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                DISPLAY "Erro ao buscar dados (SQLFetch). SQLRC: " SQLRC UPON CONSOLE
                STOP RUN
           END-IF
       
           *> Obter os dados da primeira coluna (current_user) como C_CHAR
           CALL "SQLGetData"
                USING BY VALUE hStmt
                      BY VALUE 1               *> Número da coluna
                      BY VALUE WS-SQL_C_CHAR   *> Tipo de dado C (SQL_C_CHAR)
                      BY REFERENCE WS-USER     *> Buffer para os dados
                      BY VALUE 50              *> Tamanho do buffer
                GIVING SQLRC
           IF SQLRC NOT = 0 THEN
                DISPLAY "Erro ao obter dados (SQLGetData). SQLRC: " SQLRC UPON CONSOLE
                STOP RUN
           END-IF
       
           DISPLAY "Current user: " WS-USER UPON CONSOLE
           STOP RUN.