*> Versão 3: Ajustes para melhorar a manutenção do código
*> COBOL Free-Format - Template para Manutenção
       *> Regras Importantes:
       *> 1. Não use ponto final ao fim de instruções dentro de blocos IF;
       *>    utilize-o somente após o END-IF.
       *> 2. Mantenha indentação e comentários consistentes.
       *> 3. Utilize "*> " para comentários explicativos.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALLOCATE.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Constantes ODBC
       01 SQL-CONSTANTS.
           05 SQL-HANDLE-ENV         PIC S9(4) COMP VALUE 1.
           05 SQL-HANDLE-DBC         PIC S9(4) COMP VALUE 2.
           05 SQL-HANDLE-STMT        PIC S9(4) COMP VALUE 3.
           05 SQL-ATTR-ODBC-VERSION  PIC S9(4) COMP VALUE 200.
           05 SQL-OV-ODBC3           PIC S9(4) COMP VALUE 3.
           05 SQL-DRIVER-NOPROMPT    PIC S9(4) COMP VALUE 0.
       
      *> Variáveis de controle ODBC
       01 ODBC-HANDLES.
           05 hEnv                   USAGE POINTER VALUE NULL.
           05 hDbc                   USAGE POINTER VALUE NULL.
           05 hStmt                  USAGE POINTER VALUE NULL.
       
       01 ODBC-RESULTS.
           05 SQLRC                  PIC S9(4) COMP VALUE 0.
           05 SQL-STATE              PIC X(5) VALUE SPACES.
           05 ERROR-MSG              PIC X(256) VALUE SPACES.
           05 MSG-LEN                PIC S9(4) COMP VALUE 0.
       
      *> Variáveis de aplicação
       01 APPLICATION-DATA.
           05 WS-USER                PIC X(50) VALUE SPACES..
           05 WS-SQL-CONN-STR  PIC X(256) 
               VALUE "DSN=cobolbd;SSLmode=disable".
           05 WS-SQL-CONN-STR-LEN    PIC S9(4) COMP VALUE 256.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIAR
           PERFORM 2000-CONECTAR
           PERFORM 3000-EXECUTAR-QUERY
           PERFORM 9000-FINALIZAR
           STOP RUN.

       1000-INICIAR.
           DISPLAY "Iniciando conexão via ODBC..."
           
           *> Alocação do handle de ambiente
           CALL "SQLAllocHandle" USING
               BY VALUE SQL-HANDLE-ENV
               BY VALUE NULL
               BY REFERENCE hEnv
               GIVING SQLRC
           
           IF SQLRC NOT = 0
               DISPLAY "ERRO: Falha ao criar ambiente ODBC (" SQLRC ")"
               PERFORM 9100-LIBERAR-RECURSOS
               STOP RUN
           END-IF
           
           *> Configuração da versão ODBC
           CALL "SQLSetEnvAttr" USING
               BY VALUE hEnv
               BY VALUE SQL-ATTR-ODBC-VERSION
               BY VALUE SQL-OV-ODBC3
               BY VALUE 0
               GIVING SQLRC
           
           IF SQLRC NOT = 0
               DISPLAY "ERRO: Versão ODBC não suportada (" SQLRC ")"
               PERFORM 9100-LIBERAR-RECURSOS
               STOP RUN
           END-IF.

       2000-CONECTAR.
           *> Alocação do handle de conexão
           CALL "SQLAllocHandle" USING
               BY VALUE SQL-HANDLE-DBC
               BY VALUE hEnv
               BY REFERENCE hDbc
               GIVING SQLRC
           
           IF SQLRC NOT = 0
               DISPLAY "ERRO: Falha ao alocar conexão (" SQLRC ")"
               PERFORM 9100-LIBERAR-RECURSOS
               STOP RUN
           END-IF
           
           *> Conexão com o banco de dados
           CALL "SQLDriverConnect" USING
               BY VALUE hDbc
               BY VALUE NULL
               BY REFERENCE WS-SQL-CONN-STR
               BY VALUE WS-SQL-CONN-STR-LEN
               BY REFERENCE WS-SQL-CONN-STR *> Buffer de saída
               BY VALUE WS-SQL-CONN-STR-LEN
               BY REFERENCE MSG-LEN
               BY VALUE SQL-DRIVER-NOPROMPT
               GIVING SQLRC
           
           IF SQLRC NOT = 0
               PERFORM 9200-OBTER-ERRO
               DISPLAY "ERRO NA CONEXÃO: " SQL-STATE " - " ERROR-MSG
               PERFORM 9100-LIBERAR-RECURSOS
               STOP RUN
           ELSE
               DISPLAY "Conexão estabelecida com sucesso!"
           END-IF.

       3000-EXECUTAR-QUERY.
           *> Alocação do handle de statement
           CALL "SQLAllocHandle" USING
               BY VALUE SQL-HANDLE-STMT
               BY VALUE hDbc
               BY REFERENCE hStmt
               GIVING SQLRC
           
           IF SQLRC NOT = 0
               DISPLAY "ERRO: Falha ao criar statement (" SQLRC ")"
               PERFORM 9100-LIBERAR-RECURSOS
               STOP RUN
           END-IF
           
           *> Execução da consulta
           CALL "SQLExecDirect" USING
               BY VALUE hStmt
               BY CONTENT "SELECT current_user;"
               BY VALUE -3 *> SQL_NTS
               GIVING SQLRC
           
           IF SQLRC NOT = 0
               PERFORM 9200-OBTER-ERRO
               DISPLAY "ERRO NA QUERY: " SQL-STATE " - " ERROR-MSG
               PERFORM 9100-LIBERAR-RECURSOS
               STOP RUN
           END-IF
           
           *> Processamento dos resultados
           PERFORM 3100-PROCESSAR-RESULTADOS.

       3100-PROCESSAR-RESULTADOS.
           CALL "SQLFetch" USING
               BY VALUE hStmt
               GIVING SQLRC
           
           IF SQLRC = 0
               CALL "SQLGetData" USING
                   BY VALUE hStmt
                   BY VALUE 1
                   BY VALUE 1 *> SQL_C_CHAR
                   BY REFERENCE WS-USER
                   BY VALUE 50
                   GIVING SQLRC
               
               IF SQLRC = 0
                   DISPLAY "Usuário atual: " WS-USER
               ELSE
                   DISPLAY "ERRO AO LER DADOS (" SQLRC ")"
               END-IF
           ELSE
               DISPLAY "Nenhum resultado encontrado"
           END-IF.

       9000-FINALIZAR.
           DISPLAY "Finalizando operações..."
           PERFORM 9100-LIBERAR-RECURSOS.

       9100-LIBERAR-RECURSOS.
           IF hStmt NOT = NULL
               CALL "SQLFreeHandle" USING
                   BY VALUE SQL-HANDLE-STMT
                   BY VALUE hStmt
           END-IF
           
           IF hDbc NOT = NULL
               CALL "SQLDisconnect" USING BY VALUE hDbc
               CALL "SQLFreeHandle" USING
                   BY VALUE SQL-HANDLE-DBC
                   BY VALUE hDbc
           END-IF
           
           IF hEnv NOT = NULL
               CALL "SQLFreeHandle" USING
                   BY VALUE SQL-HANDLE-ENV
                   BY VALUE hEnv
           END-IF.

       9200-OBTER-ERRO.
           CALL "SQLGetDiagRec" USING
               BY VALUE SQL-HANDLE-DBC
               BY VALUE hDbc
               BY VALUE 1
               BY REFERENCE SQL-STATE
               BY REFERENCE SQLRC
               BY REFERENCE ERROR-MSG
               BY VALUE LENGTH OF ERROR-MSG
               BY REFERENCE MSG-LEN.

       END PROGRAM ALLOCATE.