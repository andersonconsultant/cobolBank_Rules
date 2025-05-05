*> Versão 5: Refatoração para OpenESQL e simplicidade
*> COBOL Free-Format - Template para Manutenção
       *> Regras Importantes:
       *> 1. Não use ponto final ao fim de instruções dentro de blocos IF;
       *>    utilize-o somente após o END-IF.
       *> 2. Mantenha indentação e comentários consistentes.
       *> 3. Utilize "*> " para comentários explicativos.
       *> 4. Mantenha a manuntenabilidade do código.
       *> 5. Use o gerenciamento de Buffer por OpenESQL para não crashar o programa.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETUSER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Variáveis de controle OpenESQL
       01 ESQL-VARS.
           05 SQLCODE PIC S9(9) COMP VALUE 0.

      *> Variáveis de aplicação
       01 APPLICATION-DATA.
           05 WS-USER                PIC X(50) VALUE SPACES.
           05 WS-SQL-CONN-STR        PIC X(256)
               VALUE "DSN=cobolbd;ClientEncoding=UTF8;SSLmode=disable".
           05 WS-SQL-CONN-STR-LEN    PIC S9(9) COMP VALUE 256. *> Usado pelo Connect
      *> OpenESQL: Define a query diretamente
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Iniciando programa..."
           PERFORM 2000-CONECTAR
           PERFORM 3000-OBTER-USUARIO
           PERFORM 9000-FINALIZAR
           STOP RUN.

       2000-CONECTAR.
           DISPLAY "Conectando ao banco de dados..."
      *> OpenESQL: Conecta ao banco
           EXEC SQL
               CONNECT TO :WS-SQL-CONN-STR
           END-EXEC
           IF SQLCODE NOT = 0 THEN
               DISPLAY "ERRO NA CONEXÃO: " SQLERRMC
               PERFORM 9000-FINALIZAR
               STOP RUN
           ELSE
               DISPLAY "Conexão estabelecida com sucesso!"
           END-IF.

       3000-OBTER-USUARIO.
           DISPLAY "Obtendo usuário..."
      *> OpenESQL: Executa a query e armazena o resultado em WS-USER
           EXEC SQL
               SELECT get_usr() INTO :WS-USER
           END-EXEC
           IF SQLCODE NOT = 0 THEN
               DISPLAY "ERRO NA QUERY: " SQLERRMC
               PERFORM 9000-FINALIZAR
               STOP RUN
           ELSE
               DISPLAY "Usuário atual: " WS-USER
           END-IF.

       9000-FINALIZAR.
           DISPLAY "Finalizando operações..."
      *> OpenESQL: Desconecta do banco
           EXEC SQL
               DISCONNECT ALL
           END-EXEC
           IF SQLCODE NOT = 0 THEN
               DISPLAY "ERRO NA DESCONEXÃO: " SQLERRMC
           END-IF.

       END PROGRAM GETUSER.