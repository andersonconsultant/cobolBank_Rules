# 🗂️ Dev TODO: Implementação e Testes do Connection Pool COBOL + Node.js

Este TODO orienta o time a criar, testar unitariamente e implantar gradualmente o novo sistema de controle de sessões com pool gerenciado pelo COBOL e orquestrado via Node.js.

## Fase 1: Criar Schema no Banco de Dados

- [ ] Escrever e validar o script SQL de criação da tabela `session_pool`:
  ```sql
  CREATE TABLE session_pool (
    session_id UUID PRIMARY KEY,
    dbhandle VARCHAR(50) NOT NULL,
    user_id VARCHAR(50) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_activity TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
  CREATE INDEX idx_session_last_activity ON session_pool(last_activity);
  ```
- [ ] Executar script em ambiente de dev e verificar a existência da tabela:
  ```bash
  psql $CONN_STRING -f sql/create_session_pool.sql
  psql $CONN_STRING -c "\d session_pool"
  ```
- [ ] Inserir registros de teste e validar consultas de `last_activity`:
  ```sql
  INSERT INTO session_pool(session_id, dbhandle, user_id)
    VALUES('00000000-0000-0000-0000-000000000001','H1','tester');
  SELECT * FROM session_pool;
  ```

## Fase 2: Data Access + Executor COBOL

- [ ] Criar módulo de acesso SQL em COBOL:
  - Arquivos: `open_pool.cbl`, `exec_pool.cbl`, `close_pool.cbl`.
  - Estrutura básica: armazenar/recuperar `dbhandle` e `last_activity`.
- [ ] Escrever pseudocódigo COBOL para:
  - Conectar e inserir em `session_pool` (open).
  - Executar operações via handle (exec).
  - Desconectar e remover registro (close).
- [ ] Compilar e rodar testes manuais:
  ```bash
  ./cobol-compile.sh open_pool.cbl && ./open_pool H1
  ```
- [ ] Validar atualização de timestamp e remoção de sessão.

## Fase 3: Endpoints no Node.js

- [ ] Criar rotas REST em `src/api/routes/cobolPool.js`:
  - `POST /api/v1/cobol/pool/open`
  - `POST /api/v1/cobol/pool/exec`
  - `POST /api/v1/cobol/pool/close`
- [ ] Implementar controllers que chamam o executor COBOL:
  ```js
  const result = await cobolExecutor('open', { userId });
  ```
- [ ] Mockar o módulo de execução COBOL para testes unitários.
- [ ] Configurar variáveis de ambiente no `.env`:
  ```ini
  COBOL_DB_USER=server
  COBOL_DB_PASS=pwdbd123
  COBOL_DB_NAME=cobolbd
  TOKEN_TTL=15m
  ```

## Fase 4: Unit Tests

- [ ] Testar módulo SQL/COBOL com stubs de banco (mock session_pool queries).
- [ ] Testar controllers Node.js usando Jest ou Mocha:
  - Mock de `cobolExecutor` para cada operação (open, exec, close).
  - Assertions sobre payload enviado e respostas tratadas.
- [ ] Cobertura mínima de 80%.

## Fase 5: Testes de Integração (E2E)

- [ ] Configurar um container Docker com PostgreSQL e ambiente COBOL.
- [ ] Rodar testes que:
  1. Fazem login e `open` (cria sessão).
  2. Chamam `exec` várias vezes e validam `last_activity` atualizado.
  3. Simulam inatividade e disparam rotina de cleanup COBOL.
  4. Chamam `exec` após TTL e recebem erro de sessão expirada.
  5. Chamam `close` e verificam remoção definitiva.

## Fase 6: Rollout Gradual

- [ ] Deploy em ambiente de staging com feature flag `COBOL_POOL=true`.
- [ ] Monitorar métricas: conexões ativas, tempo médio de resposta, leaks.
- [ ] Habilitar para pequenos grupos de usuários e validar comportamento.
- [ ] Expandir progressivamente até todos os usuários.

---
**Observação:** Cada etapa deve ser aprovada em code review antes de prosseguir. Mantenha este TODO atualizado conforme avançamos. 


Exemplos:
      * Insert new session into session_pool table
           EXEC SQL
               INSERT INTO session_pool(session_id, dbhandle, user_id, last_activity)
               VALUES(:WS-SESSION-ID, :DBNAME, :USERNAME, CURRENT_TIMESTAMP)
           END-EXEC.
           IF SQLCODE NOT = ZERO
              PERFORM ERROR-RTN
              STOP RUN
           END-IF.