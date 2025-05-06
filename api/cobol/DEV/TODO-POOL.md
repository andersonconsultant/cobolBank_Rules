# 🗂️ Dev TODO: Implementação e Testes do Connection Pool COBOL + Node.js

Este TODO orienta o time a criar, testar unitariamente e implantar gradualmente o novo sistema de controle de sessões com pool gerenciado pelo COBOL e orquestrado via Node.js.

## Fase 1: Criar Schema no Banco de Dados ✅

- [X] Escrever e validar o script SQL de criação da tabela `sessions_pool`:
  ```sql
  CREATE TABLE sessions_pool (
    session_id UUID PRIMARY KEY,
    dbhandle VARCHAR(50) NOT NULL,
    user_id VARCHAR(50) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_activity TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
  CREATE INDEX idx_session_last_activity ON session_pool(last_activity);
  ```
- [X] Executar script em ambiente de dev e verificar a existência da tabela:
  ```bash
  psql $CONN_STRING -f sql/create_session_pool.sql
  psql $CONN_STRING -c "\d session_pool"
  ```
- [X] Inserir registros de teste e validar consultas de `last_activity`:
  ```sql
  INSERT INTO session_pool(session_id, dbhandle, user_id)
    VALUES('00000000-0000-0000-0000-000000000001','H1','tester');
  SELECT * FROM session_pool;
  ```

## Fase 2: Data Access + Executor COBOL ✅

- [X] Criar módulo de acesso SQL em COBOL:
  - Arquivos: `open_pool.cbl`, `exec_pool.cbl`, `close_pool.cbl`.
  - Estrutura básica: armazenar/recuperar `dbhandle` e `last_activity`.
- [X] Escrever pseudocódigo COBOL para:
  - Conectar e inserir em `sessions_pool` (open).
  - Executar operações via handle (exec).
  - Desconectar e remover registro (close).
- [X] Compilar e rodar testes manuais:
  ```bash
  ./cobol-compile.sh open_pool.cbl && ./open_pool H1
  ```
- [X] Validar atualização de timestamp e remoção de sessão.

## Fase 3: Sistema de Logs ✅

- [X] Implementar estrutura de log em COBOL:
  ```cobol
  Teste 1: {
    session_id: 24270,
    component: COBOL,
    action: UPDATE,
    status: ERROR,
    response_time: 558ms,
    message: "Teste automatizado de log",
    endpoint: /api/v1/saldo,
    method: PUT,
    value: 9472.64
  }
  ```
- [X] Criar parser JavaScript para logs:
  - Processamento de saída COBOL
  - Formatação JSON estruturada
  - Validação de campos

## Fase 4: Próximos Passos - Integração com Banco de Dados

- [ ] Criar tabela de logs no PostgreSQL:
  ```sql
  CREATE TABLE log_cobol (
    id SERIAL PRIMARY KEY,
    session_id VARCHAR(10),
    component VARCHAR(10),
    action VARCHAR(20),
    status VARCHAR(10),
    response_time INTEGER,
    message VARCHAR(100),
    endpoint VARCHAR(50),
    method VARCHAR(6),
    value_processed NUMERIC(12,2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
  ```

- [ ] Implementar inserção de logs no COBOL:
  ```cobol
  EXEC SQL
    INSERT INTO log_cobol (
      session_id, component, action, status,
      response_time, message, endpoint, method, value_processed
    ) VALUES (
      :WS-SESSION-ID, :WS-COMPONENT, :WS-ACTION, :WS-STATUS,
      :WS-RESPONSE-TIME, :WS-MESSAGE, :WS-ENDPOINT, :WS-METHOD, :WS-VALUE
    );
  END-EXEC.
  ```

- [ ] Criar endpoints Node.js para consulta de logs:
  - GET /api/v1/logs - Listar todos os logs
  - GET /api/v1/logs/:session_id - Filtrar por sessão
  - GET /api/v1/logs/component/:component - Filtrar por componente
  - GET /api/v1/logs/status/:status - Filtrar por status

## Fase 5: Monitoramento e Métricas

- [ ] Implementar dashboard de monitoramento:
  - Tempo médio de resposta por endpoint
  - Taxa de erros por componente
  - Volume de operações por método
  - Valor total processado por período

- [ ] Adicionar alertas para:
  - Tempo de resposta > 1000ms
  - Taxa de erro > 5%
  - Falhas de conexão com banco
  - Sessões órfãs

## Fase 6: Testes de Integração

- [ ] Criar suite de testes automatizados:
  - Validação de formato dos logs
  - Persistência no banco de dados
  - Consultas via API REST
  - Cenários de erro e recuperação

## Fase 7: Documentação e Deploy

- [ ] Documentar:
  - Estrutura do log
  - Formato de saída COBOL
  - APIs de consulta
  - Procedimentos de troubleshooting

- [ ] Preparar rollout:
  - Deploy em staging
  - Testes de carga
  - Monitoramento inicial
  - Plano de rollback

---
**Observação:** Sistema de logs implementado e testado com sucesso. Próximo foco: persistência em banco de dados e APIs de consulta.

id	SERIAL	Identificador único.
usuario	TEXT	Usuário responsável pela ação.
acao	TEXT	Exemplo: "GET SALDO", "START CONEXAO", "DISCONNECT"
descricao	TEXT	Detalhes do que aconteceu (texto livre, exemplo: "Saldo convertido: 775.54").
pid_backend	INTEGER	ID retornado por pg_backend_pid() (ajuda em rastreamento).
valor	NUMERIC(12,2)	Valor monetário envolvido (se houver).
endpoint	TEXT	Exemplo: "/api/v1/saldo"
ip_origem	INET ou TEXT	IP da requisição, se disponível.
data_hora	TIMESTAMP	Quando ocorreu. DEFAULT now()

## Fase 8: Rollout Gradual e Segurança

- [ ] Deploy em ambiente de staging com feature flag `COBOL_POOL=true`.
- [ ] Implementar rate limiting para proteção dos endpoints.
- [ ] Adicionar sanitização de dados e validação de inputs.
- [ ] Implementar auditoria de operações.
- [ ] Monitorar métricas: conexões ativas, tempo médio de resposta, leaks.
- [ ] Habilitar para pequenos grupos de usuários e validar comportamento.
- [ ] Expandir progressivamente até todos os usuários.

---
**Observação:** Cada etapa deve ser aprovada em code review antes de prosseguir. O foco atual é na robustez e confiabilidade do processo único antes de partir para múltiplas instâncias. Mantenha este TODO atualizado conforme avançamos.

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