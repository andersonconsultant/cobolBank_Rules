-- Criar sequence para o ID (compatível com COBOL)
CREATE SEQUENCE IF NOT EXISTS log_cobol_id_seq;

-- Criar tabela de log
CREATE TABLE IF NOT EXISTS log_cobol (
    id INTEGER DEFAULT nextval('log_cobol_id_seq') PRIMARY KEY,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    session_id VARCHAR(10),     -- ID:25103, ID:27938, etc (apenas números)
    component VARCHAR(10),      -- FRONTEND, BACKEND, COBOL
    action VARCHAR(20),         -- ENGINE_START, GET_SALDO, DB_CONNECTION
    status VARCHAR(10),         -- SUCCESS, ERROR, INFO
    response_time INTEGER,      -- em millisegundos
    message VARCHAR(100),       -- mensagem resumida do log
    endpoint VARCHAR(50),       -- /api/v1/saldo
    method VARCHAR(6),          -- GET, POST, etc
    value_processed NUMERIC(12,2) -- valor monetário quando aplicável
);

-- Índices para otimização de consultas
CREATE INDEX IF NOT EXISTS idx_log_cobol_timestamp ON log_cobol(timestamp);
CREATE INDEX IF NOT EXISTS idx_log_cobol_session_id ON log_cobol(session_id);
CREATE INDEX IF NOT EXISTS idx_log_cobol_action ON log_cobol(action);

-- Comentários da tabela
COMMENT ON TABLE log_cobol IS 'Logs de operações do sistema COBOL';
COMMENT ON COLUMN log_cobol.session_id IS 'ID numérico da sessão COBOL';
COMMENT ON COLUMN log_cobol.component IS 'Componente que gerou o log';
COMMENT ON COLUMN log_cobol.action IS 'Tipo de operação realizada';
COMMENT ON COLUMN log_cobol.status IS 'Status da operação';
COMMENT ON COLUMN log_cobol.response_time IS 'Tempo de resposta em ms';
COMMENT ON COLUMN log_cobol.message IS 'Descrição curta da operação';
COMMENT ON COLUMN log_cobol.value_processed IS 'Valor monetário processado';

-- Função para limpar logs antigos (retenção de 30 dias)
CREATE OR REPLACE FUNCTION cleanup_old_logs() RETURNS void AS $$
BEGIN
    DELETE FROM log_cobol 
    WHERE timestamp < CURRENT_TIMESTAMP - INTERVAL '30 days';
END;
$$ LANGUAGE plpgsql; 