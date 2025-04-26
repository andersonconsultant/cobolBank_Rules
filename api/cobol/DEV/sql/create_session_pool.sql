-- SQL script to create and test the session_pool table for COBOL connection pooling

CREATE TABLE IF NOT EXISTS session_pool (
  session_id UUID PRIMARY KEY,
  dbhandle VARCHAR(50) NOT NULL,
  user_id VARCHAR(50) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  last_activity TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Index to speed up inactivity checks
CREATE INDEX IF NOT EXISTS idx_session_last_activity
  ON session_pool(last_activity); 