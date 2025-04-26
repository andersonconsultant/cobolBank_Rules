-- Simplify session_pool insert by using defaults and short procedure

-- Allow defaults for dbhandle, user_id and last_activity
ALTER TABLE session_pool
  ALTER COLUMN dbhandle SET DEFAULT 'cobolbd',
  ALTER COLUMN user_id SET DEFAULT 'server',
  ALTER COLUMN last_activity SET DEFAULT CURRENT_TIMESTAMP;

-- Rename column session_id to id for shorter inserts
ALTER TABLE session_pool RENAME COLUMN session_id TO id;

-- Create a short procedure that only takes id
DROP PROCEDURE IF EXISTS s_pool_ins_id(uuid);
CREATE PROCEDURE s_pool_ins_id(
  p_id UUID
)
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO session_pool(id)
  VALUES(p_id);
END;
$$; 