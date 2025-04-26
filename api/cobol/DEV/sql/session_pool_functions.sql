-- SQL procedure to simplify inserting into session_pool (COBOL pooling)

-- Drop previous function if exists
DROP FUNCTION IF EXISTS s_pool_ins(uuid, varchar, varchar);

-- Create stored procedure for session insert
CREATE PROCEDURE s_pool_ins(
    p_session_id UUID,
    p_dbhandle VARCHAR,
    p_user_id VARCHAR
)
LANGUAGE plpgsql
AS $$
BEGIN
    INSERT INTO session_pool(session_id, dbhandle, user_id)
    VALUES(p_session_id, p_dbhandle, p_user_id);
END;
$$; 