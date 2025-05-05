      *    Get actual connection process ID
           EXEC SQL
      *    INSERT the ID on VARIABLE
               SELECT pg_backend_pid() INTO :WS-CONN-PID
           END-EXEC
           EXEC SQL
      *    INSERT the ID on DB
               INSERT INTO ins_session_pool (id) VALUES (:WS-CONN-PID) 
           END-EXEC