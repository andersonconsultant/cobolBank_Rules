      *    Get actual connection process ID
           EXEC SQL
      *    INSERT the ID on VARIABLE
               SELECT pg_backend_pid() INTO :WS-CONN-PID
           END-EXEC
           EXEC SQL
      *    INSERT the ID on DB
               INSERT INTO ins_session_pool (id) VALUES (:WS-CONN-PID) 
           END-EXEC


            DISPLAY "ID" WS-CONN-PID
    EXEC SQL
    SELECT ins_session_pool(id) VALUES :WS-CONN-PID
    END-EXEC

    IF SQLCODE NOT = ZERO
        PERFORM ERROR-RTN
    ELSE
        DISPLAY "1\ID IS ON BANK"
    END-IF   
END-IF
END-IF




server@linux:/srv/cb_Project/Rules/api/cobol/DEV$ gcc -shared -fPIC -o l
ibenvgetter.so getterEnv.c