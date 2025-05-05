       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECTBD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CONNECTION-STRING PIC X(50) VALUE "host=localhost dbname=test".
       PROCEDURE DIVISION.
           DISPLAY "Conectando ao banco de dados...".
           CALL "PQconnectdb" USING CONNECTION-STRING.
           STOP RUN.