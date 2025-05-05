Pool entry

THE ID
[x] Cobol - INSERT ID on BD and API
    [x] BD create table sessions_pool
    [x] Cobol create function to INSERT on START
    [] API GET the output ID - 

Middleware User
[] API - INSERT USER on Cobol, BD and API
    [] UI/UX for Create User
    [] UI/UX for waiting approval
    [] BD create table User_info?
    [] API create User
        Validate the requirement for a valid username.
        API POST Cobol User Validator 
        Cobol Validate if User is existent on BD if not, GO INSERT

[] API GET ID from COBOL 

Pool exit
[] UPDATED_at + Status: Exit where ID 
[] DISCONNECT where ID