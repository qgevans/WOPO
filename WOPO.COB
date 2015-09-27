       IDENTIFICATION DIVISION.
       PROGRAM-ID. "WOPO".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONFIG
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CONFIG-KEY.
           SELECT USERS
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS USER-NAME.
           SELECT CHANNELS
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CONFIG.
       01 CONFIG-RECORD.
           03 CONFIG-KEY PIC X(16).
           03 CONFIG-VALUE PIC X(64).
       FD USERS.
       01 USER-RECORD.
           03 USER-NAME PIC X(16).
           03 USER-LEVEL PIC 9(2).
       FD CHANNELS.
       01 CHANNEL-RECORD.
           03 CHANNEL-NAME PIC X(50).
       
       WORKING-STORAGE SECTION.
       01 STATE PIC 9(2).
           88 SUCCESS VALUE 0.
           88 DONE VALUE 99.
       01 BUFFER.
           03 MSG-LENGTH PIC 9(3).
           03 MSG-BODY PIC X(512).
       01 WOPO.
           03 WOPO-NICK PIC X(16).
       01 IRC-MESSAGE.
           03 PREFIX.
               05 NICK PIC X(16).
               05 IDENT PIC X(16).
               05 HOST PIC X(64).
           03 COMMAND PIC X(16).
	       88 PING VALUE "PING".
	       88 PRIVMSG VALUE "PRIVMSG".
	       88 NOTICE VALUE "NOTICE".
           03 PARAMETERS.
               05 TARGET PIC X(50).
               05 REST PIC X(480).
       01 WAITING-COMMAND PIC X(16).
       01 PARAMS.
           03 WORK PIC X(50).
           03 WORK-PREFIX REDEFINES WORK PIC X.
               88 IS-COMMAND VALUE "$".
           03 PARAM PIC X(50) OCCURS 5 TIMES.
	   03 REG PIC X(50) OCCURS 5 TIMES.

       PROCEDURE DIVISION.
           DISPLAY "CONFIGURATION FOLLOWS:".
           CALL "PRINT-CONFIG".
           MOVE LENGTH OF MSG-BODY TO MSG-LENGTH.
           CALL "CHANNEL-INIT"
               USING BUFFER.
           OPEN INPUT CONFIG.
           MOVE "SERVER" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           MOVE 1 TO MSG-LENGTH.
           STRING
               CONFIG-VALUE, DELIMITED BY SPACE,
               INTO MSG-BODY,
               WITH POINTER MSG-LENGTH.
           CALL "CHANNEL-OPEN", GIVING STATE.
           IF NOT SUCCESS THEN DISPLAY MSG-BODY
                               GO TO DIE.
           MOVE "NICK" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
	   MOVE CONFIG-VALUE TO WOPO-NICK.
           MOVE 1 TO MSG-LENGTH.
           INITIALIZE MSG-BODY.
	   STRING "NICK"
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           ADD 1 TO MSG-LENGTH.
           STRING WOPO-NICK DELIMITED BY SPACE,
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
	   PERFORM SEND-LINE.
           MOVE 1 TO MSG-LENGTH.
           INITIALIZE MSG-BODY.
	   STRING "USER"
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           ADD 1 TO MSG-LENGTH.
           MOVE "IDENT" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING CONFIG-VALUE DELIMITED BY SPACE,
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           ADD 1 TO MSG-LENGTH.
           MOVE "REAL-NAME" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING "BOGUS HOST :" DELIMITED BY SIZE,
                  CONFIG-VALUE DELIMITED BY "  ",
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           PERFORM SEND-LINE.
           MOVE "NICKSERV" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-KEY.
           IF CONFIG-KEY IS NOT EQUAL TO SPACES THEN
               INITIALIZE MSG-BODY
               MOVE 1 TO MSG-LENGTH
               STRING "PRIVMSG NICKSERV :IDENTIFY " DELIMITED BY SIZE,
                      CONFIG-VALUE DELIMITED BY SPACE,
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               PERFORM SEND-LINE.
           OPEN INPUT CHANNELS.
           PERFORM AUTOJOIN-CHANNELS UNTIL DONE.
           CLOSE CHANNELS.
           OPEN I-O USERS.
           PERFORM MAIN FOREVER.

       DIE.
           DISPLAY STATE.
           STOP RUN.

       AUTOJOIN-CHANNELS.
           READ CHANNELS RECORD
               AT END MOVE 99 TO STATE.
           IF NOT DONE THEN
               MOVE 1 TO MSG-LENGTH
               STRING "JOIN " DELIMITED BY SIZE,
                      CHANNEL-NAME DELIMITED BY SPACES,
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
	   PERFORM SEND-LINE.

       READ-CONFIG-ENTRY.
           READ CONFIG RECORD
               INVALID KEY DISPLAY "REQUIRED KEY UNSPECIFIED:"
                           DISPLAY CONFIG-KEY
                           GO TO DIE.           

       SEND-LINE.
           CALL "CHANNEL-SEND" GIVING STATE.
           IF NOT SUCCESS THEN DISPLAY MSG-BODY
                               GO TO DIE.

       RECEIVE-LINE.
           INITIALIZE MSG-BODY.
           CALL "CHANNEL-RECV" GIVING STATE.
           IF NOT SUCCESS THEN GO TO DIE.
           CALL "IRC-MSG" USING BUFFER, IRC-MESSAGE.

       WAIT-FOR-COMMAND.
           PERFORM RECEIVE-LINE UNTIL COMMAND EQUALS WAITING-COMMAND.

       GET-PARAMS.
           UNSTRING REST DELIMITED BY SPACE INTO
               PARAM(1)
               PARAM(2)
               PARAM(3)
               PARAM(4)
               PARAM(5).
           MOVE PARAM(1) TO WORK.

       VALIDATE-USER.
           MOVE NICK TO USER-NAME.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           IF USER-LEVEL IS GREATER THAN 0 THEN
	       INITIALIZE MSG-BODY
               MOVE 1 TO MSG-LENGTH
               STRING "PRIVMSG NICKSERV :ACC"
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               ADD 1 TO MSG-LENGTH
               STRING NICK
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               PERFORM SEND-LINE
               MOVE "NOTICE" TO WAITING-COMMAND
               MOVE 0 TO STATE
               PERFORM WAIT-FOR-ACC UNTIL DONE.

       WAIT-FOR-ACC.
           PERFORM WAIT-FOR-COMMAND.
           PERFORM GET-PARAMS.
           IF PARAM(1) EQUALS USER-NAME AND PARAM(2) EQUALS "ACC" THEN
               MOVE 99 TO STATE
               IF PARAM(3) IS NOT EQUAL TO "3" THEN
                   MOVE 0 TO USER-LEVEL
               ELSE NEXT SENTENCE
           ELSE INITIALIZE COMMAND.

       MAIN.
           PERFORM RECEIVE-LINE.
           MOVE 1 TO MSG-LENGTH.
           IF PING THEN
               PERFORM PONG
           ELSE IF PRIVMSG THEN
               PERFORM HANDLE-MESSAGE
	   ELSE IF NOTICE THEN
	       PERFORM HANDLE-MESSAGE.

       BEGIN-REPLY.
           INITIALIZE MSG-BODY.
           MOVE 1 TO MSG-LENGTH.
           STRING COMMAND
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           ADD 1 TO MSG-LENGTH.
           IF TARGET IS EQUAL TO WOPO-NICK THEN
               STRING NICK
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
           ELSE
               STRING TARGET
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH.
           ADD 1 TO MSG-LENGTH.
           STRING ":"
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.

       PONG.
	   STRING "PONG"
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           PERFORM SEND-LINE.

       HANDLE-MESSAGE.
           PERFORM GET-PARAMS.
           MOVE PARAM(1) TO WORK.
           IF IS-COMMAND THEN
               UNSTRING WORK DELIMITED BY "$" INTO PARAM(1), PARAM(1)
               IF PARAM(1) IS EQUAL TO "HELP" THEN
                   PERFORM HANDLE-HELP
               ELSE IF PARAM(1) IS EQUAL TO "SOURCE" THEN
                   PERFORM HANDLE-SOURCE
               ELSE IF PARAM(1) IS EQUAL TO "LEVEL" THEN
                   PERFORM HANDLE-LEVEL
               ELSE IF PARAM(1) IS EQUAL TO "OP" THEN
                   PERFORM HANDLE-OP
               ELSE IF PARAM(1) IS EQUAL TO "JOIN" THEN
                   PERFORM HANDLE-JOIN
               ELSE IF PARAM(1) IS EQUAL TO "PART" THEN
                   PERFORM HANDLE-PART
               ELSE IF PARAM(1) IS EQUAL TO "QUIT" THEN
                   PERFORM HANDLE-QUIT
               ELSE IF PARAM(1) IS EQUAL TO "RELEVEL" THEN
                   PERFORM HANDLE-RELEVEL.

       HANDLE-HELP.
           PERFORM BEGIN-REPLY.
           STRING "$HELP $LEVEL $OP $JOIN $PART $QUIT $RELEVEL"
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           PERFORM SEND-LINE.

       HANDLE-SOURCE.
           PERFORM BEGIN-REPLY.
           STRING "HTTPS://GITHUB.COM/HEDDWCH/WOPO"
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           PERFORM SEND-LINE.

       HANDLE-LEVEL.
           IF PARAM(2) IS NOT EQUAL TO SPACES THEN
               MOVE PARAM(2) TO USER-NAME
           ELSE
               MOVE NICK TO USER-NAME.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           PERFORM BEGIN-REPLY
           STRING USER-RECORD
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           PERFORM SEND-LINE.
 
       HANDLE-OP.
           MOVE TARGET TO REG(1).
           MOVE PARAM(2) TO REG(2).
           IF REG(2) IS EQUAL TO SPACES THEN
               MOVE NICK TO REG(2).
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 50 THEN
               INITIALIZE MSG-BODY
               MOVE 1 TO MSG-LENGTH
               STRING "MODE " DELIMITED BY SIZE,
                      REG(1) DELIMITED BY SPACES
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               ADD 1 TO MSG-LENGTH
               STRING "+O " DELIMITED BY SIZE,
                      REG(2) DELIMITED BY SPACES
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               PERFORM SEND-LINE.

       HANDLE-JOIN.
           MOVE PARAM(2) TO REG(1).
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 80 THEN
               INITIALIZE MSG-BODY
               MOVE 1 TO MSG-LENGTH
               STRING "JOIN ", REG(1)
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               PERFORM SEND-LINE.

       HANDLE-PART.
           MOVE PARAM(2) TO REG(1).
           IF REG(1) EQUALS SPACES THEN
               MOVE TARGET TO REG(1).
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 80 THEN
               INITIALIZE MSG-BODY
               MOVE 1 TO MSG-LENGTH
               STRING "PART ", REG(1)
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               PERFORM SEND-LINE.

       HANDLE-QUIT.
           MOVE "QUIT-MESSAGE" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-VALUE.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 90 THEN
               INITIALIZE MSG-BODY
               MOVE 1 TO MSG-LENGTH
               STRING "QUIT :", CONFIG-VALUE
                      INTO MSG-BODY
                      WITH POINTER MSG-LENGTH
               PERFORM SEND-LINE
               GO TO QUIT.

       HANDLE-RELEVEL.
           MOVE PARAM(2) TO REG(1).
           MOVE PARAM(3) TO REG(2).
           PERFORM VALIDATE-USER.
           MOVE REG(1) TO USER-NAME.
           IF USER-LEVEL IS EQUAL TO 99 THEN
               MOVE REG(2) TO USER-LEVEL
               REWRITE USER-RECORD
	           INVALID KEY WRITE USER-RECORD.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           INITIALIZE MSG-BODY.
           MOVE 1 TO MSG-LENGTH.
           STRING USER-RECORD
                  INTO MSG-BODY
                  WITH POINTER MSG-LENGTH.
           PERFORM SEND-LINE.

       QUIT.
           CALL "CHANNEL-CLOSE".
           CLOSE CONFIG.
           CLOSE USERS.
           STOP RUN.
 
