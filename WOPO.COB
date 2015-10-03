       IDENTIFICATION DIVISION.
       PROGRAM-ID. "WOPO".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONFIG
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CONFIG-KEY.
           SELECT USERS
               ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS USER-NAME.
           SELECT CHANNELS
               ASSIGN TO DISK
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
       01 ASCII-MSG.
           03 MSG-BODY PIC X(999).
           03 ASCII-TABLE.
               05 ASCII-CELL PIC 999 OCCURS 999 TIMES.
       01 WOPO.
           03 WOPO-NICK PIC X(16).
       01 IRC-MESSAGE.
           03 PREFIX.
               05 NICK PIC X(16).
               05 IDENT PIC X(16).
               05 HOST PIC X(64).
           03 COMMAND PIC X(16).
               88 KICK VALUE "KICK".
               88 PING VALUE "PING".
               88 PRIVMSG VALUE "PRIVMSG".
               88 NOTICE VALUE "NOTICE".
           03 PARAMETERS.
               05 TARGET PIC X(50).
               05 REST PIC X(480).
       01 WAITING-COMMAND PIC X(16).

       01 PARAMS.
           03 WORK PIC X(480).
           03 WORK-PREFIX REDEFINES WORK PIC XX.
               88 IS-COMMAND VALUE "$$".
           03 WORK-PTR PIC 999 USAGE COMPUTATIONAL.
           03 PARAM PIC X(480) OCCURS 5 TIMES.
           03 REG PIC X(480) OCCURS 5 TIMES.

       01 BF-I-O.
           03 BF-INPUT PIC X(999)
               VALUE "$NUL$".
           03 BF-CODE PIC X(999)
               VALUE ",(.,).$$".
           03 BF-OUTPUT PIC X(999)
               VALUE SPACES.
           03 CYCLE-LIMIT PIC 9(5)
               VALUE 0.

       01 BF-STATE.
           03 MAYBE-CYCLE-LIMIT PIC 9(5)
               VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "CONFIGURATION FOLLOWS.".
           CALL "PRINT-CONFIG".
           OPEN INPUT CONFIG.
           MOVE "SERVER" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING
               CONFIG-VALUE, DELIMITED BY SPACE,
               "$NUL$"
               INTO MSG-BODY,
           CALL "ENCODE-STRING" USING ASCII-MSG.
           CALL "CHANNEL-OPEN" USING ASCII-TABLE, STATE.
           IF NOT SUCCESS THEN DISPLAY MSG-BODY
                               GO TO DIE.
           MOVE "PASS" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-VALUE.
           IF CONFIG-VALUE IS NOT EQUAL TO SPACES THEN
               STRING "PASS " DELIMITED BY SIZE,
                      CONFIG-VALUE DELIMITED BY SPACE,
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE.
           MOVE "NICK" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           MOVE CONFIG-VALUE TO WOPO-NICK.
           MOVE SPACES TO MSG-BODY.
           STRING "NICK " DELIMITED BY SIZE,
                  WOPO-NICK DELIMITED BY SPACES,
                  "$NUL$"
                  INTO MSG-BODY.
           PERFORM SEND-LINE.
           MOVE SPACES TO MSG-BODY.
           MOVE 1 TO WORK-PTR.
           STRING "USER " DELIMITED BY SIZE
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           MOVE "IDENT" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING CONFIG-VALUE DELIMITED BY SPACE,
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           ADD 1 TO WORK-PTR.
           MOVE "REAL-NAME" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING "BOGUS HOST $COLN$" DELIMITED BY SIZE,
                  CONFIG-VALUE DELIMITED BY "  ",
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
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
               STRING "JOIN " DELIMITED BY SIZE,
                      CHANNEL-NAME DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE.

       READ-CONFIG-ENTRY.
           READ CONFIG RECORD
               INVALID KEY DISPLAY "REQUIRED KEY UNSPECIFIED."
                           DISPLAY CONFIG-KEY
                           GO TO DIE.           

       SEND-LINE.
      D    DISPLAY "ENCODING TO ASCII. ", MSG-BODY.
           CALL "ENCODE-STRING" USING ASCII-MSG.
           CALL "CHANNEL-SEND" USING ASCII-TABLE, STATE.
           IF NOT SUCCESS THEN CALL "DECODE-STRING" USING ASCII-MSG
                               DISPLAY MSG-BODY
                               GO TO DIE.

       RECEIVE-LINE.
           CALL "CHANNEL-RECV" USING ASCII-TABLE, STATE.
      D    DISPLAY "RECEIVED LINE FROM CHANNEL".
           MOVE SPACES TO MSG-BODY.
           CALL "DECODE-STRING" USING ASCII-MSG.
           IF NOT SUCCESS THEN DISPLAY MSG-BODY
                               GO TO DIE.
           CALL "IRC-MSG" USING MSG-BODY, IRC-MESSAGE.

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
      D    DISPLAY "ENTERED VALIDATE-USER".
           MOVE NICK TO USER-NAME.
           READ USERS RECORD
                INVALID KEY MOVE 0 TO USER-LEVEL.
      D    DISPLAY "SUPPOSED USER LEVEL ", USER-LEVEL.
           IF USER-LEVEL IS GREATER THAN 0 THEN
               MOVE SPACES TO MSG-BODY
               STRING "PRIVMSG NICKSERV $COLN$ ACC " DELIMITED BY SIZE
                      NICK DELIMITED BY SPACE
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE
               MOVE "NOTICE" TO WAITING-COMMAND
               MOVE 0 TO STATE
      D        DISPLAY "WAITING FOR ACC."
               PERFORM WAIT-FOR-ACC UNTIL DONE
      D        DISPLAY "USER VALIDATED."
      D    ELSE
      D        DISPLAY "USER NOT VALIDATED."
           .

       WAIT-FOR-ACC.
           PERFORM WAIT-FOR-COMMAND.
           PERFORM GET-PARAMS.
           IF PARAM(1) EQUALS USER-NAME AND PARAM(2) EQUALS "ACC" THEN
               MOVE 99 TO STATE
               IF PARAM(3) IS NOT EQUAL TO "3" THEN
                   MOVE 0 TO USER-LEVEL
               ELSE NEXT SENTENCE
           ELSE MOVE SPACES TO COMMAND.

       MAIN.
           PERFORM RECEIVE-LINE.
           IF PING THEN
               PERFORM PONG
           ELSE IF PRIVMSG THEN
      D        DISPLAY "PROCESSING PRIVMSG"
               PERFORM HANDLE-MESSAGE
           ELSE IF NOTICE THEN
      D        DISPLAY "PROCESSING NOTICE"
               PERFORM HANDLE-MESSAGE
           ELSE IF KICK THEN
      D        DISPLAY "PROCESSING KICK"
               PERFORM HANDLE-KICK.

      *THE REPLY FUNCTIONS NEED NICK, COMMAND, AND TARGET PRESERVED.
       BEGIN-REPLY.
           MOVE SPACES TO MSG-BODY.
           MOVE 1 TO WORK-PTR.
           STRING COMMAND DELIMITED BY SPACES
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           ADD 1 TO WORK-PTR.
           IF TARGET IS EQUAL TO WOPO-NICK THEN
               STRING NICK DELIMITED BY SPACE
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
           ELSE
               STRING TARGET DELIMITED BY SPACE
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR.
           ADD 1 TO WORK-PTR.
           STRING "$COLN$"
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           IF TARGET IS NOT EQUAL TO WOPO-NICK THEN
               STRING NICK DELIMITED BY SPACES
                     ". " DELIMITED BY SIZE
                     INTO MSG-BODY
                     WITH POINTER WORK-PTR.

       REPLY-ACK.
           PERFORM BEGIN-REPLY.
           STRING "OK.$NUL$"
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       PONG.
           STRING "PONG$NUL$"
                  INTO MSG-BODY.
           PERFORM SEND-LINE.

       HANDLE-KICK.
           PERFORM GET-PARAMS.
           IF PARAM(1) IS EQUAL TO WOPO-NICK THEN
               MOVE SPACES TO MSG-BODY
               STRING "JOIN " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES
                      "$NUL$"
                     INTO MSG-BODY
               PERFORM SEND-LINE
               MOVE 1 TO WORK-PTR
               UNSTRING REST DELIMITED BY "$COLN$"
                        INTO WORK
                        WITH POINTER WORK-PTR
               UNSTRING REST
                        INTO WORK
                        WITH POINTER WORK-PTR
               IF WORK IS NOT EQUAL TO WOPO-NICK THEN
                   MOVE SPACES TO MSG-BODY
                   STRING "PRIVMSG " DELIMITED BY SIZE,
                          TARGET DELIMITED BY SPACES,
                          " $COLN$" DELIMITED BY SIZE,
                          NICK DELIMITED BY SPACES,
                          ". " DELIMITED BY SIZE,
                          WORK DELIMITED BY "$NUL$",
                          "$NUL$"
                          INTO MSG-BODY
                   PERFORM SEND-LINE.

       HANDLE-MESSAGE.
           PERFORM GET-PARAMS.
           IF IS-COMMAND THEN
               MOVE 3 TO WORK-PTR
               UNSTRING WORK INTO PARAM(1)
                             WITH POINTER WORK-PTR
      D        DISPLAY "COMMAND BODY ", PARAM(1)
               IF PARAM(1) IS EQUAL TO "HELP" THEN
                   PERFORM HANDLE-HELP
               ELSE IF PARAM(1) IS EQUAL TO "SOURCE" THEN
                   PERFORM HANDLE-SOURCE
               ELSE IF PARAM(1) IS EQUAL TO "BF-INPUT" THEN
                   PERFORM HANDLE-BF-INPUT
               ELSE IF PARAM(1) IS EQUAL TO "BF-CODE" THEN
                   PERFORM HANDLE-BF-CODE
               ELSE IF PARAM(1) IS EQUAL TO "BF-OUTPUT" THEN
                   PERFORM HANDLE-BF-OUTPUT
               ELSE IF PARAM(1) IS EQUAL TO "BF-RUN" THEN
      D            DISPLAY "BF-RUN"
                   PERFORM HANDLE-BF-RUN
               ELSE IF PARAM(1) IS EQUAL TO "SHITFED" THEN
                   PERFORM HANDLE-SHITFED
               ELSE IF PARAM(1) IS EQUAL TO "LEVEL" THEN
                   PERFORM HANDLE-LEVEL
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
           STRING "$$BF-INPUT $$BF-CODE $$BF-OUTPUT $$BF-RUN "
      -           "$$HELP $$LEVEL $$JOIN $$PART $$QUIT $$RELEVEL "
      -           "$$SHITFED $$SOURCE"
      -           "$NUL$"
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       HANDLE-SHITFED.
           PERFORM BEGIN-REPLY.
           STRING "$002$LEAVE MY CASE ALONE, ASSHOL$LOWE$."
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       HANDLE-SOURCE.
           PERFORM BEGIN-REPLY.
           STRING "HTTPS$COLN$//GITHUB.COM/HEDDWCH/WOPO"
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
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
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.
 
       HANDLE-JOIN.
           MOVE PARAM(2) TO REG(1).
      *    PRESERVE VARIABLES FOR REPLY
           MOVE NICK TO REG(2).
           MOVE COMMAND TO REG(3).
           MOVE TARGET TO REG(4).
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 80 AND
              REG(1) IS NOT EQUAL TO "0" THEN
               MOVE SPACES TO MSG-BODY
               STRING "JOIN ", DELIMITED BY SIZE,
                      REG(1), DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE
      *        RESTORE NICK AND TARGET, THEN REPLY.
               MOVE REG(2) TO NICK
               MOVE REG(3) TO COMMAND
               MOVE REG(4) TO TARGET
               PERFORM REPLY-ACK.

       HANDLE-PART.
           MOVE PARAM(2) TO REG(1).
      *    PRESERVE VARIABLES FOR REPLY
           MOVE NICK TO REG(2).
           MOVE COMMAND TO REG(3).
           MOVE TARGET TO REG(4).
           IF REG(1) EQUALS SPACES THEN
               MOVE TARGET TO REG(1).
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 80 THEN
      *        RESTORE NICK AND TARGET, THEN REPLY.
               MOVE REG(2) TO NICK
               MOVE REG(3) TO COMMAND
               MOVE REG(4) TO TARGET
               PERFORM REPLY-ACK
               MOVE SPACES TO MSG-BODY
               STRING "PART " DELIMITED BY SIZE,
                      REG(1) DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE.

       HANDLE-QUIT.
      *    PRESERVE VARIABLES FOR REPLY
           MOVE NICK TO REG(1).
           MOVE COMMAND TO REG(2).
           MOVE TARGET TO REG(3).
           MOVE "QUIT-MESSAGE" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-VALUE.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS GREATER THAN 90 THEN
      *        RESTORE NICK AND TARGET, THEN REPLY.
               MOVE REG(1) TO NICK
               MOVE REG(2) TO COMMAND
               MOVE REG(3) TO TARGET
               PERFORM REPLY-ACK
               MOVE SPACES TO MSG-BODY
               STRING "QUIT $COLN$" DELIMITED BY SIZE,
                      CONFIG-VALUE,
                      INTO MSG-BODY
               PERFORM SEND-LINE
               GO TO QUIT.

       HANDLE-RELEVEL.
           MOVE PARAM(2) TO REG(1).
           MOVE PARAM(3) TO REG(2).
      *    PRESERVE VARIABLES FOR REPLY
           MOVE NICK TO REG(3).
           MOVE COMMAND TO REG(4).
           MOVE TARGET TO REG(5).
           PERFORM VALIDATE-USER.
           MOVE REG(1) TO USER-NAME.
           IF USER-LEVEL IS EQUAL TO 99 THEN
               MOVE REG(2) TO USER-LEVEL
               REWRITE USER-RECORD
                   INVALID KEY WRITE USER-RECORD.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
      *    RESTORE NICK AND TARGET, THEN REPLY.
           MOVE REG(3) TO NICK.
           MOVE REG(4) TO COMMAND.
           MOVE REG(5) TO TARGET.
           PERFORM BEGIN-REPLY.
           STRING USER-RECORD
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       HANDLE-BF-CODE.
           IF PARAM(2) IS EQUAL TO SPACES THEN
               PERFORM BEGIN-REPLY
               STRING "CODE. " DELIMITED BY SIZE,
                      BF-CODE DELIMITED BY "$$",
                      "$NUL$" DELIMITED BY SIZE
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
      D        DISPLAY "BF-CODE. ", BF-CODE
               PERFORM SEND-LINE
           ELSE
               MOVE PARAM(2) TO REG(1)
      *        PRESERVE VARIABLES FOR REPLY.
               MOVE NICK TO REG(2)
               MOVE COMMAND TO REG(3)
               MOVE TARGET TO REG(4)
               PERFORM VALIDATE-USER
               IF USER-LEVEL > 60 THEN
      *            RESTORE NICK AND TARGET, THEN REPLY.
                   MOVE REG(2) TO NICK
                   MOVE REG(3) TO COMMAND
                   MOVE REG(4) TO TARGET
                   MOVE SPACES TO BF-CODE
                   MOVE REG(1) TO BF-CODE
                   PERFORM REPLY-ACK.
               
       HANDLE-BF-INPUT.
           IF PARAM(2) IS EQUAL TO SPACES THEN
               PERFORM BEGIN-REPLY
               STRING "INPUT. ",
                      BF-INPUT
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
      D        DISPLAY "INPUT. ", BF-INPUT
      D        DISPLAY "MSG-BODY. ", MSG-BODY
               PERFORM SEND-LINE
           ELSE
               MOVE 1 TO WORK-PTR
               UNSTRING REST DELIMITED BY SPACE
                             INTO PARAM(1),
                             WITH POINTER WORK-PTR
               UNSTRING REST
                        INTO REG(1)
                        WITH POINTER WORK-PTR
      *        PRESERVE VARIABLES FOR REPLY.
               MOVE NICK TO REG(2)
               MOVE COMMAND TO REG(3)
               MOVE TARGET TO REG(4)
               PERFORM VALIDATE-USER
               IF USER-LEVEL > 50 THEN
      *            RESTORE NICK AND TARGET, THEN REPLY
                   MOVE REG(2) TO NICK
                   MOVE REG(3) TO COMMAND
                   MOVE REG(4) TO TARGET
                   MOVE REG(1) TO BF-INPUT
                   PERFORM REPLY-ACK.

       HANDLE-BF-OUTPUT.
           PERFORM BEGIN-REPLY.
      D    DISPLAY "BF OUTPUT. ", BF-OUTPUT.
      D    DISPLAY "STRINGING BF OUTPUT".
           STRING "OUTPUT. " DELIMITED BY SIZE,
                  BF-OUTPUT DELIMITED BY "$NUL$"
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
      D    DISPLAY "SENDING LINE".
           PERFORM SEND-LINE.
      D    DISPLAY "SENT LINE".

       HANDLE-BF-RUN.
      D    DISPLAY "HANDLING BF-RUN".
           MOVE PARAM(2) TO REG(1).
      *    PRESERVE VARIABLES FOR REPLY
           MOVE NICK TO REG(2).
           MOVE COMMAND TO REG(3).
           MOVE TARGET TO REG(4).
           PERFORM VALIDATE-USER.
           IF REG(1) IS EQUAL TO SPACES THEN
               MOVE 999 TO MAYBE-CYCLE-LIMIT
           ELSE
               UNSTRING REG(1) INTO MAYBE-CYCLE-LIMIT.
      D    DISPLAY "CYCLE LIMIT. ", CYCLE-LIMIT.
      *    RESTORE NICK AND TARGET, THEN REPLY.
           MOVE REG(2) TO NICK
           MOVE REG(3) TO COMMAND
           MOVE REG(4) TO TARGET
           IF USER-LEVEL > 50 THEN
               PERFORM REPLY-ACK
               MOVE MAYBE-CYCLE-LIMIT TO CYCLE-LIMIT
               PERFORM BF-LIMIT-CYCLES
               CALL "BF-RUN" USING BF-I-O
      D        DISPLAY "BF RAN"
               PERFORM HANDLE-BF-OUTPUT.

       BF-LIMIT-CYCLES.
           IF CYCLE-LIMIT > 999 THEN
             IF USER-LEVEL < 90 THEN
                IF USER-LEVEL < 70 THEN
                    MOVE 999 TO CYCLE-LIMIT
                ELSE IF CYCLE-LIMIT > 9999 THEN
                    MOVE 9999 TO CYCLE-LIMIT.

       QUIT.
           CALL "CHANNEL-CLOSE".
           CLOSE CONFIG.
           CLOSE USERS.
           STOP RUN.
