       IDENTIFICATION DIVISION.
       PROGRAM-ID. "WOPO".

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           SWITCH-1 IS SHOW-ESCAPES
               ON STATUS IS SHOULD-SHOW-ESCAPES.

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
      *CONFIGURATION "CONSTANTS"
       01 PLATFORM PIC X(16) VALUE "UNIX".
       01 STATE PIC 9(2).
           88 SUCCESS VALUE 0.
           88 DONE VALUE 99.
       01 ASCII-MSG.
           03 MSG-BODY PIC X(999).
           03 ASCII-TABLE.
               05 ASCII-CELL PIC 999 OCCURS 999 TIMES.
       01 WOPO.
           03 WOPO-NICK PIC X(16).
           03 WORK PIC X(999).
           03 WORK-PREFIX REDEFINES WORK PIC XX.
               88 IS-COMMAND VALUE "$$".
           03 WORK-CTCP REDEFINES WORK PIC X(5).
               88 IS-CTCP VALUE "$SOH$".
           03 WORK-PTR PIC 999 USAGE COMPUTATIONAL.
           03 PARAM PIC X(480) OCCURS 5 TIMES.
           03 WOPO-PARAMS PIC 9.
           03 WOPO-COUNTER PIC 9.
           03 MSG-BODY-TMP PIC X(999).

       01 USERS-HEADER.
           03 FILLER PIC X(16) VALUE "USER NAME.".
           03 FILLER PIC X(6) VALUE "LEVEL.".

       01 IRC-PARAMETERS.
           03 NUM-PARAMS PIC 99.
           03 PREFIX.
               05 MSG-SRC PIC 999.
                   88 GOT-PREFIX VALUES 1 THROUGH 999.
               05 IDENT PIC 999.
               05 HOST PIC 999.
           03 COMMAND PIC 999.
           03 PARAMS.
               05 PARAM-PTR PIC 999 OCCURS 15 TIMES.

       01 IRC-STATE.
           03 NICK PIC X(16).
           03 COMMAND PIC X(16).
               88 KICK VALUE "KICK".
               88 PING VALUE "PING".
               88 PRIVMSG VALUE "PRIVMSG".
               88 NOTICE VALUE "NOTICE".
           03 TARGET PIC X(50).
           03 WAITING-COMMAND PIC X(16).

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

       01 FORMATTED-TIME.
           03 FILLER PIC X VALUE "H".
           03 HOURS-DIGITS PIC 99.
           03 FILLER PIC X VALUE "M".
           03 MINUTES-DIGITS PIC 99.
           03 FILLER PIC X VALUE "S".
           03 SECONDS-DIGITS PIC 99.
           03 FILLER PIC X VALUE ".".
           03 TENTH-SECONDS PIC 99.

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
           PERFORM GET-IRC-STATE.
           PERFORM GET-PARAMS.

       GET-IRC-STATE.
           CALL "PARSE-IRC-MSG" USING MSG-BODY, IRC-PARAMETERS.
           IF GOT-PREFIX THEN
               MOVE MSG-SRC TO WORK-PTR
               UNSTRING MSG-BODY
                        DELIMITED BY "$EXC$" OR "$AT$" OR SPACES
                        INTO NICK
                        WITH POINTER WORK-PTR.
           MOVE COMMAND OF IRC-PARAMETERS TO WORK-PTR.
           UNSTRING MSG-BODY
                    DELIMITED BY SPACES
                    INTO COMMAND OF IRC-STATE
                    WITH POINTER WORK-PTR.
           IF NUM-PARAMS IS NOT LESS THAN 1 THEN
               MOVE PARAM-PTR(1) TO WORK-PTR
               UNSTRING MSG-BODY
                        DELIMITED BY SPACES
                        INTO TARGET
                        WITH POINTER WORK-PTR
           ELSE
               MOVE SPACES TO TARGET.

       WAIT-FOR-COMMAND.
           PERFORM RECEIVE-LINE UNTIL
                   COMMAND OF IRC-STATE EQUALS WAITING-COMMAND.

       GET-PARAMS.
           MOVE PARAM-PTR(NUM-PARAMS) TO WORK-PTR.
           UNSTRING MSG-BODY DELIMITED BY "$NUL$"
                    INTO WORK
                    WITH POINTER WORK-PTR.
           MOVE 1 TO WORK-PTR.
           PERFORM BLANK-PARAM VARYING WORK-PTR FROM 1, BY 1,
                               UNTIL WORK-PTR IS NOT LESS THAN 5.
           MOVE 0 TO WOPO-PARAMS, STATE.
           MOVE 1 TO WORK-PTR.
           PERFORM UNSTRING-PARAM UNTIL DONE.

       UNSTRING-PARAM.
           ADD 1 TO WOPO-PARAMS.
           UNSTRING WORK DELIMITED BY SPACE
                    INTO PARAM(WOPO-PARAMS)
                    WITH POINTER WORK-PTR.
           IF PARAM(WOPO-PARAMS) IS EQUAL TO SPACES THEN
               SUBTRACT 1 FROM WOPO-PARAMS
               MOVE 99 TO STATE.
           IF WOPO-PARAMS IS NOT LESS THAN 5 THEN
               MOVE 99 TO STATE.

       BLANK-PARAM.
           MOVE SPACES TO PARAM(WORK-PTR).

       VALIDATE-USER.
      D    DISPLAY "ENTERED VALIDATE-USER".
      *    SAVE MESSAGE.
           MOVE MSG-BODY TO MSG-BODY-TMP.
           MOVE NICK TO USER-NAME.
           READ USERS RECORD
                INVALID KEY MOVE 0 TO USER-LEVEL.
      D    DISPLAY "SUPPOSED USER LEVEL ", USER-LEVEL.
           IF USER-LEVEL IS GREATER THAN 0 THEN
               MOVE SPACES TO MSG-BODY
               STRING "PRIVMSG NICKSERV $COLN$ACC " DELIMITED BY SIZE
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
      *        RESTORE AND REPARSE MESSAGE.
               MOVE MSG-BODY-TMP TO MSG-BODY.
               PERFORM GET-IRC-STATE.
               PERFORM GET-PARAMS.

       WAIT-FOR-ACC.
           PERFORM WAIT-FOR-COMMAND.
           IF PARAM(1) EQUALS USER-NAME AND PARAM(2) EQUALS "ACC" THEN
               MOVE 99 TO STATE
               IF PARAM(3) IS NOT EQUAL TO "3" THEN
                   MOVE 0 TO USER-LEVEL
               ELSE NEXT SENTENCE
           ELSE MOVE SPACES TO COMMAND OF IRC-STATE.

       MAIN.
           PERFORM RECEIVE-LINE.
      D    DISPLAY "NICK. ", NICK,
      D            "COMMAND. ", COMMAND OF IRC-STATE,
      D            "TARGET. ", TARGET.
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
           STRING COMMAND OF IRC-STATE DELIMITED BY SPACES
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
      D    DISPLAY "DETECTED KICK.".
           MOVE SPACES TO WORK.
           MOVE PARAM-PTR(2) TO WORK-PTR.
           UNSTRING MSG-BODY DELIMITED BY SPACE
                    INTO WORK
                    WITH POINTER WORK-PTR.
           IF WORK IS EQUAL TO WOPO-NICK THEN
      D        DISPLAY "KICK WAS ME."
               MOVE MSG-BODY TO MSG-BODY-TMP
               MOVE SPACES TO MSG-BODY
               STRING "JOIN " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES
                      "$NUL$"
                     INTO MSG-BODY
               PERFORM SEND-LINE
               MOVE PARAM-PTR(NUM-PARAMS) TO WORK-PTR
               UNSTRING MSG-BODY-TMP
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
           IF IS-CTCP THEN
               PERFORM HANDLE-CTCP
           ELSE IF IS-COMMAND THEN
               MOVE 3 TO WORK-PTR
               UNSTRING PARAM(1) INTO WORK
                        WITH POINTER WORK-PTR
               MOVE WORK TO PARAM(1)
      D        DISPLAY "COMMAND BODY ", PARAM(1)
               IF PARAM(1) IS EQUAL TO "HELP" THEN
                   PERFORM HANDLE-HELP
               ELSE IF PARAM(1) IS EQUAL TO "SOURCE" THEN
                   PERFORM HANDLE-SOURCE
               ELSE IF PARAM(1) IS EQUAL TO "STRESS" THEN
                   PERFORM HANDLE-STRESS
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
               ELSE IF PARAM(1) IS EQUAL TO "LIST-USERS" THEN
                   PERFORM HANDLE-LIST-USERS
               ELSE IF PARAM(1) IS EQUAL TO "OP" THEN
                   PERFORM HANDLE-OP
               ELSE IF PARAM(1) IS EQUAL TO "DEOP" THEN
                   PERFORM HANDLE-DEOP
               ELSE IF PARAM(1) IS EQUAL TO "JOIN" THEN
                   PERFORM HANDLE-JOIN
               ELSE IF PARAM(1) IS EQUAL TO "PART" THEN
                   PERFORM HANDLE-PART
               ELSE IF PARAM(1) IS EQUAL TO "QUIT" THEN
                   PERFORM HANDLE-QUIT
               ELSE IF PARAM(1) IS EQUAL TO "SHOW-ESCAPES" THEN
                   PERFORM HANDLE-SHOW-ESCAPES
               ELSE IF PARAM(1) IS EQUAL TO "RELEVEL" THEN
                   PERFORM HANDLE-RELEVEL.

       HANDLE-HELP.
           PERFORM BEGIN-REPLY.
           STRING "$$BF-INPUT $$BF-CODE $$BF-OUTPUT $$BF-RUN "
      -           "$$HELP $$LEVEL $$LIST-USERS $$JOIN $$PART "
      -           "$$SHOW-ESCAPES $$RELEVEL $$SOURCE $$STRESS"
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

       HANDLE-STRESS.
           MOVE 1 TO WORK-PTR.
           STRING "PRIVMSG " DELIMITED BY SIZE,
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           IF TARGET IS EQUAL TO WOPO-NICK THEN
               STRING NICK
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
           ELSE
               STRING TARGET
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR.
           STRING " $COLN$$SOH$ACTION PUNCHES A BABY.$SOH$$NUL$"
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
           PERFORM BEGIN-REPLY.
           MOVE 0 TO WOPO-COUNTER.
           IF SHOULD-SHOW-ESCAPES THEN
      D        DISPLAY "USER NAME. ", USER-NAME
               MOVE USER-NAME TO WORK
               CALL "RE-ESCAPE" USING WORK, WOPO-COUNTER
               MOVE WORK TO USER-NAME
      D        DISPLAY "USER NAME. ", USER-NAME
               .
           STRING USER-NAME
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           ADD WOPO-COUNTER TO WORK-PTR.
           STRING USER-LEVEL
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       HANDLE-LIST-USERS.
           CLOSE USERS.
           OPEN INPUT USERS.
           MOVE 0 TO STATE.
           PERFORM BEGIN-REPLY.
           STRING USERS-HEADER INTO MSG-BODY WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.
           PERFORM LIST-USER-RECORD UNTIL DONE.
           CLOSE USERS.
           OPEN I-O USERS.

       LIST-USER-RECORD.
           READ USERS NEXT RECORD, AT END MOVE 99 TO STATE.
           MOVE 0 TO WOPO-COUNTER.
           IF SHOULD-SHOW-ESCAPES THEN
      D        DISPLAY "USER NAME. ", USER-NAME
               MOVE USER-NAME TO WORK
               CALL "RE-ESCAPE" USING WORK, WOPO-COUNTER
               MOVE WORK TO USER-NAME
      D        DISPLAY "USER NAME. ", USER-NAME
               .
           IF NOT DONE THEN
               PERFORM BEGIN-REPLY
               STRING USER-NAME
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
               ADD WOPO-COUNTER TO WORK-PTR
               STRING USER-LEVEL
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
               PERFORM SEND-LINE.

           
 
       HANDLE-JOIN.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 80 AND
              PARAM(2) IS NOT EQUAL TO "0" THEN
               MOVE SPACES TO MSG-BODY
               STRING "JOIN ", DELIMITED BY SIZE,
                      PARAM(2), DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE
               PERFORM REPLY-ACK.

       HANDLE-PART.
           IF WOPO-PARAMS IS LESS THAN 2 THEN
               MOVE TARGET TO WORK
           ELSE
               MOVE PARAM(2) TO WORK
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 80 THEN
               PERFORM REPLY-ACK
               MOVE SPACES TO MSG-BODY
               STRING "PART " DELIMITED BY SIZE,
                      WORK DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY
               PERFORM SEND-LINE.

       STRING-LOWOS.
           STRING "$LOWO$" INTO MSG-BODY WITH POINTER WORK-PTR.

       STRING-PARAMS.
           STRING PARAM(WOPO-COUNTER) DELIMITED BY SPACES
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           ADD 1 TO WORK-PTR.

       HANDLE-OP.
           PERFORM VALIDATE-USER.
           IF WOPO-PARAMS IS LESS THAN 2 THEN
               MOVE NICK TO PARAM(2)
               MOVE 2 TO WOPO-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 70 THEN
               PERFORM REPLY-ACK
               MOVE SPACES TO MSG-BODY
               MOVE 1 TO WORK-PTR
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES,
                      " +" DELIMITED BY SIZE
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
               PERFORM STRING-LOWOS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN WOPO-PARAMS
               ADD 1 TO WORK-PTR
               PERFORM STRING-PARAMS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN WOPO-PARAMS
               STRING "$NUL$"
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
               PERFORM SEND-LINE.

       HANDLE-DEOP.
           PERFORM VALIDATE-USER.
           IF WOPO-PARAMS IS LESS THAN 2 THEN
               MOVE NICK TO PARAM(2)
               MOVE 2 TO WOPO-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 70 THEN
               PERFORM REPLY-ACK
               MOVE SPACES TO MSG-BODY
               MOVE 1 TO WORK-PTR
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES,
                      " -" DELIMITED BY SIZE
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
               PERFORM STRING-LOWOS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN WOPO-PARAMS
               ADD 1 TO WORK-PTR
               PERFORM STRING-PARAMS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN WOPO-PARAMS
               STRING "$NUL$"
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
               PERFORM SEND-LINE.

       HANDLE-QUIT.
           MOVE "QUIT-MESSAGE" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-VALUE.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 90 THEN
               PERFORM REPLY-ACK
               MOVE SPACES TO MSG-BODY
               STRING "QUIT $COLN$" DELIMITED BY SIZE,
                      CONFIG-VALUE,
                      INTO MSG-BODY
               PERFORM SEND-LINE
               GO TO QUIT.

       HANDLE-SHOW-ESCAPES.
           PERFORM VALIDATE-USER.
           PERFORM BEGIN-REPLY.
           STRING "SHOW-ESCAPES " INTO MSG-BODY WITH POINTER WORK-PTR.
           IF USER-LEVEL IS NOT LESS THAN 90 THEN
               IF PARAM(2) IS NOT EQUAL TO SPACES THEN
                   IF PARAM(2) IS EQUAL TO "ON" THEN
                       SET SHOW-ESCAPES TO ON
                   ELSE IF PARAM(2) IS EQUAL TO "OFF" THEN
                       SET SHOW-ESCAPES TO OFF
                   ELSE NEXT SENTENCE
               ELSE IF SHOULD-SHOW-ESCAPES THEN
                   SET SHOW-ESCAPES TO OFF
               ELSE SET SHOW-ESCAPES TO ON.
           IF SHOULD-SHOW-ESCAPES THEN
               STRING "ON." INTO MSG-BODY WITH POINTER WORK-PTR
           ELSE STRING "OFF." INTO MSG-BODY WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       HANDLE-RELEVEL.
           PERFORM VALIDATE-USER.
           MOVE PARAM(2) TO USER-NAME.
           IF USER-LEVEL IS NOT LESS THAN 99 THEN
               MOVE PARAM(3) TO USER-LEVEL
               REWRITE USER-RECORD
                   INVALID KEY WRITE USER-RECORD.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           PERFORM BEGIN-REPLY.
           STRING USER-RECORD
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
           PERFORM SEND-LINE.

       HANDLE-BF-CODE.
           IF WOPO-PARAMS IS LESS THAN 2 THEN
               PERFORM BEGIN-REPLY
               STRING "CODE. " DELIMITED BY SIZE,
                      BF-CODE DELIMITED BY "$$",
                      "$NUL$" DELIMITED BY SIZE
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
      D        DISPLAY "BF-CODE. ", BF-CODE
               PERFORM SEND-LINE
           ELSE
               PERFORM VALIDATE-USER
               IF USER-LEVEL IS NOT LESS THAN 60 THEN
                   MOVE SPACES TO BF-CODE
                   MOVE PARAM(2) TO BF-CODE
                   PERFORM REPLY-ACK.
               
       HANDLE-BF-INPUT.
           IF WOPO-PARAMS IS LESS THAN 2 THEN
               PERFORM BEGIN-REPLY
               STRING "INPUT. ",
                      BF-INPUT
                      INTO MSG-BODY
                      WITH POINTER WORK-PTR
      D        DISPLAY "INPUT. ", BF-INPUT
      D        DISPLAY "MSG-BODY. ", MSG-BODY
               PERFORM SEND-LINE
           ELSE
               PERFORM VALIDATE-USER
               MOVE PARAM-PTR(NUM-PARAMS) TO WORK-PTR
               UNSTRING MSG-BODY DELIMITED BY SPACE
                                 INTO WORK,
                                 WITH POINTER WORK-PTR
               UNSTRING MSG-BODY
                        INTO WORK
                        WITH POINTER WORK-PTR
               IF USER-LEVEL IS NOT LESS THAN 50 THEN
                   MOVE WORK TO BF-INPUT
                   PERFORM REPLY-ACK.

       HANDLE-BF-OUTPUT.
           PERFORM BEGIN-REPLY.
      D    DISPLAY "BF OUTPUT. ", BF-OUTPUT.
      D    DISPLAY "STRINGING BF OUTPUT".
           IF SHOULD-SHOW-ESCAPES THEN
               CALL "RE-ESCAPE" USING BF-OUTPUT, WOPO-COUNTER.
           STRING "OUTPUT. " DELIMITED BY SIZE,
                  BF-OUTPUT DELIMITED BY "$NUL$"
                  INTO MSG-BODY
                  WITH POINTER WORK-PTR.
      D    DISPLAY "SENDING LINE".
           PERFORM SEND-LINE.
      D    DISPLAY "SENT LINE".

       HANDLE-BF-RUN.
      D    DISPLAY "HANDLING BF-RUN".
           PERFORM VALIDATE-USER.
           IF WOPO-PARAMS IS LESS THAN 2 THEN
               MOVE 999 TO MAYBE-CYCLE-LIMIT
           ELSE
               UNSTRING PARAM(2) INTO MAYBE-CYCLE-LIMIT.
      D    DISPLAY "CYCLE LIMIT. ", CYCLE-LIMIT.
           IF USER-LEVEL IS NOT LESS THAN 50 THEN
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

       HANDLE-CTCP.
      D    DISPLAY "HANDLING CTCP.".
           IF NOTICE AND TARGET IS NOT EQUAL TO WOPO-NICK THEN
               NEXT SENTENCE
           ELSE
               MOVE 6 TO WORK-PTR
               UNSTRING PARAM(1) DELIMITED BY SPACE OR "$SOH$"
                        INTO WORK
                        WITH POINTER WORK-PTR
      D        DISPLAY "CTCP PARAM. ", WORK
               MOVE WORK TO PARAM(1)
               IF PARAM(1) IS EQUAL TO "PING" THEN
                   PERFORM HANDLE-PING
               ELSE IF PARAM(1) IS EQUAL TO "VERSION" THEN
                   PERFORM HANDLE-VERSION
      *        ELSE IF PARAM(1) IS EQUAL TO "TIME" THEN
      *            PERFORM HANDLE-TIME
               ELSE NEXT SENTENCE.

       HANDLE-PING.
           MOVE PARAM-PTR(NUM-PARAMS) TO WORK-PTR.
           ADD 5 TO WORK-PTR
           UNSTRING MSG-BODY DELIMITED BY "$NUL$"
                    INTO WORK
                    WITH POINTER WORK-PTR.
           STRING "NOTICE " DELIMITED BY SIZE,
                  NICK DELIMITED BY SPACES,
                  " $COLN$$SOH$" DELIMITED BY SIZE,
                  WORK DELIMITED BY "$SOH$",
                  "$SOH$$NUL$" DELIMITED BY SIZE
                  INTO MSG-BODY.
      D    DISPLAY MSG-BODY.
           PERFORM SEND-LINE.

       HANDLE-VERSION.
      D    DISPLAY "HANDLING VERSION."
           STRING "NOTICE " DELIMITED BY SIZE,
                  NICK DELIMITED BY SPACES,
                  " $COLN$$SOH$VERSION WOPO THE COBOL-74 BOT. "
      -           "VERSION WHATEVER. RUNNING ON " DELIMITED BY SIZE
                  PLATFORM DELIMITED BY SPACES
                  ".$SOH$$NUL$" DELIMITED BY SIZE
                  INTO MSG-BODY.
           PERFORM SEND-LINE.

      *HANDLE-TIME.
      *    MOVE TIME TO FORMATTED-TIME.
      *    STRING "NOTICE " DELIMITED BY SIZE,
      *           NICK DELIMITED BY SPACES,
      *           " $COLN$$SOH$TIME" DELIMITED BY SIZE,
      *           FORMATTED-TIME DELIMITED BY SIZE,
      *           "$SOH$"
      *           INTO MSG-BODY.
      *    PERFORM SEND-LINE.

       QUIT.
           CALL "CHANNEL-CLOSE".
           CLOSE CONFIG.
           CLOSE USERS.
           STOP RUN.
