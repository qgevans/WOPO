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
           03 USER-NAME PIC X(40).
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

       01 IRC-I-O.
           03 INPUT-BUFFER.
               05 MSG-BODY PIC X(999).
               05 ASCII-TABLE.
                   07 ASCII-CELL PIC 999 OCCURS 999 TIMES.
           03 OUTPUT-BUFFER.
               05 MSG-BODY PIC X(999).
               05 ASCII-TABLE.
                   07 ASCII-CELL PIC 999 OCCURS 999 TIMES.

       01 WOPO.
           03 WOPO-NICK PIC X(40).
           03 REGISTER-FILE.
               05 REGISTER OCCURS 8 TIMES.
                   07 R PIC X(999).
                   07 R-COMMAND REDEFINES R PIC XX.
                       88 IS-COMMAND VALUE "$$".
                   07 R-CTCP REDEFINES R PIC X(5).
                       88 IS-CTCP VALUE "$SOH$".
                   07 PTR PIC 999.
               05 SRC PIC 9.
               05 DEST PIC 9.
           03 PARAM PIC X(480) OCCURS 5 TIMES.
           03 NUM-PARAMS PIC 9.
           03 WOPO-COUNTER PIC 9.
           03 MSG-BODY-TMP PIC X(999).
      D    03 DEBUG-PTR PIC 9.

       01 USERS-HEADER.
           03 FILLER PIC X(40) VALUE "USER NAME.".
           03 FILLER PIC X(6) VALUE "LEVEL.".

       01 IRC-PARAMS.
           03 NUM-PARAMS PIC 99.
           03 PREFIX.
               05 MSG-SRC PIC 999.
                   88 GOT-PREFIX VALUES 1 THROUGH 999.
               05 IDENT PIC 999.
               05 HOST PIC 999.
           03 COMMAND PIC 999.
           03 PARAM PIC 999 OCCURS 15 TIMES.

       01 IRC-STATE.
           03 NICK PIC X(40).
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
               VALUE "++++++++++(>++++++(>++++<-)<-)>>.<<+++++(>++++(>--
      -              "--<-)<-)>>-.<+++(>---<-)>.-.$$".
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
               INTO MSG-BODY OF OUTPUT-BUFFER,
           CALL "ENCODE-STRING" USING OUTPUT-BUFFER.
           CALL "CHANNEL-OPEN" USING ASCII-TABLE OF OUTPUT-BUFFER,
                                     STATE.
           IF NOT SUCCESS THEN DISPLAY MSG-BODY OF OUTPUT-BUFFER
                               GO TO DIE.
           MOVE "PASS" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-VALUE.
           IF CONFIG-VALUE IS NOT EQUAL TO SPACES THEN
               STRING "PASS " DELIMITED BY SIZE,
                      CONFIG-VALUE DELIMITED BY SPACE,
                      "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE.
           MOVE "NICK" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           MOVE CONFIG-VALUE TO WOPO-NICK.
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER.
           STRING "NICK " DELIMITED BY SIZE,
                  WOPO-NICK DELIMITED BY SPACES,
                  "$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER.
           PERFORM SEND-LINE.
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER.
           MOVE 1 TO PTR(1).
           STRING "USER " DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           MOVE "IDENT" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING CONFIG-VALUE DELIMITED BY SPACE,
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           ADD 1 TO PTR(1).
           MOVE "REAL-NAME" TO CONFIG-KEY.
           PERFORM READ-CONFIG-ENTRY.
           STRING "BOGUS HOST $COLN$" DELIMITED BY SIZE,
                  CONFIG-VALUE DELIMITED BY "  ",
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
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
                      INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE.

       READ-CONFIG-ENTRY.
           READ CONFIG RECORD
               INVALID KEY DISPLAY "REQUIRED KEY UNSPECIFIED."
                           DISPLAY CONFIG-KEY
                           GO TO DIE.           

       SEND-LINE.
           CALL "ENCODE-STRING" USING OUTPUT-BUFFER.
           CALL "CHANNEL-SEND" USING ASCII-TABLE OF OUTPUT-BUFFER,
                                     STATE.
           IF NOT SUCCESS THEN CALL "DECODE-STRING" USING OUTPUT-BUFFER
                               DISPLAY MSG-BODY OF OUTPUT-BUFFER
                               GO TO DIE.

       RECEIVE-LINE.
           CALL "CHANNEL-RECV" USING ASCII-TABLE OF INPUT-BUFFER,
                                     STATE.
      D    DISPLAY "RECEIVED LINE FROM CHANNEL".
           MOVE SPACES TO MSG-BODY OF INPUT-BUFFER.
           CALL "DECODE-STRING" USING INPUT-BUFFER.
           IF NOT SUCCESS THEN DISPLAY MSG-BODY OF INPUT-BUFFER
                               GO TO DIE.
           PERFORM GET-IRC-STATE.
           PERFORM GET-PARAMS.

       GET-IRC-STATE.
           CALL "PARSE-IRC-MSG" USING MSG-BODY OF INPUT-BUFFER,
                                      IRC-PARAMS.
           IF GOT-PREFIX THEN
               MOVE MSG-SRC TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        DELIMITED BY "$EXC$" OR "$AT$" OR SPACES
                        INTO NICK
                        WITH POINTER PTR(1).
           MOVE COMMAND OF IRC-PARAMS TO PTR(1).
           UNSTRING MSG-BODY OF INPUT-BUFFER
                    DELIMITED BY SPACES
                    INTO COMMAND OF IRC-STATE
                    WITH POINTER PTR(1).
           IF NUM-PARAMS OF IRC-PARAMS IS NOT LESS THAN 1 THEN
               MOVE PARAM OF IRC-PARAMS(1) TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        DELIMITED BY SPACES
                        INTO TARGET
                        WITH POINTER PTR(1)
           ELSE
               MOVE SPACES TO TARGET.

       WAIT-FOR-COMMAND.
           PERFORM RECEIVE-LINE UNTIL
                   COMMAND OF IRC-STATE EQUALS WAITING-COMMAND.

       GET-PARAMS.
           MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
	        TO PTR(1).
           UNSTRING MSG-BODY OF INPUT-BUFFER DELIMITED BY "$NUL$"
                    INTO R(1)
                    WITH POINTER PTR(1).
           MOVE 1 TO PTR(1).
           PERFORM BLANK-PARAM VARYING PTR(1) FROM 1, BY 1,
                               UNTIL PTR(1) IS NOT LESS THAN 5.
           MOVE 0 TO NUM-PARAMS OF WOPO, STATE.
           MOVE 1 TO PTR(1).
           PERFORM UNSTRING-PARAM UNTIL DONE.

       UNSTRING-PARAM.
           ADD 1 TO NUM-PARAMS OF WOPO.
           UNSTRING R(1) DELIMITED BY SPACE
                    INTO PARAM OF WOPO(NUM-PARAMS OF WOPO)
                    WITH POINTER PTR(1).
           IF PARAM OF WOPO(NUM-PARAMS OF WOPO) IS EQUAL TO SPACES THEN
               SUBTRACT 1 FROM NUM-PARAMS OF WOPO
               MOVE 99 TO STATE.
           IF NUM-PARAMS OF WOPO IS NOT LESS THAN 5 THEN
               MOVE 99 TO STATE.

       BLANK-PARAM.
           MOVE SPACES TO PARAM OF WOPO(PTR(1)).

       PRESERVE-MESSAGE.
           MOVE MSG-BODY OF INPUT-BUFFER TO MSG-BODY-TMP.

       RESTORE-MESSAGE.
           MOVE MSG-BODY-TMP TO MSG-BODY OF INPUT-BUFFER.

       VALIDATE-USER.
      D    DISPLAY "ENTERED VALIDATE-USER".
           PERFORM PRESERVE-MESSAGE.
           MOVE NICK TO USER-NAME.
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
           STRING "PRIVMSG NICKSERV $COLN$ACC " DELIMITED BY SIZE
                  NICK DELIMITED BY SPACE
                  " *$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
           PERFORM SEND-LINE
           MOVE "NOTICE" TO WAITING-COMMAND
           MOVE 0 TO STATE
      D    DISPLAY "WAITING FOR ACC."
           PERFORM WAIT-FOR-ACC UNTIL DONE
      *    RESTORE AND REPARSE MESSAGE.
           PERFORM RESTORE-MESSAGE
           PERFORM GET-IRC-STATE.
           PERFORM GET-PARAMS.

       WAIT-FOR-ACC.
           PERFORM WAIT-FOR-COMMAND.
           IF PARAM OF WOPO(1) EQUALS USER-NAME AND
	      PARAM OF WOPO(4) EQUALS "ACC" THEN
               MOVE 99 TO STATE
               IF PARAM OF WOPO(5) IS NOT EQUAL TO "3" THEN
                   MOVE 0 TO USER-LEVEL
               ELSE
                   MOVE PARAM OF WOPO(3) TO USER-NAME
                   READ USERS RECORD
                       INVALID KEY MOVE 0 TO USER-LEVEL
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
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER.
           MOVE 1 TO PTR(1).
           STRING COMMAND OF IRC-STATE DELIMITED BY SPACES
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           ADD 1 TO PTR(1).
           IF TARGET IS EQUAL TO WOPO-NICK THEN
               STRING NICK DELIMITED BY SPACE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
           ELSE
               STRING TARGET DELIMITED BY SPACE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1).
           STRING " $COLN$" DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).

       BEGIN-STANDARD-REPLY.
           PERFORM BEGIN-REPLY.
           STRING "$226$$128$$139$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           IF TARGET IS NOT EQUAL TO WOPO-NICK THEN
               STRING NICK DELIMITED BY SPACES
                     ". " DELIMITED BY SIZE
                     INTO MSG-BODY OF OUTPUT-BUFFER
                     WITH POINTER PTR(1).

       REPLY-ACK.
           PERFORM PRESERVE-MESSAGE.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "OK.$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.
           PERFORM RESTORE-MESSAGE.

       REPLY-NAK.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "ACCESS DENIED.$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       PONG.
           STRING "PONG$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER.
           PERFORM SEND-LINE.

       HANDLE-KICK.
      D    DISPLAY "DETECTED KICK.".
           MOVE SPACES TO R(1).
           MOVE PARAM OF IRC-PARAMS(2) TO PTR(1).
           UNSTRING MSG-BODY OF INPUT-BUFFER DELIMITED BY SPACE
                    INTO R(1)
                    WITH POINTER PTR(1).
           IF R(1) IS EQUAL TO WOPO-NICK THEN
      D        DISPLAY "KICK WAS ME."
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               STRING "JOIN " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES
                      "$NUL$"
                     INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE
               MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
	            TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        INTO R(1)
                        WITH POINTER PTR(1)
               IF R(1) IS NOT EQUAL TO WOPO-NICK THEN
                   MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
                   STRING "PRIVMSG " DELIMITED BY SIZE,
                          TARGET DELIMITED BY SPACES,
                          " $COLN$" DELIMITED BY SIZE,
                          NICK DELIMITED BY SPACES,
                          ". " DELIMITED BY SIZE,
                          R(1) DELIMITED BY "$NUL$",
                          "$NUL$"
                          INTO MSG-BODY OF OUTPUT-BUFFER
                   PERFORM SEND-LINE.

      DDEBUG-PARAMS.
      D    DISPLAY "PARAMETER ", DEBUG-PTR, ": ",
      D	           PARAM OF WOPO(DEBUG-PTR).

       HANDLE-MESSAGE.
      D    DISPLAY "HANDLING MESSAGE."
      D    PERFORM DEBUG-PARAMS
      D            VARYING DEBUG-PTR
      D            FROM 1, BY 1
      D            UNTIL DEBUG-PTR IS GREATER THAN NUM-PARAMS OF WOPO
           IF IS-CTCP(1) THEN
               PERFORM HANDLE-CTCP
           ELSE IF IS-COMMAND(1) THEN
               MOVE 3 TO PTR(1)
               UNSTRING PARAM OF WOPO(1) INTO R(1)
                        WITH POINTER PTR(1)
               MOVE R(1) TO PARAM OF WOPO(1)
      D        DISPLAY "COMMAND BODY ", PARAM OF WOPO(1)
               IF PARAM OF WOPO(1) IS EQUAL TO "BF-CODE" THEN
                   PERFORM HANDLE-BF-CODE
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "BF-INPUT" THEN
                   PERFORM HANDLE-BF-INPUT
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "BF-OUTPUT" THEN
                   PERFORM HANDLE-BF-OUTPUT
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "BF-RUN" THEN
      D            DISPLAY "BF-RUN"
                   PERFORM HANDLE-BF-RUN
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "DEOP" THEN
                   PERFORM HANDLE-DEOP
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "DEVOICE" THEN
                   PERFORM HANDLE-DEVOICE
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "HELP" THEN
                   PERFORM HANDLE-HELP
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "JOIN" THEN
                   PERFORM HANDLE-JOIN
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "LEVEL" THEN
                   PERFORM HANDLE-LEVEL
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "LICK" THEN
                   PERFORM HANDLE-LICK
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "LIST-USERS" THEN
                   PERFORM HANDLE-LIST-USERS
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "OP" THEN
                   PERFORM HANDLE-OP
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "PART" THEN
                   PERFORM HANDLE-PART
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "QUIT" THEN
                   PERFORM HANDLE-QUIT
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "RELEVEL" THEN
                   PERFORM HANDLE-RELEVEL
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "SHITFED" THEN
                   PERFORM HANDLE-SHITFED
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "SHOW-ESCAPES" THEN
                   PERFORM HANDLE-SHOW-ESCAPES
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "SOURCE" THEN
                   PERFORM HANDLE-SOURCE
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "STRESS" THEN
                   PERFORM HANDLE-STRESS
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "VOICE" THEN
                   PERFORM HANDLE-VOICE.

       HANDLE-HELP.
           PERFORM BEGIN-STANDARD-REPLY.
           IF PARAM OF WOPO(2) IS EQUAL TO "ME" THEN
               STRING "$240$$159$$142$$135$ "
      -                "GOD HELPS THOSE WHO HELP THEMSELVES, COMMIE. "
      -                "$240$$159$$142$$134"
                       INTO MSG-BODY OF OUTPUT-BUFFER
                       WITH POINTER PTR(1)
           ELSE
               STRING "COMMANDS$COLN$ "
      -               "$$BF-CODE $$BF-INPUT $$BF-OUTPUT $$BF-RUN "
      -               "$$DEOP $$DEVOICE $$HELP $$JOIN $$LEVEL $$LICK "
      -               "$$LIST-USERS $$OP $$PART $$RELEVEL $$SHITFED "
      -               "$$SHOW-ESCAPES $$SOURCE $$STRESS $$VOICE "
      -               "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-SHITFED.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "$002$LEAVE MY CASE ALONE, "
      -           "$226$$156$$168$ASSHOL$LOWE$$226$$156$$168$."
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-SOURCE.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "HTTPS$COLN$//GITHUB.COM/HEDDWCH/WOPO"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-STRESS.
           PERFORM BEGIN-REPLY.
           STRING "$SOH$ACTION PUNCHES A "
      -           "$226$$156$$168$BABY$226$$156$$168$.$SOH$$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-LICK.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE NICK TO R(1)
           ELSE
               MOVE PARAM OF WOPO(2) TO R(1).
           PERFORM BEGIN-REPLY.
           STRING "$SOH$ACTION VIGOROUSLY LICKS " DELIMITED BY SIZE,
                  R(1) DELIMITED BY SPACES,
                  ".$SOH$$NUL$" DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-LEVEL.
           IF PARAM OF WOPO(2) IS NOT EQUAL TO SPACES THEN
               MOVE PARAM OF WOPO(2) TO USER-NAME
           ELSE
               MOVE NICK TO USER-NAME.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           PERFORM BEGIN-STANDARD-REPLY.
           MOVE 0 TO WOPO-COUNTER.
           IF SHOULD-SHOW-ESCAPES THEN
      D        DISPLAY "USER NAME. ", USER-NAME
               MOVE USER-NAME TO R(1)
               CALL "RE-ESCAPE" USING R(1), WOPO-COUNTER
               MOVE R(1) TO USER-NAME
      D        DISPLAY "USER NAME. ", USER-NAME
               .
           STRING USER-NAME
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           ADD WOPO-COUNTER TO PTR(1).
           STRING USER-LEVEL
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-LIST-USERS.
           CLOSE USERS.
           OPEN INPUT USERS.
           MOVE 0 TO STATE.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING USERS-HEADER INTO MSG-BODY OF OUTPUT-BUFFER
                               WITH POINTER PTR(1).
           PERFORM SEND-LINE.
           PERFORM LIST-USER-RECORD UNTIL DONE.
           CLOSE USERS.
           OPEN I-O USERS.

       LIST-USER-RECORD.
           READ USERS NEXT RECORD, AT END MOVE 99 TO STATE.
           MOVE 0 TO WOPO-COUNTER.
           IF SHOULD-SHOW-ESCAPES THEN
      D        DISPLAY "USER NAME. ", USER-NAME
               MOVE USER-NAME TO R(1)
               CALL "RE-ESCAPE" USING R(1), WOPO-COUNTER
               MOVE R(1) TO USER-NAME
      D        DISPLAY "USER NAME. ", USER-NAME
               .
           IF NOT DONE THEN
               PERFORM BEGIN-STANDARD-REPLY
               STRING USER-NAME
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               ADD WOPO-COUNTER TO PTR(1)
               STRING USER-LEVEL
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM SEND-LINE.

       HANDLE-JOIN.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 80 AND
              PARAM OF WOPO(2) IS NOT EQUAL TO "0" THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               STRING "JOIN ", DELIMITED BY SIZE,
                      PARAM OF WOPO(2), DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-PART.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE TARGET TO R(1)
           ELSE
               MOVE PARAM OF WOPO(2) TO R(1)
           IF USER-LEVEL IS NOT LESS THAN 80 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               STRING "PART " DELIMITED BY SIZE,
                      R(1) DELIMITED BY SPACES,
                      "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       STRING-LOWVS.
           STRING "$LOWV$" INTO MSG-BODY OF OUTPUT-BUFFER
                           WITH POINTER PTR(1).

       STRING-PARAMS.
           STRING PARAM OF WOPO(WOPO-COUNTER) DELIMITED BY SPACES
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           ADD 1 TO PTR(1).

       HANDLE-VOICE.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE NICK TO PARAM OF WOPO(2)
               MOVE 2 TO NUM-PARAMS OF WOPO.
           IF USER-LEVEL IS NOT LESS THAN 60 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(1)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES,
                      " +" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM STRING-LOWVS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		             NUM-PARAMS OF WOPO
               ADD 1 TO PTR(1)
               PERFORM STRING-PARAMS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		             NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-DEVOICE.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE NICK TO PARAM OF WOPO(2)
               MOVE 2 TO NUM-PARAMS OF WOPO.
           IF USER-LEVEL IS NOT LESS THAN 60 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(1)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES,
                      " -" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM STRING-LOWVS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		             NUM-PARAMS OF WOPO
               ADD 1 TO PTR(1)
               PERFORM STRING-PARAMS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		             NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       STRING-LOWOS.
           STRING "$LOWO$" INTO MSG-BODY OF OUTPUT-BUFFER
                           WITH POINTER PTR(1).

       HANDLE-OP.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE NICK TO PARAM OF WOPO(2)
               MOVE 2 TO NUM-PARAMS OF WOPO.
           IF USER-LEVEL IS NOT LESS THAN 70 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(1)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES,
                      " +" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM STRING-LOWOS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		       	     NUM-PARAMS OF WOPO
               ADD 1 TO PTR(1)
               PERFORM STRING-PARAMS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		       	     NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-DEOP.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE NICK TO PARAM OF WOPO(2)
               MOVE 2 TO NUM-PARAMS OF WOPO.
           IF USER-LEVEL IS NOT LESS THAN 70 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(1)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET DELIMITED BY SPACES,
                      " -" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM STRING-LOWOS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		       	     NUM-PARAMS OF WOPO
               ADD 1 TO PTR(1)
               PERFORM STRING-PARAMS
                       VARYING WOPO-COUNTER
                       FROM 2, BY 1
                       UNTIL WOPO-COUNTER IS GREATER THAN
		       	     NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-QUIT.
           PERFORM REPLY-ACK.
           MOVE "QUIT-MESSAGE" TO CONFIG-KEY.
           READ CONFIG RECORD
               INVALID KEY MOVE SPACES TO CONFIG-VALUE.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 90 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               STRING "QUIT $COLN$" DELIMITED BY SIZE,
                      CONFIG-VALUE,
                      INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE
               GO TO QUIT
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-SHOW-ESCAPES.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 90 THEN
               IF PARAM OF WOPO(2) IS NOT EQUAL TO SPACES THEN
                   IF PARAM OF WOPO(2) IS EQUAL TO "ON" THEN
                       SET SHOW-ESCAPES TO ON
                   ELSE IF PARAM OF WOPO(2) IS EQUAL TO "OFF" THEN
                       SET SHOW-ESCAPES TO OFF
                   ELSE NEXT SENTENCE
               ELSE IF SHOULD-SHOW-ESCAPES THEN
                   SET SHOW-ESCAPES TO OFF
               ELSE SET SHOW-ESCAPES TO ON
           ELSE
               PERFORM REPLY-NAK.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "SHOW-ESCAPES " INTO MSG-BODY OF OUTPUT-BUFFER
                                  WITH POINTER PTR(1).
           IF SHOULD-SHOW-ESCAPES THEN
               STRING "ON." INTO MSG-BODY OF OUTPUT-BUFFER
                                 WITH POINTER PTR(1)
           ELSE STRING "OFF." INTO MSG-BODY OF OUTPUT-BUFFER
                              WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-RELEVEL.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE PARAM OF WOPO(2) TO USER-NAME.
           IF USER-LEVEL IS NOT LESS THAN 99 THEN
               MOVE PARAM OF WOPO(3) TO USER-LEVEL
               IF USER-LEVEL IS NOT GREATER THAN ZERO THEN
                   DELETE USERS RECORD
                       INVALID KEY NEXT SENTENCE
               ELSE
                   REWRITE USER-RECORD
                       INVALID KEY WRITE USER-RECORD
           ELSE
               PERFORM REPLY-NAK.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING USER-RECORD
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-BF-CODE.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               PERFORM BEGIN-STANDARD-REPLY
               STRING "CODE. ",
                      BF-CODE,
                      "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
      D        DISPLAY "BF-CODE. ", BF-CODE
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-ACK
               PERFORM VALIDATE-USER
               MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
	            TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER DELIMITED BY SPACE
                        INTO R(1),
                        WITH POINTER PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        INTO R(1)
                        WITH POINTER PTR(1)
               IF USER-LEVEL IS NOT LESS THAN 60 THEN
                   MOVE R(1) TO BF-CODE
               ELSE
                   PERFORM REPLY-NAK.

       HANDLE-BF-INPUT.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               PERFORM BEGIN-STANDARD-REPLY
               STRING "INPUT. ",
                      BF-INPUT
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(1)
      D        DISPLAY "INPUT. ", BF-INPUT
      D        DISPLAY "MSG-BODY. ", MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-ACK
               PERFORM VALIDATE-USER
               MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
	            TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER DELIMITED BY SPACE
                        INTO R(1),
                        WITH POINTER PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        INTO R(1)
                        WITH POINTER PTR(1)
               IF USER-LEVEL IS NOT LESS THAN 50 THEN
                   MOVE R(1) TO BF-INPUT
               ELSE
                   PERFORM REPLY-NAK.

       HANDLE-BF-OUTPUT.
           PERFORM BEGIN-STANDARD-REPLY.
      D    DISPLAY "BF OUTPUT. ", BF-OUTPUT.
      D    DISPLAY "STRINGING BF OUTPUT".
           IF SHOULD-SHOW-ESCAPES THEN
               CALL "RE-ESCAPE" USING BF-OUTPUT, WOPO-COUNTER.
           STRING "OUTPUT. " DELIMITED BY SIZE,
                  BF-OUTPUT DELIMITED BY "$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
      D    DISPLAY "SENDING LINE".
           PERFORM SEND-LINE.
      D    DISPLAY "SENT LINE".

       HANDLE-BF-RUN.
      D    DISPLAY "HANDLING BF-RUN".
           PERFORM REPLY-ACK
           PERFORM VALIDATE-USER.
           IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
               MOVE 999 TO MAYBE-CYCLE-LIMIT
           ELSE
               UNSTRING PARAM OF WOPO(2) INTO MAYBE-CYCLE-LIMIT.
      D    DISPLAY "CYCLE LIMIT. ", CYCLE-LIMIT.
           IF USER-LEVEL IS NOT LESS THAN 50 THEN
               MOVE MAYBE-CYCLE-LIMIT TO CYCLE-LIMIT
               PERFORM BF-LIMIT-CYCLES
               CALL "BF-RUN" USING BF-I-O
      D        DISPLAY "BF RAN"
               PERFORM HANDLE-BF-OUTPUT
           ELSE
               PERFORM REPLY-NAK.

       BF-LIMIT-CYCLES.
           IF CYCLE-LIMIT > 999 THEN
               IF USER-LEVEL < 90 THEN
                   IF USER-LEVEL < 70 THEN
                       MOVE 999 TO CYCLE-LIMIT
                       PERFORM BF-CYCLES-LIMITED
                   ELSE IF CYCLE-LIMIT > 9999 THEN
                       MOVE 9999 TO CYCLE-LIMIT
                       PERFORM BF-CYCLES-LIMITED.

       BF-CYCLES-LIMITED.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "INSUFFICIENT LEVEL FOR REQUESTED CYCLE LIMIT. "
      -           "ACTUAL LIMIT WILL BE "
                  CYCLE-LIMIT
                  "."
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(1).
           PERFORM SEND-LINE.

       HANDLE-CTCP.
      D    DISPLAY "HANDLING CTCP.".
           IF NOTICE AND TARGET IS NOT EQUAL TO WOPO-NICK THEN
               NEXT SENTENCE
           ELSE
               MOVE 6 TO PTR(1)
               UNSTRING PARAM OF WOPO(1) DELIMITED BY SPACE OR "$SOH$"
                        INTO R(1)
                        WITH POINTER PTR(1)
      D        DISPLAY "CTCP PARAM. ", R(1)
               MOVE R(1) TO PARAM OF WOPO(1)
               IF PARAM OF WOPO(1) IS EQUAL TO "PING" THEN
                   PERFORM HANDLE-PING
               ELSE IF PARAM OF WOPO(1) IS EQUAL TO "VERSION" THEN
                   PERFORM HANDLE-VERSION
      *        ELSE IF PARAM OF WOPO(1) IS EQUAL TO "TIME" THEN
      *            PERFORM HANDLE-TIME
               ELSE NEXT SENTENCE.

       HANDLE-PING.
           MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
	        TO PTR(1).
           ADD 5 TO PTR(1)
           UNSTRING MSG-BODY OF INPUT-BUFFER DELIMITED BY "$NUL$"
                    INTO R(1)
                    WITH POINTER PTR(1).
           STRING "NOTICE " DELIMITED BY SIZE,
                  NICK DELIMITED BY SPACES,
                  " $COLN$$SOH$" DELIMITED BY SIZE,
                  R(1) DELIMITED BY "$SOH$",
                  "$SOH$$NUL$" DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER.
      D    DISPLAY MSG-BODY OF OUTPUT-BUFFER.
           PERFORM SEND-LINE.

       HANDLE-VERSION.
      D    DISPLAY "HANDLING VERSION."
           STRING "NOTICE " DELIMITED BY SIZE,
                  NICK DELIMITED BY SPACES,
                  " $COLN$$SOH$VERSION WOPO THE COBOL-74 BOT. "
      -           "VERSION WHATEVER. RUNNING ON " DELIMITED BY SIZE
                  PLATFORM DELIMITED BY SPACES
                  ".$SOH$$NUL$" DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER.
           PERFORM SEND-LINE.

      *HANDLE-TIME.
      *    MOVE TIME TO FORMATTED-TIME.
      *    STRING "NOTICE " DELIMITED BY SIZE,
      *           NICK DELIMITED BY SPACES,
      *           " $COLN$$SOH$TIME" DELIMITED BY SIZE,
      *           FORMATTED-TIME DELIMITED BY SIZE,
      *           "$SOH$"
      *           INTO MSG-BODY OF OUTPUT-BUFFER.
      *    PERFORM SEND-LINE.

       QUIT.
           CALL "CHANNEL-CLOSE".
           CLOSE CONFIG.
           CLOSE USERS.
           STOP RUN.
