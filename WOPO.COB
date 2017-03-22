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
               ACCESS MODE IS DYNAMIC
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

       01 I-O-REGS.
           03 INPUT-BUFFER.
               05 MSG-BODY PIC X(999).
               05 ASCII-TABLE.
                   07 ASCII-CELL PIC 999 OCCURS 999 TIMES.
           03 INPUT-SOURCE PIC 9.
               88 STANDARD-INPUT VALUE 0.
           03 OUTPUT-BUFFER.
               05 MSG-BODY PIC X(999).
               05 ASCII-TABLE.
                   07 ASCII-CELL PIC 999 OCCURS 999 TIMES.
           03 OUTPUT-DEST PIC 9.
               88 STANDARD-OUTPUT VALUE 0.
           03 OUTPUT-SPEC.
              05 COMMAND PIC X(16).
              05 NICK PIC X(40).
              05 TARGET PIC X(50).

       01 WOPO.
           03 NICK PIC X(40).
           03 REGISTER-FILE.
               05 REGISTER OCCURS 8 TIMES.
                   07 R PIC X(999).
                   07 R-COMMAND REDEFINES R.
                       09 PREFIX PIC XX.
                         88 IS-COMMAND VALUE "$$".
                       09 COMMAND-BODY PIC X(997).
                   07 R-CTCP REDEFINES R.
                       09 CTCP-PREFIX PIC X(5).
                           88 IS-CTCP VALUE "$SOH$".
                       09 CTCP-BODY PIC X(994).
                   07 R-SWITCH REDEFINES R.
                       09 SWITCH PIC X.
                       09 SWITCH-PARAM PIC X.
                   07 PTR PIC 999.
               05 SRC PIC 9.
               05 DEST PIC 9.
           03 PARAM PIC 999 OCCURS 9 TIMES.
           03 NUM-PARAMS PIC 9.
           03 WOPO-COUNTER PIC 9.
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
           MOVE CONFIG-VALUE TO NICK OF WOPO.
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER.
           STRING "NICK " DELIMITED BY SIZE,
                  NICK OF WOPO DELIMITED BY SPACES,
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

       GET-IRC-STATE.
           CALL "PARSE-IRC-MSG" USING MSG-BODY OF INPUT-BUFFER,
                                      IRC-PARAMS.
           MOVE SPACES TO NICK OF IRC-STATE.
           IF GOT-PREFIX THEN
               MOVE MSG-SRC TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        DELIMITED BY "$EXC$" OR "$AT$" OR SPACES
                        INTO NICK OF IRC-STATE
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
                        INTO TARGET OF IRC-STATE
                        WITH POINTER PTR(1)
           ELSE
               MOVE SPACES TO TARGET OF IRC-STATE.

       GET-MSG-CONTENTS.
           MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
                TO PTR(DEST).
           UNSTRING MSG-BODY OF INPUT-BUFFER DELIMITED BY "$NUL$"
                    INTO R(DEST)
                    WITH POINTER PTR(DEST).

       INDEX-PARAMS.
           MOVE 0 TO NUM-PARAMS OF WOPO, STATE.
           MOVE 1 TO PTR(DEST)
           PERFORM INDEX-PARAM UNTIL DONE.

       INDEX-PARAM.
           ADD 1 TO NUM-PARAMS OF WOPO.
           MOVE PTR(DEST) TO PARAM OF WOPO(NUM-PARAMS OF WOPO).
           UNSTRING R(SRC) DELIMITED BY SPACE
                    INTO R(DEST)
                    WITH POINTER PTR(DEST).
           IF R(DEST) IS EQUAL TO SPACES THEN
               SUBTRACT 1 FROM NUM-PARAMS OF WOPO
               MOVE 99 TO STATE
           IF NUM-PARAMS OF WOPO IS NOT LESS THAN 9 THEN
               MOVE 99 TO STATE.

       GET-PARAM.
           MOVE PARAM OF WOPO(PTR(SRC)) TO PTR(DEST).
           UNSTRING R(SRC) DELIMITED BY SPACES
                    INTO R(DEST)
                    WITH POINTER PTR(DEST).

       GET-REST.
           MOVE PARAM OF WOPO(PTR(SRC)) TO PTR(DEST).
           UNSTRING R(SRC)
                    INTO R(DEST)
                    WITH POINTER PTR(DEST).

       WAIT-FOR-COMMAND.
           MOVE SPACES TO COMMAND OF IRC-STATE.
           PERFORM RECEIVE-LINE UNTIL
                   COMMAND OF IRC-STATE EQUALS WAITING-COMMAND.

       VALIDATE-USER.
      D    DISPLAY "ENTERED VALIDATE-USER".
           MOVE NICK OF IRC-STATE TO USER-NAME.
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER.
           STRING "PRIVMSG NICKSERV $COLN$ACC " DELIMITED BY SIZE
                  NICK OF IRC-STATE DELIMITED BY SPACE
                  " *$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER.
           PERFORM SEND-LINE.
           MOVE "NOTICE" TO WAITING-COMMAND.
           MOVE 0 TO STATE.
      D    DISPLAY "WAITING FOR ACC."
           PERFORM WAIT-FOR-ACC UNTIL DONE.

       WAIT-FOR-ACC.
           PERFORM WAIT-FOR-COMMAND.
           MOVE 2 TO DEST.
           PERFORM GET-MSG-CONTENTS.
           MOVE 2 TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           MOVE 1 TO PTR(2).
           PERFORM GET-PARAM.
           IF R(1) EQUALS USER-NAME THEN
               MOVE 4 TO PTR(2)
               PERFORM GET-PARAM
               IF R(1) EQUALS "ACC" THEN
                   MOVE 99 TO STATE
                   MOVE 5 TO PTR(2)
                   PERFORM GET-PARAM
                   IF R(1) IS NOT EQUAL TO "3" THEN
                       MOVE 0 TO USER-LEVEL
                   ELSE
                       MOVE 3 TO PTR(2)
                       PERFORM GET-PARAM
                       MOVE R(1) TO USER-NAME
                       READ USERS RECORD
                           INVALID KEY MOVE 0 TO USER-LEVEL.

       MAIN.
           PERFORM RECEIVE-LINE.
      D    DISPLAY "NICK. ", NICK OF IRC-STATE,
      D            "COMMAND. ", COMMAND OF IRC-STATE,
      D            "TARGET. ", TARGET OF IRC-STATE.
           IF PING THEN
               PERFORM PONG
           ELSE IF PRIVMSG OR NOTICE THEN
               PERFORM HANDLE-MESSAGE
           ELSE IF KICK THEN
      D        DISPLAY "PROCESSING KICK"
               PERFORM HANDLE-KICK.

       INIT-REPLY.
           MOVE COMMAND OF IRC-STATE TO COMMAND OF OUTPUT-SPEC.
           MOVE NICK OF IRC-STATE TO NICK OF OUTPUT-SPEC.
           IF TARGET OF IRC-STATE IS EQUAL TO NICK OF WOPO THEN
               MOVE NICK OF IRC-STATE TO TARGET OF OUTPUT-SPEC
           ELSE
               MOVE TARGET OF IRC-STATE TO TARGET OF OUTPUT-SPEC.

       BEGIN-REPLY.
           MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER.
           MOVE 1 TO PTR(SRC).
           STRING COMMAND OF OUTPUT-SPEC DELIMITED BY SPACES
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(SRC).
           ADD 1 TO PTR(SRC).
           STRING TARGET OF OUTPUT-SPEC DELIMITED BY SPACES
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(SRC).
           STRING " $COLN$" DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(SRC).

       BEGIN-STANDARD-REPLY.
           PERFORM BEGIN-REPLY.
           IF TARGET OF OUTPUT-SPEC IS NOT EQUAL TO NICK OF WOPO THEN
               STRING "$226$$128$$139$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(SRC).
               STRING NICK OF OUTPUT-SPEC DELIMITED BY SPACES
                     ". " DELIMITED BY SIZE
                     INTO MSG-BODY OF OUTPUT-BUFFER
                     WITH POINTER PTR(SRC).

       USAGE-REPLY.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "USAGE. " DELIMITED BY SIZE,
                   R(SRC) DELIMITED BY "$NUL$",
                   "$NUL$" DELIMITED BY SIZE
                   INTO MSG-BODY OF OUTPUT-BUFFER
                   WITH POINTER PTR(SRC).
           PERFORM SEND-LINE.

       REPLY-ACK.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "OK.$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(SRC).
           PERFORM SEND-LINE.

       REPLY-NAK.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING "ACCESS DENIED.$NUL$"
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(SRC).
           PERFORM SEND-LINE.

       MAYBE-SHOW-ESCAPES.
           IF SHOULD-SHOW-ESCAPES THEN
               CALL "RE-ESCAPE" USING R(SRC).

       DO-OUTPUT.
           IF STANDARD-OUTPUT THEN
               PERFORM MAYBE-SHOW-ESCAPES
               STRING R(SRC) DELIMITED BY "$NUL$",
                      "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(SRC)
               PERFORM SEND-LINE
           ELSE
               MOVE R(SRC) TO R(OUTPUT-DEST).

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
           IF R(1) IS EQUAL TO NICK OF WOPO THEN
      D        DISPLAY "KICK WAS ME."
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               STRING "JOIN " DELIMITED BY SIZE,
                      TARGET OF IRC-STATE DELIMITED BY SPACES
                      "$NUL$"
                     INTO MSG-BODY OF OUTPUT-BUFFER
               PERFORM SEND-LINE
               MOVE PARAM OF IRC-PARAMS(NUM-PARAMS OF IRC-PARAMS)
                    TO PTR(1)
               UNSTRING MSG-BODY OF INPUT-BUFFER
                        INTO R(1)
                        WITH POINTER PTR(1)
      D        DISPLAY "KICK MESSAGE. ", R(1)
               IF R(1) IS NOT EQUAL TO NICK OF WOPO THEN
                   MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
                   STRING "PRIVMSG " DELIMITED BY SIZE,
                          TARGET OF IRC-STATE DELIMITED BY SPACES,
                          " $COLN$" DELIMITED BY SIZE,
                          NICK OF IRC-STATE DELIMITED BY SPACES,
                          ". " DELIMITED BY SIZE,
                          R(1) DELIMITED BY "$NUL$",
                          "$NUL$"
                          INTO MSG-BODY OF OUTPUT-BUFFER
                   PERFORM SEND-LINE.

       HANDLE-MESSAGE.
      D    DISPLAY "HANDLING MESSAGE."
           MOVE 2 TO DEST.
           PERFORM GET-MSG-CONTENTS.
           IF IS-CTCP(2) THEN
               PERFORM HANDLE-CTCP
           ELSE
               MOVE 0 TO INPUT-SOURCE, OUTPUT-DEST.
               PERFORM INIT-REPLY
               IF IS-COMMAND(2) THEN
                   MOVE COMMAND-BODY(2) TO R(1)
      D            DISPLAY "COMMAND BODY ", R(1)
                   PERFORM HANDLE-COMMAND
               ELSE IF TARGET OF IRC-STATE IS EQUAL TO NICK OF WOPO THEN
                   MOVE R(2) TO R(1)
                   PERFORM HANDLE-COMMAND
               ELSE
                   MOVE 1 TO PTR(2)
                   UNSTRING R(2) DELIMITED BY "$COLN$ "
                            INTO R(1)
                            WITH POINTER PTR(2)
                   IF R(1) IS EQUAL TO NICK OF WOPO THEN
                       UNSTRING R(2)
                                INTO R(1)
                                WITH POINTER PTR(2)
                       PERFORM HANDLE-COMMAND.

       HANDLE-COMMAND.
           MOVE 1 TO SRC.
           MOVE 2 TO DEST.
           PERFORM INDEX-PARAMS.
           MOVE 1 TO PTR(1).
           PERFORM GET-PARAM.
      D    DISPLAY "COMMAND. ", R(2).
           IF STANDARD-INPUT THEN
               MOVE 2 TO PTR(1)
               MOVE 8 TO DEST
               PERFORM GET-REST
               MOVE 8 TO INPUT-SOURCE.
      *    HERE GOES THE SWITCH LOGIC
           MOVE R(2) TO R(1).
           MOVE INPUT-SOURCE TO SRC.
           IF R(1) IS EQUAL TO "BF-CODE" THEN
               PERFORM HANDLE-BF-CODE
           ELSE IF R(1) IS EQUAL TO "BF-INPUT" THEN
               PERFORM HANDLE-BF-INPUT
           ELSE IF R(1) IS EQUAL TO "BF-OUTPUT" THEN
               PERFORM HANDLE-BF-OUTPUT
           ELSE IF R(1) IS EQUAL TO "BF-RUN" THEN
      D        DISPLAY "BF-RUN"
               PERFORM HANDLE-BF-RUN
           ELSE IF R(1) IS EQUAL TO "DEOP" THEN
               PERFORM HANDLE-DEOP
           ELSE IF R(1) IS EQUAL TO "DEVOICE" THEN
               PERFORM HANDLE-DEVOICE
           ELSE IF R(1) IS EQUAL TO "HELP" THEN
               PERFORM HANDLE-HELP
           ELSE IF R(1) IS EQUAL TO "JOIN" THEN
               PERFORM HANDLE-JOIN
           ELSE IF R(1) IS EQUAL TO "LEVEL" THEN
               PERFORM HANDLE-LEVEL
           ELSE IF R(1) IS EQUAL TO "LICK" THEN
               PERFORM HANDLE-LICK
           ELSE IF R(1) IS EQUAL TO "LIST-USERS" THEN
               PERFORM HANDLE-LIST-USERS
           ELSE IF R(1) IS EQUAL TO "OP" THEN
               PERFORM HANDLE-OP
           ELSE IF R(1) IS EQUAL TO "PART" THEN
               PERFORM HANDLE-PART
           ELSE IF R(1) IS EQUAL TO "QUIT" THEN
               PERFORM HANDLE-QUIT
           ELSE IF R(1) IS EQUAL TO "RELEVEL" THEN
               PERFORM HANDLE-RELEVEL
           ELSE IF R(1) IS EQUAL TO "SHITFED" THEN
               PERFORM HANDLE-SHITFED
           ELSE IF R(1) IS EQUAL TO "SHOW-ESCAPES" THEN
               PERFORM HANDLE-SHOW-ESCAPES
           ELSE IF R(1) IS EQUAL TO "SOURCE" THEN
               PERFORM HANDLE-SOURCE
           ELSE IF R(1) IS EQUAL TO "STRESS" THEN
               PERFORM HANDLE-STRESS
           ELSE IF R(1) IS EQUAL TO "VOICE" THEN
                   PERFORM HANDLE-VOICE.

       HANDLE-HELP.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS GREATER THAN 0 THEN
               MOVE 1 TO PTR(SRC)
               PERFORM GET-PARAM
           ELSE
               MOVE SPACES TO R(1).
           IF R(1) IS EQUAL TO "ME" THEN
               STRING "$240$$159$$142$$135$ ",
                       "GOD HELPS THOSE WHO HELP THEMSELVES, COMMIE. ",
                       "$240$$159$$142$$134$$NUL$"
                       INTO R(1)
           ELSE
               STRING "COMMANDS. "
                      "$$BF-CODE $$BF-INPUT $$BF-OUTPUT $$BF-RUN ",
                      "$$DEOP $$DEVOICE $$HELP $$JOIN $$LEVEL $$LICK ",
                      "$$LIST-USERS $$OP $$PART $$RELEVEL $$SHITFED ",
                      "$$SHOW-ESCAPES $$SOURCE $$STRESS $$VOICE ",
                      "$NUL$"
                      INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-SHITFED.
           STRING "$002$LEAVE MY CASE ALONE, ",
                  "$226$$156$$168$ASSHOL$LOWE$$226$$156$$168$.$NUL$"
                  INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-SOURCE.
           MOVE "HTTPS$COLN$//GITHUB.COM/HEDDWCH/WOPO$NUL$"
                  TO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-STRESS.
           STRING "$SOH$ACTION PUNCHES A "
                  "$226$$156$$168$BABY$226$$156$$168$.$SOH$$NUL$"
                  INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-LICK.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS EQUAL TO 0 THEN
               MOVE NICK OF OUTPUT-SPEC TO R(1)
           ELSE
               MOVE 1 TO PTR(SRC)
               PERFORM GET-PARAM.
           STRING "$SOH$ACTION VIGOROUSLY LICKS " DELIMITED BY SIZE,
                  R(1) DELIMITED BY SPACES,
                  ".$SOH$$NUL$" DELIMITED BY SIZE
                  INTO R(2).
           MOVE 2 TO SRC.
           PERFORM BEGIN-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-LEVEL.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS GREATER THAN 0 THEN
               MOVE 1 TO PTR(SRC)
               PERFORM GET-PARAM
               MOVE R(1) TO USER-NAME
           ELSE
               PERFORM VALIDATE-USER.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           STRING USER-RECORD, "$NUL$" INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-LIST-USERS.
           CLOSE USERS.
           STRING USERS-HEADER, "$NUL$" INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.
           OPEN I-O USERS.
           MOVE 0 TO STATE.
           PERFORM LIST-USER-RECORD UNTIL DONE.

       LIST-USER-RECORD.
           READ USERS NEXT RECORD, AT END MOVE 99 TO STATE.
           IF NOT DONE THEN
               STRING USER-RECORD, "$NUL$" INTO R(1)
               PERFORM BEGIN-STANDARD-REPLY
               PERFORM DO-OUTPUT.

       HANDLE-JOIN.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE INPUT-SOURCE TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 80 AND
              NUM-PARAMS OF WOPO IS GREATER THAN 0 THEN
               MOVE 1 TO DEST
               MOVE 1 TO PTR(SRC)
               PERFORM GET-PARAM
               IF R(1) IS NOT EQUAL TO "0" THEN
                   MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
                   STRING "JOIN ", DELIMITED BY SIZE,
                          R(1), DELIMITED BY SPACES,
                          "$NUL$"
                          INTO MSG-BODY OF OUTPUT-BUFFER
                   PERFORM SEND-LINE
               ELSE
                   NEXT SENTENCE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-PART.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE INPUT-SOURCE TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS GREATER THAN 0 THEN
               MOVE 1 TO DEST
               MOVE 1 TO PTR(SRC)
               PERFORM GET-PARAM
           ELSE
               MOVE TARGET OF OUTPUT-SPEC TO R(1).
           IF USER-LEVEL IS NOT LESS THAN 80 THEN
               IF R(1) IS NOT EQUAL TO "0" THEN
                   MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
                   STRING "PART ", DELIMITED BY SIZE,
                          R(1), DELIMITED BY SPACES,
                          "$NUL$"
                          INTO MSG-BODY OF OUTPUT-BUFFER
                   PERFORM SEND-LINE
               ELSE
                   NEXT SENTENCE
           ELSE
               PERFORM REPLY-NAK.

       STRING-LOWVS.
           STRING "$LOWV$" INTO MSG-BODY OF OUTPUT-BUFFER
                           WITH POINTER PTR(2).

       STRING-PARAMS.
           PERFORM GET-PARAM.
           ADD 1 TO PTR(2).
           STRING R(1) DELIMITED BY SPACES
                  INTO MSG-BODY OF OUTPUT-BUFFER
                  WITH POINTER PTR(2).

       HANDLE-VOICE.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE INPUT-SOURCE TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS LESS THAN 1 THEN
               MOVE NICK OF OUTPUT-SPEC TO R(3)
               MOVE 3 TO SRC, INPUT-SOURCE
               PERFORM INDEX-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 60 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(2)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET OF OUTPUT-SPEC DELIMITED BY SPACES,
                      " +" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM STRING-LOWVS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               PERFORM STRING-PARAMS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-DEVOICE.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE INPUT-SOURCE TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS LESS THAN 1 THEN
               MOVE NICK OF OUTPUT-SPEC TO R(3)
               MOVE 3 TO SRC, INPUT-SOURCE
               PERFORM INDEX-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 60 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(2)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET OF OUTPUT-SPEC DELIMITED BY SPACES,
                      " -" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM STRING-LOWVS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               PERFORM STRING-PARAMS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       STRING-LOWOS.
           STRING "$LOWO$" INTO MSG-BODY OF OUTPUT-BUFFER
                           WITH POINTER PTR(2).

       HANDLE-OP.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE INPUT-SOURCE TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS LESS THAN 1 THEN
               MOVE NICK OF OUTPUT-SPEC TO R(3)
               MOVE 3 TO SRC, INPUT-SOURCE
               PERFORM INDEX-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 70 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(2)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET OF OUTPUT-SPEC DELIMITED BY SPACES,
                      " +" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM STRING-LOWOS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               PERFORM STRING-PARAMS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM SEND-LINE
           ELSE
               PERFORM REPLY-NAK.

       HANDLE-DEOP.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           MOVE INPUT-SOURCE TO SRC.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS LESS THAN 1 THEN
               MOVE NICK OF OUTPUT-SPEC TO R(3)
               MOVE 3 TO SRC, INPUT-SOURCE
               PERFORM INDEX-PARAMS.
           IF USER-LEVEL IS NOT LESS THAN 70 THEN
               MOVE SPACES TO MSG-BODY OF OUTPUT-BUFFER
               MOVE 1 TO PTR(2)
               STRING "MODE " DELIMITED BY SIZE,
                      TARGET OF OUTPUT-SPEC DELIMITED BY SPACES,
                      " -" DELIMITED BY SIZE
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
               PERFORM STRING-LOWOS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               PERFORM STRING-PARAMS
                       VARYING PTR(SRC)
                       FROM 1, BY 1
                       UNTIL PTR(SRC) IS GREATER THAN
                             NUM-PARAMS OF WOPO
               STRING "$NUL$"
                      INTO MSG-BODY OF OUTPUT-BUFFER
                      WITH POINTER PTR(2)
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
               MOVE INPUT-SOURCE TO SRC
               MOVE 1 TO DEST
               PERFORM INDEX-PARAMS
               IF NUM-PARAMS OF WOPO IS GREATER THAN 0 THEN
                   MOVE 1 TO PTR(SRC)
                   PERFORM GET-PARAM
                   IF R(1) IS EQUAL TO "ON" THEN
                       SET SHOW-ESCAPES TO ON
                   ELSE IF R(1) IS EQUAL TO "OFF" THEN
                       SET SHOW-ESCAPES TO OFF
                   ELSE NEXT SENTENCE
               ELSE IF SHOULD-SHOW-ESCAPES THEN
                   SET SHOW-ESCAPES TO OFF
               ELSE SET SHOW-ESCAPES TO ON
           ELSE
               PERFORM REPLY-NAK.
           IF SHOULD-SHOW-ESCAPES THEN
               MOVE "SHOW-ESCAPES ON.$NUL$" TO R(1)
           ELSE
               MOVE "SHOW-ESCAPES OFF.$NUL$" TO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-RELEVEL.
           PERFORM REPLY-ACK.
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 99 THEN
               MOVE INPUT-SOURCE TO SRC
               MOVE 1 TO DEST
               PERFORM INDEX-PARAMS
               IF NUM-PARAMS OF WOPO IS EQUAL TO 2 THEN
                   MOVE 1 TO PTR(SRC)
                   PERFORM GET-PARAM
                   MOVE R(1) TO USER-NAME
                   MOVE 2 TO PTR(SRC)
                   PERFORM GET-PARAM
                   MOVE R(1) TO USER-LEVEL
                   IF USER-LEVEL IS NOT GREATER THAN ZERO THEN
                       DELETE USERS RECORD
                              INVALID KEY NEXT SENTENCE
                   ELSE
                       REWRITE USER-RECORD
                               INVALID KEY WRITE USER-RECORD
               ELSE
                   MOVE "<ACCOUNT NAME> <LEVEL>$NUL$" TO R(1)
                   MOVE 1 TO SRC
                   PERFORM USAGE-REPLY
           ELSE
               PERFORM REPLY-NAK.
           READ USERS RECORD
               INVALID KEY MOVE 0 TO USER-LEVEL.
           PERFORM BEGIN-STANDARD-REPLY.
           STRING USER-RECORD, "$NUL$" INTO R(1).
           MOVE 1 TO SRC.
           PERFORM DO-OUTPUT.

       HANDLE-BF-CODE.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS LESS THAN 1 THEN
               STRING "CODE. ",
                      BF-CODE,
                      "$NUL$"
                      INTO R(1)
      D        DISPLAY "BF-CODE. ", BF-CODE
               MOVE 1 TO SRC
               PERFORM BEGIN-STANDARD-REPLY
               PERFORM DO-OUTPUT
           ELSE
               PERFORM REPLY-ACK
               PERFORM VALIDATE-USER
               IF USER-LEVEL IS NOT LESS THAN 60 THEN
                   MOVE INPUT-SOURCE TO SRC
                   MOVE 1 TO DEST
                   PERFORM INDEX-PARAMS
                   MOVE 1 TO PTR(SRC)
                   PERFORM GET-REST
                   MOVE R(1) TO BF-CODE
               ELSE
                   PERFORM REPLY-NAK.

       HANDLE-BF-INPUT.
           MOVE 1 TO DEST.
           PERFORM INDEX-PARAMS.
           IF NUM-PARAMS OF WOPO IS LESS THAN 1 THEN
               STRING "INPUT. ",
                      BF-INPUT,
                      "$NUL$"
                      INTO R(1)
      D        DISPLAY "BF-INPUT. ", BF-CODE
               MOVE 1 TO SRC
               PERFORM BEGIN-STANDARD-REPLY
               PERFORM DO-OUTPUT
           ELSE
               PERFORM REPLY-ACK
               PERFORM VALIDATE-USER
               IF USER-LEVEL IS NOT LESS THAN 50 THEN
                   MOVE INPUT-SOURCE TO SRC
                   MOVE 1 TO DEST
                   PERFORM INDEX-PARAMS
                   MOVE 1 TO PTR(SRC)
                   PERFORM GET-REST
                   MOVE R(1) TO BF-INPUT
               ELSE
                   PERFORM REPLY-NAK.

       HANDLE-BF-OUTPUT.
      D    DISPLAY "BF OUTPUT. ", BF-OUTPUT.
           STRING "OUTPUT. " DELIMITED BY SIZE,
                  BF-OUTPUT DELIMITED BY "$NUL$",
                  "$NUL$"
                  INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-BF-RUN.
      D    DISPLAY "HANDLING BF-RUN".
           PERFORM REPLY-ACK
           PERFORM VALIDATE-USER.
           IF USER-LEVEL IS NOT LESS THAN 50 THEN
               MOVE INPUT-SOURCE TO SRC
               MOVE 1 TO DEST
               PERFORM INDEX-PARAMS
               IF NUM-PARAMS OF WOPO IS LESS THAN 2 THEN
                   PERFORM BF-LIMIT-CYCLES
      D            DISPLAY "CYCLE LIMIT. ", CYCLE-LIMIT
                   CALL "BF-RUN" USING BF-I-O
      D            DISPLAY "BF RAN"
                   PERFORM HANDLE-BF-OUTPUT
               ELSE
                   MOVE "<CYCLE LIMIT>" TO R(1)
                   MOVE 1 TO SRC
                   PERFORM USAGE-REPLY
           ELSE
               PERFORM REPLY-NAK.

       BF-LIMIT-CYCLES.
           IF NUM-PARAMS OF WOPO IS EQUAL TO 0 THEN
               MOVE 999 TO CYCLE-LIMIT
           ELSE
               MOVE 1 TO PTR(SRC)
               PERFORM GET-PARAM
               MOVE R(1) TO CYCLE-LIMIT.
           IF CYCLE-LIMIT > 999 THEN
               IF USER-LEVEL < 90 THEN
                   IF USER-LEVEL < 70 THEN
                       MOVE 999 TO CYCLE-LIMIT
                       PERFORM BF-CYCLES-LIMITED
                   ELSE IF CYCLE-LIMIT > 9999 THEN
                       MOVE 9999 TO CYCLE-LIMIT
                       PERFORM BF-CYCLES-LIMITED.

       BF-CYCLES-LIMITED.
           STRING "INSUFFICIENT LEVEL FOR REQUESTED CYCLE LIMIT. ",
                  "ACTUAL LIMIT WILL BE ",
                  CYCLE-LIMIT,
                  "."
                  INTO R(1).
           MOVE 1 TO SRC.
           PERFORM BEGIN-STANDARD-REPLY.
           PERFORM DO-OUTPUT.

       HANDLE-CTCP.
      D    DISPLAY "HANDLING CTCP.".
           IF NOTICE AND 
              TARGET OF IRC-STATE IS NOT EQUAL TO NICK OF WOPO THEN
               NEXT SENTENCE
           ELSE
               MOVE CTCP-BODY(2) TO R(1)
               MOVE 1 TO SRC
               PERFORM INDEX-PARAMS
               MOVE 1 TO PTR(1)
               PERFORM GET-PARAM
      D        DISPLAY "CTCP PARAM. ", R(2)
               IF R(2) IS EQUAL TO "PING" THEN
                   PERFORM HANDLE-PING
               ELSE IF R(2) IS EQUAL TO "VERSION" THEN
                   PERFORM HANDLE-VERSION
      *        ELSE IF R(2) IS EQUAL TO "TIME" THEN
      *            PERFORM HANDLE-TIME
               ELSE NEXT SENTENCE.

       HANDLE-PING.
           STRING "NOTICE " DELIMITED BY SIZE,
                  NICK OF IRC-STATE DELIMITED BY SPACES,
                  " $COLN$$SOH$" DELIMITED BY SIZE,
                  R(1) DELIMITED BY "$SOH$",
                  "$SOH$$NUL$" DELIMITED BY SIZE
                  INTO MSG-BODY OF OUTPUT-BUFFER.
      D    DISPLAY MSG-BODY OF OUTPUT-BUFFER.
           PERFORM SEND-LINE.

       HANDLE-VERSION.
      D    DISPLAY "HANDLING VERSION."
           STRING "NOTICE " DELIMITED BY SIZE,
                  NICK OF IRC-STATE DELIMITED BY SPACES,
                  " $COLN$$SOH$VERSION WOPO THE COBOL-74 BOT. "
                  "VERSION WHATEVER. RUNNING ON " DELIMITED BY SIZE
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
