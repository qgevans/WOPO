       IDENTIFICATION DIVISION.
       PROGRAM-ID. "BF-RUN".

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STATE USAGE COMPUTATIONAL.
           03 IN-PTR PIC 9(3) VALUE 1.
           03 IP PIC 9(3) VALUE 1.
           03 CYCLES PIC 9(5) VALUE 0.
           03 OUT-PTR PIC 9(3) VALUE 1.
           03 LOOP-DEPTH PIC 99 VALUE 0.
           03 LOOP-WORK PIC 99 VALUE 0.
           03 LOOP-STATE PIC 99 VALUE 0.
               88 DONE VALUE 99.
       01 BF-MEMORY.
           03 BF-CELL PIC 999
                       USAGE COMPUTATIONAL
                       OCCURS 9999 TIMES
                           INDEXED BY CURRENT-CELL.
       01 CURRENT-INSTRUCTION PIC X.
           88 BF-LEFT VALUE "<".
           88 BF-RIGHT VALUE ">".
           88 BF-DEC VALUE "-".
           88 BF-INC VALUE "+".
           88 BF-OUT VALUE ".".
           88 BF-IN VALUE ",".
           88 BF-BEGIN VALUE "(".
           88 BF-END VALUE ")".
	   88 BF-DIE VALUE "$".
       01 I-O-CHARACTER PIC X.
           88 ESCAPE-CHAR VALUE "$".
       01 CONVERSION.
           03 CHAR-CODE PIC 999.
           03 COBOL-STRING PIC X(6).
      D01  DEBUG-DISPLAY.
      D    03 FILLER PIC XXX VALUE "IP.".
      D    03 DEBUG-IP PIC 9(3).
      D    03 FILLER PIC X VALUE ";".
      D    03 DEBUG-INSTRUCTION PIC X.
      D    03 FILLER PIC X(5) VALUE "CELL.".
      D    03 DEBUG-CURRENT-CELL PIC 999.
      D    03 FILLER PIC X(4) VALUE "VAL.".
      D    03 DEBUG-CELL PIC 999.
      D    03 FILLER PIC X VALUE "L".
      D    03 DEBUG-LOOP-DEPTH PIC 99.
      D    03 FILLER PIC X VALUE "C".
      D    03 DEBUG-CYCLES PIC 9(5).

       LINKAGE SECTION.
       01 BF-I-O.
           03 BF-INPUT PIC X(512).
           03 BF-CODE PIC X(512).
           03 BF-OUTPUT PIC X(512).
           03 CYCLE-LIMIT PIC 9(5).

       PROCEDURE DIVISION USING BF-I-O.
           MOVE 1 TO IN-PTR.
           MOVE 1 TO IP.
           MOVE 0 TO CYCLES.
           MOVE 1 TO OUT-PTR.
           MOVE 0 TO LOOP-DEPTH.
           MOVE 0 TO LOOP-WORK.
           MOVE 0 TO LOOP-STATE.
           MOVE SPACES TO BF-OUTPUT.
           SET CURRENT-CELL TO 1.
           PERFORM ZERO-CELL VARYING CURRENT-CELL
                             FROM 1 BY 1
                             UNTIL CURRENT-CELL IS GREATER THAN 9999.
           SET CURRENT-CELL TO 1.
      D    MOVE 1 TO DEBUG-CURRENT-CELL.
           PERFORM EXECUTE-INSTRUCTION
               UNTIL CYCLES IS GREATER THAN OR EQUAL TO CYCLE-LIMIT.
           EXIT PROGRAM.

       HELL.
           EXIT PROGRAM.

       ZERO-CELL.
           MOVE 0 TO BF-CELL(CURRENT-CELL).

       READ-INSTRUCTION.
           IF IP IS LESS THAN 512 THEN
               UNSTRING BF-CODE INTO CURRENT-INSTRUCTION WITH POINTER IP
           ELSE
               MOVE 99 TO LOOP-STATE.

       EXECUTE-INSTRUCTION.
           MOVE 0 TO LOOP-STATE.
           PERFORM READ-INSTRUCTION.
      D    MOVE IP TO DEBUG-IP.
      D    MOVE CURRENT-INSTRUCTION TO DEBUG-INSTRUCTION.
      D    MOVE BF-CELL(CURRENT-CELL) TO DEBUG-CELL.
      D    MOVE LOOP-DEPTH TO DEBUG-LOOP-DEPTH.
      D    MOVE CYCLES TO DEBUG-CYCLES.
      D    DISPLAY DEBUG-DISPLAY.
           IF DONE THEN
      D        DISPLAY "GOING TO HELL"
               GO TO HELL.
           IF BF-LEFT THEN
               PERFORM DO-LEFT
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-RIGHT AND NOT DONE THEN
               PERFORM DO-RIGHT
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-DEC AND NOT DONE THEN
               PERFORM DO-DEC
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-INC AND NOT DONE THEN
               PERFORM DO-INC
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-OUT AND NOT DONE THEN
               PERFORM DO-OUT
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-IN AND NOT DONE THEN
               PERFORM DO-IN
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-BEGIN AND NOT DONE THEN
               PERFORM DO-BEGIN
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-END AND NOT DONE THEN
               PERFORM DO-END
               MOVE 99 TO LOOP-STATE
           ELSE IF BF-DIE AND NOT DONE THEN
	       GO TO HELL.
           ADD 1 TO CYCLES.

       DO-LEFT.
           IF CURRENT-CELL IS GREATER THAN 1 THEN
               SET CURRENT-CELL DOWN BY 1
      D        SUBTRACT 1 FROM DEBUG-CURRENT-CELL
           ELSE
      D        MOVE 9999 TO DEBUG-CURRENT-CELL
               SET CURRENT-CELL TO 9999.
           MOVE 99 TO LOOP-STATE.

       DO-RIGHT.
           IF CURRENT-CELL IS LESS THAN 9999 THEN
               SET CURRENT-CELL UP BY 1
      D        ADD 1 TO DEBUG-CURRENT-CELL
           ELSE
      D        MOVE 1 TO DEBUG-CURRENT-CELL
               SET CURRENT-CELL TO 1.
           MOVE 99 TO LOOP-STATE.

       DO-DEC.
           IF BF-CELL(CURRENT-CELL) > 0 THEN
               SUBTRACT 1 FROM BF-CELL(CURRENT-CELL)
           ELSE
               MOVE 255 TO BF-CELL(CURRENT-CELL).
           MOVE 99 TO LOOP-STATE.

       DO-INC.
           IF BF-CELL(CURRENT-CELL) < 255 THEN
               ADD 1 TO BF-CELL(CURRENT-CELL)
           ELSE
               MOVE 0 TO BF-CELL(CURRENT-CELL).
           MOVE 99 TO LOOP-STATE.

       DO-OUT.
           IF OUT-PTR < 505 THEN
               MOVE BF-CELL(CURRENT-CELL) TO CHAR-CODE
               CALL "DECODE-ASCII" USING CONVERSION
               IF COBOL-STRING IS EQUAL TO SPACES THEN
	           ADD 1 TO OUT-PTR
 	       ELSE
	           STRING COBOL-STRING,
                   DELIMITED BY SPACES,
                   INTO BF-OUTPUT,
                   WITH POINTER OUT-PTR
      D        DISPLAY "OUT", I-O-CHARACTER, BF-CELL(CURRENT-CELL)
           ELSE
               GO TO HELL.
           MOVE 99 TO LOOP-STATE.

       DO-IN.
           UNSTRING BF-INPUT,
                    INTO I-O-CHARACTER,
                    WITH POINTER IN-PTR.
           IF NOT ESCAPE-CHAR THEN
               MOVE I-O-CHARACTER TO COBOL-STRING
           ELSE IF IN-PTR < 513 THEN
               UNSTRING BF-INPUT,
                        INTO I-O-CHARACTER,
                        WITH POINTER IN-PTR
               IF ESCAPE-CHAR THEN
                   MOVE I-O-CHARACTER TO COBOL-STRING
               ELSE IF IN-PTR < 507 THEN
                   SUBTRACT 1 FROM IN-PTR
                   UNSTRING BF-INPUT,
                            DELIMITED BY "$",
                            INTO COBOL-STRING,
                            WITH POINTER IN-PTR
               ELSE GO TO HELL
           ELSE GO TO HELL.
           CALL "ENCODE-ASCII" USING CONVERSION.
           MOVE CHAR-CODE TO BF-CELL(CURRENT-CELL).
      D    DISPLAY "IN", I-O-CHARACTER, BF-CELL(CURRENT-CELL).
           MOVE 99 TO LOOP-STATE.

       DO-BEGIN.
           IF BF-CELL(CURRENT-CELL) IS EQUAL TO 0 THEN
               MOVE LOOP-DEPTH TO LOOP-WORK
               ADD 1 TO LOOP-WORK
               PERFORM FIND-END UNTIL DONE
           ELSE
               ADD 1 TO LOOP-DEPTH.
           MOVE 99 TO LOOP-STATE.
           
        FIND-END.
      D    DISPLAY "ENTERED FIND-END."
           PERFORM READ-INSTRUCTION.
           IF NOT DONE THEN
               IF BF-BEGIN THEN
                   ADD 1 TO LOOP-WORK
               ELSE IF BF-END THEN
      D            DISPLAY "FOUND END."
                   SUBTRACT 1 FROM LOOP-WORK
                   IF LOOP-WORK EQUALS LOOP-DEPTH THEN
                       MOVE 99 TO LOOP-STATE.
      D    DISPLAY "FIND-END.", IP, ";", CURRENT-INSTRUCTION,
      D            "LD", LOOP-DEPTH, "LW", LOOP-WORK.
           
       DO-END.
           SUBTRACT 1 FROM LOOP-DEPTH.
           IF BF-CELL(CURRENT-CELL) IS NOT EQUAL TO 0 THEN
               MOVE LOOP-DEPTH TO LOOP-WORK
               PERFORM FIND-BEGIN UNTIL DONE
               ADD 1 TO IP.
           MOVE 99 TO LOOP-STATE.

       FIND-BEGIN.
           PERFORM UNREAD-INSTRUCTION.
           IF NOT DONE THEN
               IF BF-END THEN
                   ADD 1 TO LOOP-WORK
               ELSE IF BF-BEGIN THEN
                   SUBTRACT 1 FROM LOOP-WORK
                   IF LOOP-WORK EQUALS LOOP-DEPTH THEN
                       MOVE 99 TO LOOP-STATE.

       UNREAD-INSTRUCTION.
           UNSTRING BF-CODE INTO CURRENT-INSTRUCTION WITH POINTER IP.
           IF IP IS GREATER THAN 3 THEN
               SUBTRACT 2 FROM IP
           ELSE
               MOVE 1 TO IP
               MOVE 99 TO LOOP-STATE.
