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
           03 BF-CELL PIC S999
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
      D        DISPLAY CURRENT-INSTRUCTION
           ELSE
               MOVE 99 TO LOOP-STATE.

       EXECUTE-INSTRUCTION.
           MOVE 0 TO LOOP-STATE.
      D    DISPLAY IP, BF-CELL(CURRENT-CELL), LOOP-DEPTH, CYCLES.
           PERFORM READ-INSTRUCTION.
           IF DONE THEN
      D        DISPLAY "GOING TO HELL"
               GO TO HELL.
           IF BF-LEFT THEN
               PERFORM DO-LEFT
           ELSE IF BF-RIGHT THEN
               PERFORM DO-RIGHT
           ELSE IF BF-DEC THEN
               PERFORM DO-DEC
           ELSE IF BF-INC THEN
               PERFORM DO-INC
           ELSE IF BF-OUT THEN
               PERFORM DO-OUT
           ELSE IF BF-IN THEN
               PERFORM DO-IN
           ELSE IF BF-BEGIN THEN
               PERFORM DO-BEGIN
           ELSE IF BF-END THEN
               PERFORM DO-END
	   ELSE IF BF-DIE THEN
	       GO TO HELL.
           ADD 1 TO CYCLES.

       DO-LEFT.
           IF CURRENT-CELL IS GREATER THAN 1 THEN
               SET CURRENT-CELL DOWN BY 1
           ELSE
               SET CURRENT-CELL TO 9999.

       DO-RIGHT.
           IF CURRENT-CELL IS LESS THAN 9999 THEN
               SET CURRENT-CELL UP BY 1
           ELSE
               SET CURRENT-CELL TO 1.

       DO-DEC.
           SUBTRACT 1 FROM BF-CELL(CURRENT-CELL).

       DO-INC.
           ADD 1 TO BF-CELL(CURRENT-CELL).

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

       DO-BEGIN.
           IF BF-CELL(CURRENT-CELL) IS EQUAL TO 0 THEN
               MOVE LOOP-DEPTH TO LOOP-WORK
               PERFORM FIND-END UNTIL DONE
               SUBTRACT 1 FROM IP
           ELSE
               ADD 1 TO LOOP-DEPTH.
           
       FIND-END.
           PERFORM READ-INSTRUCTION.
           IF NOT DONE THEN
               IF BF-BEGIN THEN
                   ADD 1 TO LOOP-WORK
               ELSE IF BF-END THEN
                   IF LOOP-WORK EQUALS LOOP-DEPTH THEN
                       MOVE 99 TO LOOP-STATE
                   ELSE
                       SUBTRACT 1 FROM LOOP-WORK.
           
       DO-END.
           SUBTRACT 1 FROM LOOP-DEPTH.
           IF BF-CELL(CURRENT-CELL) IS NOT EQUAL TO 0 THEN
               MOVE LOOP-DEPTH TO LOOP-WORK
               PERFORM FIND-BEGIN UNTIL DONE.

       FIND-BEGIN.
           PERFORM UNREAD-INSTRUCTION.
           IF NOT DONE THEN
               IF BF-END THEN
                   SUBTRACT 1 FROM LOOP-WORK
               ELSE IF BF-BEGIN THEN
                   IF LOOP-WORK EQUALS LOOP-DEPTH THEN
                       MOVE 99 TO LOOP-STATE
                   ELSE
                       ADD 1 TO LOOP-WORK.

       UNREAD-INSTRUCTION.
           IF IP IS GREATER THAN 1 THEN
               SUBTRACT 2 FROM IP
               UNSTRING BF-CODE INTO CURRENT-INSTRUCTION WITH POINTER IP
           ELSE
               MOVE 99 TO LOOP-STATE.
