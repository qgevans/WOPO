       IDENTIFICATION DIVISION.
       PROGRAM-ID. "RE-ESCAPE".

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PTRS.
           03 IN-PTR PIC 999.
           03 OUT-PTR PIC 999.
           03 TEMP-PTR PIC 999.
       01 CURRENT-CHAR PIC X.
           88 ESCAPE-CHAR VALUE "$".
       01 ESCAPE-TEMP PIC XXX.
           88 STRING-TERMINATED VALUE "NUL".
       01 LOOP-STATE PIC 99.
           88 DONE VALUE 99.

       LINKAGE SECTION.
       01 INPUT-BUFFER PIC X(999).
       01 OUTPUT-BUFFER PIC X(999).

       PROCEDURE DIVISION USING	INPUT-BUFFER, OUTPUT-BUFFER.
           MOVE 0 TO LOOP-STATE.
           MOVE 1 TO IN-PTR, OUT-PTR.
           PERFORM MAYBE-ESCAPE-CHAR UNTIL DONE.
      *    IF THE STRING WAS UNTERMINATED, THE POINTER WILL HAVE WRAPPED
           IF OUT-PTR IS GREATER THAN 0 THEN
               MOVE 0 TO LOOP-STATE
               PERFORM BLANK-REST UNTIL DONE.
           EXIT PROGRAM.

       MAYBE-ESCAPE-CHAR.
           IF IN-PTR IS LESS THAN 999 AND
              OUT-PTR IS LESS THAN 999 THEN
               UNSTRING INPUT-BUFFER
	       		INTO CURRENT-CHAR
			WITH POINTER IN-PTR
               IF ESCAPE-CHAR THEN
                   MOVE IN-PTR TO TEMP-PTR
                   UNSTRING INPUT-BUFFER DELIMITED BY "$"
                            INTO ESCAPE-TEMP
                            WITH POINTER TEMP-PTR
                   IF STRING-TERMINATED THEN
                       MOVE 99 TO LOOP-STATE
                       IF OUT-PTR IS LESS THAN 995 THEN
                           STRING "$NUL$"
                                  INTO OUTPUT-BUFFER
                                  WITH POINTER OUT-PTR
                       ELSE
                           NEXT SENTENCE
                   ELSE
                       STRING "$$"
		       	      INTO OUTPUT-BUFFER
			      WITH POINTER OUT-PTR
               ELSE
                   STRING CURRENT-CHAR
		   	  INTO OUTPUT-BUFFER
			  WITH POINTER OUT-PTR
           ELSE
               MOVE 99 TO LOOP-STATE
               UNSTRING INPUT-BUFFER 
	       		INTO CURRENT-CHAR
			WITH POINTER IN-PTR
               IF ESCAPE-CHAR THEN
                   STRING SPACE
                          INTO OUTPUT-BUFFER
                          WITH POINTER OUT-PTR
               ELSE
                   STRING CURRENT-CHAR
                          INTO OUTPUT-BUFFER
                          WITH POINTER OUT-PTR.

       BLANK-REST.
           IF OUT-PTR IS EQUAL TO 999 THEN
               MOVE 99 TO LOOP-STATE.
           STRING SPACE
                  INTO OUTPUT-BUFFER
                  WITH POINTER OUT-PTR.
