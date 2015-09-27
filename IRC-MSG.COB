       IDENTIFICATION DIVISION.
       PROGRAM-ID. "IRC-MSG".

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PREFIX-WORK PIC X(96).
       01 MSG-POINTER PIC 9(3).

       LINKAGE SECTION.
       01 BUFFER.
           03 MSG-LENGTH PIC 9(3).
	   03 MSG-BODY PIC X(512).
           03 MSG-BODY-1 REDEFINES MSG-BODY PIC X.
               88 HAS-PREFIX VALUE ":".
       01 IRC-MESSAGE.
           03 PREFIX.
               05 NICK PIC X(16).
               05 IDENT PIC X(16).
               05 HOST PIC X(64).
           03 COMMAND PIC X(16).
           03 PARAMETERS.
               05 TARGET PIC X(50).
               05 REST PIC X(480).

       PROCEDURE DIVISION USING BUFFER, IRC-MESSAGE.
           INITIALIZE IRC-MESSAGE, PREFIX-WORK, MSG-POINTER.
           IF HAS-PREFIX THEN
               PERFORM WITH-PREFIX
           ELSE
               MOVE 1 TO MSG-POINTER.
           UNSTRING MSG-BODY,
               DELIMITED BY SPACES,
               INTO COMMAND, TARGET
               WITH POINTER MSG-POINTER.
           UNSTRING MSG-BODY,
               DELIMITED BY "  ",
               INTO REST,
               WITH POINTER MSG-POINTER.

       WITH-PREFIX.
           MOVE 2 TO MSG-POINTER.
           UNSTRING MSG-BODY,
               DELIMITED BY SPACES,
               INTO PREFIX-WORK,
               WITH POINTER MSG-POINTER.
           UNSTRING PREFIX-WORK,
               DELIMITED BY "!" OR "@",
               INTO NICK,
                    IDENT,
                    HOST.
