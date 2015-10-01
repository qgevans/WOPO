       IDENTIFICATION DIVISION.
       PROGRAM-ID. "IRC-MSG".

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WORK PIC X(480).
       01 MSG-POINTER PIC 9(3).

       LINKAGE SECTION.
       01 BUFFER.
           03 MSG-LENGTH PIC 9(3).
           03 MSG-BODY PIC X(512).
           03 MSG-BODY-1 REDEFINES MSG-BODY PIC X.
               88 HAS-PREFIX VALUE ";".
       01 IRC-MESSAGE.
           03 PREFIX.
               05 NICK PIC X(16).
               05 IDENT PIC X(16).
               05 HOST PIC X(64).
           03 COMMAND PIC X(16).
           03 PARAMETERS.
               05 TARGET PIC X(50).
               05 REST PIC X(480).
               05 REST-PREFIX REDEFINES REST PIC X.
                   88 REST-PARAM VALUE ";".

       PROCEDURE DIVISION USING BUFFER, IRC-MESSAGE.
           MOVE SPACES TO IRC-MESSAGE.
           MOVE SPACES TO WORK.
           MOVE 1 TO MSG-POINTER.
           IF HAS-PREFIX THEN
               PERFORM WITH-PREFIX
           ELSE
               MOVE 1 TO MSG-POINTER.
           UNSTRING MSG-BODY,
               DELIMITED BY SPACES,
               INTO COMMAND, TARGET
               WITH POINTER MSG-POINTER.
           UNSTRING MSG-BODY,
               INTO REST,
               WITH POINTER MSG-POINTER.
           IF REST-PARAM THEN
               MOVE REST TO WORK
               MOVE 2 TO MSG-POINTER
               UNSTRING WORK
                        INTO REST
                        WITH POINTER MSG-POINTER.

       WITH-PREFIX.
           MOVE 2 TO MSG-POINTER.
           UNSTRING MSG-BODY,
                    DELIMITED BY SPACES,
                    INTO WORK,
                    WITH POINTER MSG-POINTER.
           UNSTRING WORK,
                   DELIMITED BY "!" OR "@",
                   INTO NICK,
                   IDENT,
                   HOST.
