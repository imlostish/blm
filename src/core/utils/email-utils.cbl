      *> --------------------------------------------------------------
      *> Author: imlostish
      *> Module: email-utils.cbl
      *> Date:   14/07/2025
      *> Purpose: Email validation
      *> License: MIT
      *> --------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM-EMAIL-UTILS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 WS-EMAIL            PIC X(124) VALUE SPACES.
       77 WS-AT-COUNT         PIC 9(2).
       77 WS-DOT-COUNT        PIC 9(2).
       77 WS-POS-AT           PIC 9(2).
       77 WS-POS-LAST-DOT     PIC 9(2).
       77 WS-I                PIC 9(3).
       77 WS-CURRENT-CHAR     PIC X.
       77 WS-SPACE-COUNT      PIC 9(2).
       77 WS-LEN              PIC 9(3).

       LINKAGE SECTION.
       01 LK-EMAIL-INPUT      PIC X(124).
       01 LK-EMAIL-RET-CODE   PIC S9(4) COMP.

       PROCEDURE DIVISION USING LK-EMAIL-INPUT LK-EMAIL-RET-CODE.

           MOVE FUNCTION TRIM(LK-EMAIL-INPUT) TO WS-EMAIL
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-EMAIL)) TO WS-LEN

           *> Paso 1: debe haber exactamente un '@'
           IF WS-LEN > 0 AND WS-EMAIL(WS-LEN:1) = "@"
               MOVE 8 TO LK-EMAIL-RET-CODE
               GOBACK.

           *> Paso 2: debe tener al menos un '.'
           INSPECT WS-EMAIL TALLYING WS-DOT-COUNT FOR ALL "."
           IF WS-DOT-COUNT = 0
               MOVE 8 TO LK-EMAIL-RET-CODE
               GOBACK.

           *> Paso 3: no puede iniciar ni terminar con '@'
           IF WS-EMAIL(1:1) = "@"
               MOVE 8 TO LK-EMAIL-RET-CODE
               GOBACK.

           IF WS-LEN > 0
              AND WS-EMAIL(FUNCTION LENGTH(WS-EMAIL):1) = "@"
               MOVE 8 TO LK-EMAIL-RET-CODE
               GOBACK.

           *> Paso 4: '.' debe estar después de '@'
           MOVE 0 TO WS-POS-AT WS-POS-LAST-DOT
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > FUNCTION LENGTH(WS-EMAIL)
               MOVE WS-EMAIL(WS-I:1) TO WS-CURRENT-CHAR
               IF WS-CURRENT-CHAR = "@"
                   MOVE WS-I TO WS-POS-AT
               END-IF
               IF WS-CURRENT-CHAR = "."
                   MOVE WS-I TO WS-POS-LAST-DOT
               END-IF
           END-PERFORM

           IF WS-POS-LAST-DOT <= WS-POS-AT
               MOVE 8 TO LK-EMAIL-RET-CODE
               GOBACK.

           *> Paso 5: sin espacios
           INSPECT LK-EMAIL-INPUT TALLYING WS-SPACE-COUNT FOR ALL " "
           IF WS-SPACE-COUNT > 0
               MOVE 8 TO LK-EMAIL-RET-CODE
               GOBACK.

           DISPLAY "✅ Email válido: " WS-EMAIL
           MOVE 0 TO LK-EMAIL-RET-CODE.

           GOBACK.
       END PROGRAM BLM-EMAIL-UTILS.
