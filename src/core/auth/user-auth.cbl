      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Module: user-auth.cbl
      *> Date:   13/07/2025
      *> Purpose: User validation flow
      *> License: MIT
      *> PLEASE GIVE ME A JOB
      *> ----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM-USER-AUTH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Hash Pwd Validation
       77 WS-LOCAL-HASH PIC X(64).
       77 WS-STORED-HASH PIC X(64) VALUE SPACE.

      *> Validation Email
       77 WS-EMAIL            PIC X(124).
       77 WS-AT-COUNT         PIC 9(2).
       77 WS-DOT-COUNT        PIC 9(2).
       77 WS-POS-AT           PIC 9(2).
       77 WS-POS-LAST-DOT     PIC 9(2).
       77 WS-I                PIC 9(2).
       77 WS-CURRENT-CHAR     PIC X.
       77 WS-SPACE-COUNT      PIC 9(2).

       LINKAGE SECTION.

       01 LK-ACTION       PIC X(20).
       01 LK-INPUT        PIC X(64).
       01 LK-RET-CODE     PIC S9(4) COMP.

       PROCEDURE DIVISION USING LK-ACTION LK-INPUT LK-RET-CODE.
           EVALUATE LK-ACTION
               WHEN "IS-USERNAME-VALID"
                   PERFORM IS-USERNAME-VALID
               WHEN "IS-USERNAME-UNUSED"
                   PERFORM IS-USERNAME-UNUSED
               WHEN "IS-EMAIL-VALID"
                   PERFORM IS-EMAIL-VALID
               WHEN "IS-EMAIL-UNUSED"
                   PERFORM IS-EMAIL-UNUSED
               WHEN "HASH-PASSWORD"
                   PERFORM HASH-PASSWORD
               WHEN "COMPARE-HASH"
                   PERFORM COMPARE-HASH
               WHEN OTHER
                   DISPLAY "Funcion desconocida en USER-AUTH-UTILS"
                   MOVE 12 TO LK-RET-CODE
           END-EVALUATE
      
           GOBACK.

       IS-USERNAME-VALID.
           IF FUNCTION LENGTH(FUNCTION TRIM(LK-INPUT)) < 5
              MOVE 4 TO LK-RET-CODE  *> Username demasiado corto
           ELSE
           INSPECT LK-INPUT TALLYING WS-SPACE-COUNT FOR ALL " "
           IF WS-SPACE-COUNT > 0
               MOVE 5 TO LK-RET-CODE *> No se permiten espacios
           ELSE
               MOVE 0 TO LK-RET-CODE
           END-IF
           END-IF.
           EXIT.
       IS-USERNAME-UNUSED.
           EXIT.
       IS-EMAIL-VALID.
           CALL "BLM-EMAIL-UTILS"
               USING LK-INPUT LK-RET-CODE
           IF LK-RET-CODE = 0
               DISPLAY "✅ Email validado OK."
           ELSE
               DISPLAY "❌ Email inválido."
           END-IF.
           EXIT.

       IS-EMAIL-UNUSED.
           EXIT.

       HASH-PASSWORD.
           DISPLAY "→ simulando hash de password: " LK-INPUT
           CALL "SHA256" USING LK-INPUT WS-LOCAL-HASH
           DISPLAY "User Inp: " LK-INPUT
           DISPLAY "hash Out: " WS-LOCAL-HASH
           MOVE 0 TO LK-RET-CODE.
           EXIT.

       COMPARE-HASH.
           DISPLAY "Comparando hash..."
           PERFORM HASH-PASSWORD
           DISPLAY "hash Ingresado: " WS-LOCAL-HASH
           DISPLAY "hash Almacenado: " WS-STORED-HASH

           IF WS-LOCAL-HASH = WS-STORED-HASH
               DISPLAY "La contraseña es correcta."
               MOVE 0 TO LK-RET-CODE
           ELSE
               DISPLAY "La contraseña es incorrecta."
               MOVE 1 TO LK-RET-CODE
           END-IF.
           EXIT.
