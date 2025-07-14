      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Date:   13/07/2025
      *> Purpose: self-learning
      *> License: MIT
      *> PLEASE GIVE ME A JOB
      *> ----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM-USER-AUTH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.

       01 LK-ACTION       PIC X(20).
       01 LK-INPUT        PIC X(30).
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
           IF (LK-INPUT NOT = SPACES)
               DISPLAY "Valid username"
               MOVE 0 TO LK-RET-CODE
           ELSE
               MOVE 4 TO LK-RET-CODE
           END-IF.
           EXIT.

       IS-USERNAME-UNUSED.
           EXIT.
       IS-EMAIL-VALID.
           IF LK-INPUT CONTAINS "@"
               MOVE 0 TO LK-RET-CODE
           ELSE
               MOVE 8 TO LK-RET-CODE
           END-IF.
           EXIT.

       IS-EMAIL-UNUSED.
           EXIT.

       HASH-PASSWORD.
           DISPLAY "→ simulando hash de password: " LK-INPUT
           MOVE 0 TO LK-RET-CODE.
           EXIT.

       COMPARE-HASH.
           EXIT.