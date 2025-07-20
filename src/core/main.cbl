      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Date:   13/07/2025
      *> Purpose: self‑learning
      *> License: MIT
      *> PLEASE GIVE ME A JOB
      *> ----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARGS-LENGTH        PIC 9(4) COMP.
       01 I                  PIC 9(4) COMP.
       01 ARGS-VALUE.
           05 ARG-VAL        PIC X(100) OCCURS 10 TIMES.

       *> unnecessary variables removed for clarity

       PROCEDURE DIVISION.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARGS-LENGTH
               ACCEPT ARG-VAL(I) FROM ARGUMENT-VALUE
           END-PERFORM.

           IF ARGS-LENGTH > 0
               CALL "BLM-ARGS-AUTH" USING ARGS-LENGTH, ARGS-VALUE
                   RETURNING RETURN-CODE
               IF RETURN-CODE NOT = 0
                   DISPLAY "Error in ARGS-AUTH module"
                   STOP RUN
               END-IF
           END-IF

           CALL "BLM-CLI" RETURNING RETURN-CODE
           IF RETURN-CODE NOT = 0
               DISPLAY "Error in CLI module"
           END-IF

           STOP RUN.
