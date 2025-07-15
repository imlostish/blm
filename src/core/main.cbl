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
         *> ningún dato en WS necesario aquí

       LINKAGE SECTION.
         *> ningún parámetro de entrada

       PROCEDURE DIVISION.

           DISPLAY ">>> BLM STARTED <<<"
           CALL "BLM-CLI" RETURNING RETURN-CODE
           IF RETURN-CODE NOT = 0
               DISPLAY "Error in CLI module"
           END-IF
           DISPLAY "%> Come back soon!"
           STOP RUN.
