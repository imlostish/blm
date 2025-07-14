      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Date:   13/07/2025
      *> Purpose: self-learning
      *> License: MIT
      *> PLEASE GIVE ME A JOB
      *> ----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM-USER-CONTROLLER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "user.cpy"
       LINKAGE SECTION.
       01 LK-ACTION PIC X(20).
       01 LK-DATA   PIC X(30).
       01 LK-RET-CODE PIC S9(4) COMP.

       PROCEDURE DIVISION USING LK-ACTION LK-DATA LK-RET-CODE.

           DISPLAY "blm user controller"

           EVALUATE LK-ACTION
              WHEN "CREATE-USER"
                   PERFORM CREATE-USER
              WHEN "VALIDATE-USER"
                   PERFORM VALIDATE-USER
              WHEN "UPDATE-USER"
                   PERFORM UPDATE-USER
              WHEN "DELETE-USER"
                   PERFORM DELETE-USER
              WHEN OTHER
                   DISPLAY "FUNCTION NOT FOUND."
                   MOVE 12 TO LK-RET-CODE
           END-EVALUATE

           GOBACK.

       CREATE-USER.
           DISPLAY "🚀 Creando usuario: " LK-DATA
           MOVE 0 TO LK-RET-CODE.
           EXIT.

       VALIDATE-USER.
           DISPLAY "🔎 Validando usuario: " LK-DATA
           MOVE 0 TO LK-RET-CODE.
           EXIT.

       UPDATE-USER.
           DISPLAY "✏️ Actualizando usuario: " LK-DATA
           MOVE 0 TO LK-RET-CODE.
           EXIT.

       DELETE-USER.
           DISPLAY "✏️ Borrando usuario: " LK-DATA
           MOVE 0 TO LK-RET-CODE.
           EXIT.

       END PROGRAM BLM-USER-CONTROLLER.