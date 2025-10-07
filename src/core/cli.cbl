      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Module: cli.cbl
      *> Date:   13/07/2025
      *> Purpose: Terminal CLI
      *> License: MIT
      *> PLEASE GIVE ME A JOB
      *> ----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM-CLI.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       COPY 'login-data.cpy'.
       COPY 'register-data.cpy'.
       01 WS-VARS.
       05 WS-RET-CODE      PIC S9(4) COMP VALUE 0.
       05 WS-OPTION        PIC 9.
       05 WS-EXIT-FLAG     PIC X VALUE "N".
           88 EXIT-APP     VALUE "Y".
       05 WS-ACCOUNT-FLAG  PIC X VALUE "N".
           88 LOGGED-IN    VALUE "Y".
           88 REG-SUCCESS  VALUE "Y".
       LINKAGE SECTION.
       77  L-CLI-RET-CODE  PIC S9(4) COMP VALUE 0.

       PROCEDURE DIVISION RETURNING L-CLI-RET-CODE.
           PERFORM UNTIL EXIT-APP
               PERFORM WELCOME-MENU
           END-PERFORM

           MOVE 0 TO L-CLI-RET-CODEk
           GOBACK.

       WELCOME-MENU.
           DISPLAY "=== WELCOME TO BLM ==="
           DISPLAY "1) Create account"
           DISPLAY "2) Login"
           DISPLAY "3) Exit"
           ACCEPT WS-OPTION
           EVALUATE WS-OPTION
             WHEN 1 PERFORM REGISTRATION-PROCESS
             WHEN 2 PERFORM LOGIN-PROCESS
             WHEN 3 MOVE "Y" TO WS-EXIT-FLAG
             WHEN OTHER DISPLAY "Invalid option"
           END-EVALUATE.
           EXIT.

       REGISTRATION-PROCESS.
           DISPLAY ">>> REGISTER <<<"
           DISPLAY "User: " WITH NO ADVANCING
           ACCEPT RD-USERNAME IN REGISTER-DATA
           DISPLAY "Email: " WITH NO ADVANCING
           ACCEPT RD-EMAIL IN REGISTER-DATA
           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT RD-PWD IN REGISTER-DATA WITH NO ECHO

          CALL "BLM-USER-CONTROLLER"
           USING "CREATE-USER", REGISTER-DATA, WS-RET-CODE
           IF WS-RET-CODE NOT = 0
              DISPLAY "Error creating user: ", WS-RET-CODE
           END-IF.

           IF (RD-USERNAME NOT = SPACES)
              AND (RD-PWD NOT = SPACES)
             MOVE "Y" TO WS-ACCOUNT-FLAG
             DISPLAY "Registration successful."
           ELSE
               DISPLAY "Invalid data"
           END-IF.
           EXIT.

       LOGIN-PROCESS.
           DISPLAY ">>> LOGIN <<<"
           DISPLAY "Email: " WITH NO ADVANCING
           ACCEPT LD-EMAIL IN LOGIN-DATA
           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT LD-PWD IN LOGIN-DATA WITH NO ECHO
           CALL "BLM-USER-AUTH" USING "HASH-PASSWORD",
                                      LD-PWD,
                                      WS-RET-CODE

           IF (LD-EMAIL = "imlostish")
              AND (LD-PWD = "imlostish")
               DISPLAY "Login OK."
               PERFORM ACCOUNT-MENU
           ELSE
               DISPLAY "Login failed."
           END-IF.
           EXIT.

       ACCOUNT-MENU.
           MOVE "N" TO WS-EXIT-FLAG    *> return to main-loop
           PERFORM UNTIL EXIT-APP
               DISPLAY "=== ACCOUNT MENU ==="
               DISPLAY "1) Credit cards"
               DISPLAY "2) Payments"
               DISPLAY "3) Transfers"
               DISPLAY "4) Logout"
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                 WHEN 1 DISPLAY "Showing credit cards..."
                 WHEN 2 DISPLAY "Showing payments..."
                 WHEN 3 DISPLAY "Showing transfers..."
                 WHEN 4 MOVE "Y" TO WS-EXIT-FLAG
                 WHEN OTHER DISPLAY "Try again."
               END-EVALUATE
           END-PERFORM.
           EXIT.
       END PROGRAM BLM-CLI.
