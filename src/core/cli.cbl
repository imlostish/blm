      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Date:   13/07/2025
      *> Purpose: self-learning
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

       77 WS-OPTION        PIC 9.
       77 WS-EXIT-FLAG     PIC X VALUE "N".
           88 EXIT-APP     VALUE "Y".
       77 WS-ACCOUNT-FLAG  PIC X VALUE "N".
           88 LOGGED-IN    VALUE "Y".
           88 REG-SUCCESS  VALUE "Y".

       77 WS-USERNAME      PIC X(30) VALUE SPACES.
       77 WS-EMAIL         PIC X(30) VALUE SPACES.
       77 WS-PASSWORD      PIC X(30) VALUE SPACES.

       LINKAGE SECTION.

       PROCEDURE DIVISION.
           PERFORM UNTIL EXIT-APP
               PERFORM WELCOME-MENU
           END-PERFORM
           EXIT PROGRAM.

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
           END-EVALUATE
       .
       REGISTRATION-PROCESS.
           DISPLAY ">>> REGISTER <<<"
           DISPLAY "User: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT OMITTED WS-PASSWORD
           IF WS-USERNAME NOT = SPACES AND WS-PASSWORD NOT = SPACES
             MOVE "Y" TO WS-ACCOUNT-FLAG
             DISPLAY "Registration successful."
           ELSE
               DISPLAY "Invalid data"
           END-IF
       .

      *> -----------------------------------------------
        LOGIN-PROCESS.
           DISPLAY ">>> LOGIN <<<"
           DISPLAY "User: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT OMITTED WS-PASSWORD
           IF LOGGED-IN AND (WS-USERNAME = "imlostish")
                         AND (WS-PASSWORD = "imlostish")
               DISPLAY "Login OK."
               PERFORM ACCOUNT-MENU
           ELSE
               DISPLAY "Login failed."
           END-IF
       .

      *> -----------------------------------------------
         ACCOUNT-MENU.
           MOVE "N" TO WS-EXIT-FLAG    *> retorna al main-loop al salir
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
           END-PERFORM
       .