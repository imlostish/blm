      *> ----------------------------------------------------------------
      *> Author: imlostish
      *> Date:   19/07/2025
      *> Purpose: Command line argument parsing for utilitys
      *> License: MIT
      *> PLEASE GIVE ME A JOB
      *> ---------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLM-ARGS-AUTH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-ARG-IDX       PIC 9(4) COMP VALUE 1.
       77 WS-ARG-TEXT      PIC X(100).
       LINKAGE SECTION.
       01 L-ARGS-LEN       PIC 9(4) COMP.
       01 L-ARGS-VALUE.
           05 L-ARG-VAL     PIC X(100) OCCURS 10 TIMES.


       PROCEDURE DIVISION USING L-ARGS-LEN, L-ARGS-VALUE.
           PERFORM DEFAULT-RESPONSE
           PERFORM PARSE-ARGUMENTS
           GOBACK.

       DEFAULT-RESPONSE.
           IF L-ARGS-LEN = 0
               DISPLAY "No arguments provided."
               EXIT PROGRAM
           END-IF.

       AVAILABLE-COMMANDS.
           DISPLAY "Available commands:"
           DISPLAY "  help: Show this help message"
           DISPLAY "  |_ h"
           DISPLAY "  |_ -h"
           DISPLAY "  |_ --help"
           DISPLAY "  version: Show blm version"
           DISPLAY "  |_ v"
           DISPLAY "  |_ -v"
           DISPLAY "  |_ --version"
           DISPLAY "  --authenticate: Authenticate user"
           DISPLAY "  |_ --auth"
           DISPLAY "  ♙ args: Parse arguments for authentication"
           DISPLAY "  |_ <auth-type>: login, register, verify"
           DISPLAY "  |_ <auth-field>: --[username | email | password]"
           DISPLAY "  |_ <username>: Username for authentication"
           DISPLAY "  |_ <email>: Email for authentication"
           DISPLAY "  |_ <password>: Password for authentication"
           DISPLAY "  --account: Account data"
           DISPLAY "  |_ --acc"
           DISPLAY "  ♙ args: Parse arguments for account data"
           DISPLAY "  |_ <acc-op>: view, status, update, delete, ..."
           DISPLAY "  |_ <acc-by>: --[id | username | email | ...]"
           DISPLAY "  |_ <acc-id>: Account ID for operations"
           DISPLAY "  |_ <acc-username>: Username of the account"
           DISPLAY "  |_ <acc-email>: Email of the account"
           DISPLAY "  |_ <acc-password>: Password of the account"
           DISPLAY "  |_ <acc-status>: Status of the account"
           DISPLAY "  |_ <acc-type>: Type of the account"
           DISPLAY "  |_ <acc-balance>: Balance of the account"
           DISPLAY "  |_ <acc-created>: Creation date of the account"
           DISPLAY "  |_ <acc-updated>: Last update date of the account"
           DISPLAY "  --credit-card: Credit cards"
           DISPLAY "  |_ --cc"
           DISPLAY "  ♙ args: Parse arguments for cc"
           DISPLAY "  |_ <cc-op>: add, remove, list, search"
           DISPLAY "  |_ <cc-by>: --[id | name | type | number | ...]"
           DISPLAY "  |_ <cc-id>: Credit card ID for operations"
           DISPLAY "  |_ <cc-name>: Name of the credit card"
           DISPLAY "  |_ <cc-type>: Type of the credit card"
           DISPLAY "  |_ <cc-number>: Credit card number"
           DISPLAY "  |_ <cc-expiry>: Expiry date (MM/YY)"
           DISPLAY "  |_ <cc-cvv>: CVV code"
           DISPLAY "  |_ <cc-holder>: Cardholder name"
           DISPLAY "  --loan: Loan operations"
           DISPLAY "  ♙ args: Parse arguments for loan"
           DISPLAY "  |_ <loan-amount>: Amount of the loan"
           DISPLAY "  |_ <loan-term>: Term of the loan in months"
           DISPLAY "  |_ <loan-rate>: Interest rate of the loan"
           DISPLAY "  |_ <loan-purpose>: Purpose of the loan"
           DISPLAY "  --transfers: Transfer operations"
           DISPLAY "  |_ --tr"
           DISPLAY "  ♙ args: Parse arguments for transfers"
           DISPLAY "  |_ <tr-op>: send, receive, history"
           DISPLAY "  |_ <tr-by>: --[id | amount | date | status | ...]"
           DISPLAY "  |_ <tr-id>: Transfer ID for operations"
           DISPLAY "  |_ <tr-amount>: Amount of the transfer"
           DISPLAY "  |_ <tr-date>: Date of the transfer"
           DISPLAY "  |_ <tr-status>: Status of the transfer"
           DISPLAY "  |_ <tr-recipient>: Recipient of the transfer"
           DISPLAY "  --logout: Logout user"
           DISPLAY "  |_ --lg"
           EXIT PROGRAM.
      
        PARSE-ARGUMENTS.
              PERFORM VARYING WS-ARG-IDX FROM 1 BY 1
                UNTIL WS-ARG-IDX > L-ARGS-LEN
                MOVE L-ARG-VAL(WS-ARG-IDX) TO WS-ARG-TEXT
                EVALUATE WS-ARG-TEXT
                     WHEN "--authenticate" WHEN "--auth"
                          DISPLAY "Authentication command detected."
                     WHEN "--account" WHEN "--acc"
                          DISPLAY "Account command detected."
                     WHEN "--credit-card" WHEN "--cc"
                          DISPLAY "Credit card command detected."
                     WHEN "--loan"
                          DISPLAY "Loan command detected."
                     WHEN "--transfers" WHEN "--tr"
                          DISPLAY "Transfer command detected."
                     WHEN "--logout" WHEN "--lg"
                          DISPLAY "Logout command detected."
                     WHEN "version" WHEN "v" WHEN "-v" WHEN "--version"
                          DISPLAY "current version: 0.0.1beta"
                     WHEN "help" WHEN "h" WHEN "-h" WHEN "--help"
                          PERFORM AVAILABLE-COMMANDS
                     WHEN OTHER
                          DISPLAY "Unknown command: " WS-ARG-TEXT
                END-EVALUATE
              END-PERFORM.
              EXIT PROGRAM.