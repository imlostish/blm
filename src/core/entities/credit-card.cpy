       01 CREDIT-CARD-RECORD.
           05 CARD-ID          PIC X(10).
           05 CARD-USER-ID     PIC X(10).
           05 CARD-NUMBER      PIC X(30).
           05 CARD-HOLDER-NAME PIC X(50).
           05 CARD-EXP-MONTH   PIC XX.
           05 CARD-EXP-YEAR    PIC X(4).
           05 CARD-CVV         PIC X(4).
           05 CARD-BRAND       PIC X(20).
           05 CARD-USAGE-TYPE  PIC X(20).
           05 CARD-IS-PRIMARY  PIC X.     *> 'Y'/'N' or '1'/'0'
           05 CARD-CREATED-AT  PIC X(50).
           05 FILLER           PIC X(10).
