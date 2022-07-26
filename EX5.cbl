       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX5.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 25/07/22.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO SORTOUT
           FILE STATUS IS WS-INPUT-STATUS.
           SELECT FILEOUT ASSIGN TO CDOUT.

       DATA DIVISION.

       FILE SECTION.

       FD FILEIN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 22 CHARACTERS.
       01 FILEIN-RECORD.
           05 ITEM                           PIC X(6).
           05 OPTION                         PIC X(2).
           05 PARTNER-CODE                   PIC X(2).
           05 BRANCH                         PIC X(4).
           05 PRICE                          PIC 9(3)V99.
           05 QUANTITY                       PIC 9(3).

       FD FILEOUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 22 CHARACTERS.
       01 FILEOUT-RECORD.
           05 ITEM-OUT                       PIC X(6).
           05 OPTION-OUT                     PIC X(2).
           05 PARTNER-CODE-OUT               PIC X(2).
           05 BRANCH-OUT                     PIC X(4).
           05 PRICE-OUT                      PIC 9(3)V99.
           05 QUANTITY-OUT                   PIC 9(3).

       WORKING-STORAGE SECTION.

       01 WS-INPUT-STATUS                    PIC X(2).
           88 INPUT-OK                       VALUE "00".
           88 INPUT-EOF                      VALUE "10".
           88 INPUT-VALID                    VALUE "00", "10".
       01 WS-EOF-POINT                       PIC X VALUE 'N'.
       01 WS-COUNT-RECORDS                   PIC 9(5) VALUE 0.
       01 WS-COUNT-WRITTEN                   PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAM-CONTROL.
           OPEN INPUT FILEIN
                OUTPUT FILEOUT.
           PERFORM READ-DATA UNTIL WS-EOF-POINT = 'Y'.
           CLOSE FILEIN
                 FILEOUT.

           DISPLAY 'ALL RECORDS: ' WS-COUNT-RECORDS.
           DISPLAY 'ALL WRITTEN RECORDS: ' WS-COUNT-WRITTEN.

           STOP RUN.

       READ-DATA.
           READ FILEIN AT END MOVE 'Y' TO WS-EOF-POINT.
           ADD 1 TO WS-COUNT-RECORDS.
           IF QUANTITY > 100 THEN
                ADD 1 TO WS-COUNT-WRITTEN
                MOVE FILEIN-RECORD TO FILEOUT-RECORD
                WRITE FILEOUT-RECORD
           END-IF.
