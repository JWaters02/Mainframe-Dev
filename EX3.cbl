       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX3.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 18/07/22.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO CDIN.

       DATA DIVISION.

       FILE SECTION.

       FD FILEIN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 22 CHARACTERS.
       01 FILEIN-RECORD.
           05 ITEM PIC X(6).
           05 OPTION PIC X(2).
           05 PARTNER-CODE PIC X(2).
           05 BRANCH PIC X(4).
           05 PRICE PIC 9(3)V99.
           05 QUANTITY PIC 9(3).

       WORKING-STORAGE SECTION.

       01 WS-EOF-POINT PIC X VALUE 'N'.
       01 WS-COUNT-RECORDS PIC 9(5) VALUE 0.
       01 WS-COUNT-VALID-RECORDS PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

       PROGRAM-CONTROL.
           OPEN INPUT FILEIN
      *    READ FILEIN AT END MOVE 'Y' TO WS-EOF-POINT
           PERFORM READ-FILE UNTIL WS-EOF-POINT = 'Y'
           CLOSE FILEIN

           DISPLAY 'ALL RECORDS: ' WS-COUNT-RECORDS
           DISPLAY 'ALL VALID RECORDS: ' WS-COUNT-VALID-RECORDS

           STOP RUN.

       READ-FILE.
           READ FILEIN AT END MOVE 'Y' TO WS-EOF-POINT
           PERFORM READ-DATA.

       READ-DATA.
           ADD 1 TO WS-COUNT-RECORDS
           IF OPTION = '01' OR OPTION = '03'
           OR OPTION = '04' OR OPTION = '05' THEN
                ADD 1 TO WS-COUNT-VALID-RECORDS
                DISPLAY FILEIN-RECORD
           END-IF

           READ FILEIN AT END MOVE 'Y' TO WS-EOF-POINT.