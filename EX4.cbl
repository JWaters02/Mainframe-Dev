       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX4.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 18/07/22.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO SORTOUT
           FILE STATUS IS WS-INPUT-STATUS.

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

       WORKING-STORAGE SECTION.

       01 WS-INPUT-STATUS                    PIC X(2).
           88 INPUT-OK                       VALUE "00".
           88 INPUT-EOF                      VALUE "10".
           88 INPUT-VALID                    VALUE "00", "10".
       01 WS-EOF-POINT                       PIC X VALUE 'N'.
       01 WS-COUNT-RECORDS                   PIC 9(5) VALUE 0.
       01 WS-COUNT-ITEM-TYPES.
           05 COUNT-ITEM-1                   PIC 9(3) VALUE 0.
           05 COUNT-ITEM-2                   PIC 9(3) VALUE 0.
           05 COUNT-ITEM-3                   PIC 9(3) VALUE 0.
       01 WS-ITEM-TYPES.
           05 RECORD-ITEM-1                  PIC X(6) VALUE 'A00001'.
           05 RECORD-ITEM-2                  PIC X(6) VALUE 'B00001'.
           05 RECORD-ITEM-3                  PIC X(6) VALUE 'C00001'.

       PROCEDURE DIVISION.

       PROGRAM-CONTROL.
           OPEN INPUT FILEIN.
           PERFORM READ-DATA UNTIL WS-EOF-POINT = 'Y'.
           CLOSE FILEIN.

           DISPLAY 'ALL RECORDS: ' WS-COUNT-RECORDS.
           DISPLAY 'ALL RECORDS FOR A00001: ' COUNT-ITEM-1.
           DISPLAY 'ALL RECORDS FOR B00001: ' COUNT-ITEM-2.
           DISPLAY 'ALL RECORDS FOR C00001: ' COUNT-ITEM-3.

           STOP RUN.

       READ-DATA.
           READ FILEIN AT END MOVE 'Y' TO WS-EOF-POINT.
           EVALUATE ITEM
              WHEN RECORD-ITEM-1
                 ADD 1 TO COUNT-ITEM-1
              WHEN RECORD-ITEM-2
                 ADD 1 TO COUNT-ITEM-2
              WHEN RECORD-ITEM-3
                 ADD 1 TO COUNT-ITEM-3
           END-EVALUATE.
