       IDENTIFICATION DIVISION.
       PROGRAM-ID.   REPORT4.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 04/08/22.
      *----------------------------------------------------------------*
      *                                                                *
      *  Project     : REPORT 4                                        *
      *                                                                *
      *  Function    : DISPLAY RECORDS WITH:                           *
      *                - PAGE                                          *
      *                - PARTNER                                       *
      *                - REGION                                        *
      *                BREAK                                           *
      *                                                                *
      *----------------------------------------------------------------*
      * Date         Programmer      Description of change             *
      * ----------  ---------------  --------------------------------- *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT REP-IN ASSIGN TO REP4IN
           FILE STATUS IS WS-REP-IN-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD REP-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 36 CHARACTERS.
       01 REPIN-RECORD.
          05 R-KEY.
             10 R-PARTNER                  PIC X(2).
             10 R-REGION                   PIC X(02).
             10 R-AREA                     PIC X(03).
             10 R-BRANCH                   PIC X(04).
             10 R-ITEM                     PIC 9(06).
          05 R-UNITS                       PIC 9(05).
          05 R-COST-VALUE                  PIC 9(05)V99.
          05 R-RETAIL-VALUE                PIC 9(05)V99.
      *
       WORKING-STORAGE SECTION.
      *
      * Variables...
      *
       01 WS-VARIABLES.
          05 WS-DATE                        PIC 9(8).
          05 WS-DATE-RED                    REDEFINES WS-DATE.
             10 WS-DATE-CC                  PIC 99.
             10 WS-DATE-YY                  PIC 99.
             10 WS-DATE-MM                  PIC 99.
             10 WS-DATE-DD                  PIC 99.
          05 WS-REC-OUT.
             10 WS-REC-AREA                 PIC X(03).
             10 FILLER                      PIC X(6).
             10 WS-REC-BRANCH               PIC X(04).
             10 FILLER                      PIC X(6).
             10 WS-REC-ITEM                 PIC X(06).
             10 FILLER                      PIC X(7).
             10 WS-REC-UNITS                PIC Z(04)9.
             10 FILLER                      PIC X(7).
             10 WS-REC-COST-VALUE           PIC Z(04)9.99.
             10 FILLER                      PIC X(8).
             10 WS-REC-RETAIL-VALUE         PIC Z(04)9.99.
             10 FILLER                      PIC X(3).
          05 WS-PREV-PARTNER                PIC XX.
          05 WS-CUR-PARTNER                 PIC XX.
          05 WS-PREV-REGION                 PIC XX.
          05 WS-CUR-REGION                  PIC XX.
      *
      * Constants...
      *
       01 WS-MAX-RECS-PAGE                  PIC 99      VALUE 20.
       01 WS-REGION-TEXT                    PIC X(7)    VALUE 'REGION'.
       01 WS-PARTNER-TEXT                   PIC X(7)    VALUE 'PARTNER'.
      *
      * Counters...
      *
       01 WS-PAGE-COUNT                     PIC 99      VALUE 1.
       01 WS-REP-IN-COUNT                   PIC 9(9)    VALUE 0.
       01 WS-REGION-COUNTS.
          05 WS-R-ITEM-COUNT                PIC 9(9)    VALUE 0.
          05 WS-R-UNIT-COUNT                PIC 9(9)    VALUE 0.
          05 WS-R-COST-VALUE-COUNT          PIC 9(9)V99 VALUE 0.
          05 WS-R-RETAIL-VALUE-COUNT        PIC 9(9)V99 VALUE 0.
       01 WS-PARTNER-COUNTS.
          05 WS-P-ITEM-COUNT                PIC 9(9)    VALUE 0.
          05 WS-P-UNIT-COUNT                PIC 9(9)    VALUE 0.
          05 WS-P-COST-VALUE-COUNT          PIC 9(9)V99 VALUE 0.
          05 WS-P-RETAIL-VALUE-COUNT        PIC 9(9)V99 VALUE 0.
      *
      * Messages...
      *
       01 WS-MESSAGES.
          05 WS-HEADER1-MSG.
             10 WS-H1-TAG                   PIC X(11)   VALUE
                'NEXT RETAIL'.
             10 FILLER                      PIC XX      VALUE SPACES.
             10 WS-H1-DATE                  PIC X(10)   VALUE
                'DD/MM/CCYY'.
             10 FILLER                      PIC X(34)   VALUE SPACES.
             10 WS-H1-PAGE                  PIC X(4)    VALUE
                'PAGE'.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-H1-PAGE-NUM              PIC 9       VALUE 1.
          05 WS-HEADER2-MSG.
             10 FILLER                      PIC X(6)    VALUE SPACES.
             10 WS-H2-TITLE                 PIC X(60)   VALUE
                '"DISPLAY RECORDS WITH PAGE, PARTNER AND REGION BREAK"'.
          05 WS-HEADER3-MSG.
             10 FILLER                      PIC X(92)   VALUE SPACES.
          05 WS-HEADER4-MSG.
             10 WS-H4-PARTNER               PIC X(9)    VALUE
                'PARTNER: '.
             10 WS-H4-CODE                  PIC XX.
          05 WS-HEADER5-MSG.
             10 WS-H5-REGION                PIC X(9)    VALUE
                'REGION : '.
             10 WS-H5-CODE                  PIC XX.
          05 WS-HEADER6-MSG.
             10 WS-H6-AREA                  PIC X(4)    VALUE 'AREA'.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-H6-BRANCH                PIC X(6)    VALUE 'BRANCH'.
             10 FILLER                      PIC X(6)    VALUE SPACES.
             10 WS-H6-ITEM                  PIC X(4)    VALUE 'ITEM'.
             10 FILLER                      PIC X(8)    VALUE SPACES.
             10 WS-H6-UNITS                 PIC X(5)    VALUE 'UNITS'.
             10 FILLER                      PIC X(6)    VALUE SPACES.
             10 WS-H6-COST-VALUE            PIC X(10)   VALUE
                'COST-VALUE'.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-H6-RETAIL-VALUE          PIC X(12)   VALUE
                'RETAIL-VALUE'.
             10 FILLER                      PIC X       VALUE SPACES.
          05 WS-PARTNER-TOTALS.
             10 WS-P-TEXT                   PIC X(16)   VALUE
                'TOTAL OF PARTNER'.
             10 FILLER                      PIC X(3)    VALUE SPACES.
             10 WS-P-ITEMS                  PIC Z(5)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-P-UNITS                  PIC Z(7)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-P-COST-VALUES            PIC Z(7)9.99.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-P-RETAIL-VALUES          PIC Z(7)9.99.
             10 FILLER                      PIC X(3)    VALUE SPACES.
          05 WS-REGION-TOTALS.
             10 WS-R-TEXT                   PIC X(16)   VALUE
                'TOTAL OF REGION'.
             10 FILLER                      PIC X(3)    VALUE SPACES.
             10 WS-R-ITEMS                  PIC Z(5)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-R-UNITS                  PIC Z(7)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-R-COST-VALUES            PIC Z(7)9.99.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-R-RETAIL-VALUES          PIC Z(7)9.99.
             10 FILLER                      PIC X(3)    VALUE SPACES.
          05 WS-DISPLAY-DATE.
             10 WS-DISP-DATE-DD             PIC XX.
             10 FILLER                      PIC X       VALUE '/'.
             10 WS-DISP-DATE-MM             PIC XX.
             10 FILLER                      PIC X       VALUE '/'.
             10 WS-DISP-DATE-CC             PIC XX.
             10 WS-DISP-DATE-YY             PIC XX.
      *
      * Flags...
      *
       01 WS-REP-IN-STATUS                  PIC X(2).
          88 REP-IN-OK                      VALUE "00".
          88 REP-IN-EOF                     VALUE "10".
          88 REP-IN-VALID                   VALUE "00", "10".
       01 WS-DONE-TOTALS                    PIC X(5)    VALUE 'TRUE'.
      *
       PROCEDURE DIVISION.
      *
       PROGRAM-CONTROL.
           PERFORM 1000-INITIAL-PROCESS

           PERFORM 2000-MAIN-PROCESS UNTIL REP-IN-EOF

           PERFORM 3000-END-PROCESS

           STOP RUN.

       1000-INITIAL-PROCESS.
           INITIALIZE WS-VARIABLES
               REPLACING ALPHANUMERIC BY SPACES
               NUMERIC BY ZEROS

           ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE-YY TO WS-DISP-DATE-YY
           MOVE WS-DATE-MM TO WS-DISP-DATE-MM
           MOVE WS-DATE-DD TO WS-DISP-DATE-DD
           MOVE WS-DATE-CC TO WS-DISP-DATE-CC

           PERFORM 4000-OPEN-REP-IN

           PERFORM 4100-READ-REP-IN

           MOVE R-REGION TO WS-H5-CODE
           MOVE R-PARTNER TO WS-H4-CODE

           PERFORM 5000-DISPLAY-HEADERS.

       2000-MAIN-PROCESS.
           IF REP-IN-OK
              ADD 1 TO WS-REP-IN-COUNT

              PERFORM 6000-MOVE-CODES

              PERFORM 6100-CHECK-TOTALS

              PERFORM 6200-INCREMENTS

              PERFORM 6300-CHECK-PAGE-COUNT

              PERFORM 5100-DISPLAY-RECORDS
           END-IF

           PERFORM 4100-READ-REP-IN.

       3000-END-PROCESS.
           PERFORM 4200-CLOSE-REP-IN

           PERFORM 5200-DISPLAY-REGION-TOTALS.

           PERFORM 5300-DISPLAY-PARTNER-TOTALS.

       4000-OPEN-REP-IN.
           OPEN INPUT REP-IN.
           IF NOT REP-IN-OK
              DISPLAY '** REP-IN FILE IS NOT OK **'
              DISPLAY '** REP-IN: ' WS-REP-IN-STATUS
              PERFORM 9999-ABEND
           END-IF.

       4100-READ-REP-IN.
           IF NOT REP-IN-EOF
               READ REP-IN
               IF NOT REP-IN-OK AND NOT REP-IN-EOF
                  DISPLAY '** REP-IN FILE IS NOT OK **'
                  DISPLAY '** READ REP-IN: ' REPIN-RECORD
                  PERFORM 9999-ABEND
               END-IF
           END-IF.

       4200-CLOSE-REP-IN.
           CLOSE REP-IN.
           IF NOT REP-IN-OK
              DISPLAY '** COULD NOT CLOSE REP-IN **'
              PERFORM 9999-ABEND
           END-IF.

       5000-DISPLAY-HEADERS.
           MOVE WS-DISPLAY-DATE TO WS-H1-DATE

           MOVE WS-PAGE-COUNT TO WS-H1-PAGE-NUM

           DISPLAY WS-HEADER1-MSG
           DISPLAY WS-HEADER2-MSG
           DISPLAY WS-HEADER3-MSG
           DISPLAY WS-HEADER4-MSG
           DISPLAY WS-HEADER5-MSG
           DISPLAY WS-HEADER6-MSG.

       5100-DISPLAY-RECORDS.
           MOVE R-AREA TO WS-REC-AREA
           MOVE R-BRANCH TO WS-REC-BRANCH
           MOVE R-ITEM TO WS-REC-ITEM
           MOVE R-UNITS TO WS-REC-UNITS
           MOVE R-COST-VALUE TO WS-REC-COST-VALUE
           MOVE R-RETAIL-VALUE TO WS-REC-RETAIL-VALUE

           DISPLAY WS-REC-OUT.

       5200-DISPLAY-REGION-TOTALS.
           MOVE WS-R-ITEM-COUNT TO WS-R-ITEMS
           MOVE WS-R-UNIT-COUNT TO WS-R-UNITS
           MOVE WS-R-COST-VALUE-COUNT TO WS-R-COST-VALUES
           MOVE WS-R-RETAIL-VALUE-COUNT TO WS-R-RETAIL-VALUES

           MOVE ZEROS TO WS-REGION-COUNTS

           DISPLAY WS-REGION-TOTALS.

       5300-DISPLAY-PARTNER-TOTALS.
           MOVE WS-P-ITEM-COUNT TO WS-P-ITEMS
           MOVE WS-P-UNIT-COUNT TO WS-P-UNITS
           MOVE WS-P-COST-VALUE-COUNT TO WS-P-COST-VALUES
           MOVE WS-P-RETAIL-VALUE-COUNT TO WS-P-RETAIL-VALUES

           MOVE ZEROS TO WS-PARTNER-COUNTS

           DISPLAY WS-PARTNER-TOTALS.

       6000-MOVE-CODES.
           IF WS-REP-IN-COUNT = 1
              MOVE R-REGION TO WS-CUR-REGION
              MOVE R-PARTNER TO WS-CUR-PARTNER
           END-IF
           MOVE WS-CUR-REGION TO WS-PREV-REGION
           MOVE R-REGION TO WS-CUR-REGION WS-H5-CODE
           MOVE WS-CUR-PARTNER TO WS-PREV-PARTNER
           MOVE R-PARTNER TO WS-CUR-PARTNER WS-H4-CODE.

       6100-CHECK-TOTALS.
           IF WS-CUR-PARTNER IS NOT EQUAL TO WS-PREV-PARTNER
              MOVE 'TRUE' TO WS-DONE-TOTALS
              PERFORM 5200-DISPLAY-REGION-TOTALS
              PERFORM 5300-DISPLAY-PARTNER-TOTALS
              DISPLAY ' '
              PERFORM 5000-DISPLAY-HEADERS
           ELSE MOVE 'FALSE' TO WS-DONE-TOTALS
           END-IF.

           IF WS-DONE-TOTALS = 'FALSE'
              IF WS-CUR-REGION IS NOT EQUAL TO WS-PREV-REGION
                 PERFORM 5200-DISPLAY-REGION-TOTALS
                 DISPLAY ' '
                 PERFORM 5000-DISPLAY-HEADERS
              END-IF
           END-IF.

       6200-INCREMENTS.
           PERFORM 6210-INCREMENT-REGIONS
           PERFORM 6220-INCREMENT-PARTNERS.

       6210-INCREMENT-REGIONS.
           ADD 1 TO WS-R-ITEM-COUNT
           ADD R-UNITS TO WS-R-UNIT-COUNT
           ADD R-COST-VALUE TO WS-R-COST-VALUE-COUNT
           ADD R-RETAIL-VALUE TO WS-R-RETAIL-VALUE-COUNT.

       6220-INCREMENT-PARTNERS.
           ADD 1 TO WS-P-ITEM-COUNT
           ADD R-UNITS TO WS-P-UNIT-COUNT
           ADD R-COST-VALUE TO WS-P-COST-VALUE-COUNT
           ADD R-RETAIL-VALUE TO WS-P-RETAIL-VALUE-COUNT.

       6300-CHECK-PAGE-COUNT.
            IF FUNCTION MOD(WS-REP-IN-COUNT, WS-MAX-RECS-PAGE) = 0
               ADD 1 TO WS-PAGE-COUNT
               DISPLAY ' '
               DISPLAY ' '
               PERFORM 5000-DISPLAY-HEADERS
            END-IF.

       9999-ABEND.
           DISPLAY 'PROGRAM ENDED'.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.