       IDENTIFICATION DIVISION.
       PROGRAM-ID.   REPORT5.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 11/08/22.
      *----------------------------------------------------------------*
      *                                                                *
      *  Project     : REPORT 5                                        *
      *                                                                *
      *  Function    : DISPLAY RECORDS WITH:                           *
      *                - PAGE                                          *
      *                - REGION                                        *
      *                - PARTNER                                       *
      *                - BRANCH                                        *
      *                - AREA                                          *
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
           SELECT REP-IN ASSIGN TO REP5IN
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
             10 FILLER                      PIC X(19).
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
          05 WS-PREV-BRANCH                 PIC X(4).
          05 WS-CUR-BRANCH                  PIC X(4).
          05 WS-PREV-AREA                   PIC X(3).
          05 WS-CUR-AREA                    PIC X(3).    
      *
      * Constants...
      *
       01 WS-MAX-RECS-PAGE                  PIC 99      VALUE 20.
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
       01 WS-BRANCH-COUNTS.
          05 WS-B-ITEM-COUNT                PIC 9(9)    VALUE 0.
          05 WS-B-UNIT-COUNT                PIC 9(9)    VALUE 0.
          05 WS-B-COST-VALUE-COUNT          PIC 9(9)V99 VALUE 0.
          05 WS-B-RETAIL-VALUE-COUNT        PIC 9(9)V99 VALUE 0.
       01 WS-AREA-COUNTS.
          05 WS-A-ITEM-COUNT                PIC 9(9)    VALUE 0.
          05 WS-A-UNIT-COUNT                PIC 9(9)    VALUE 0.
          05 WS-A-COST-VALUE-COUNT          PIC 9(9)V99 VALUE 0.
          05 WS-A-RETAIL-VALUE-COUNT        PIC 9(9)V99 VALUE 0.        
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
             10 WS-H2-TITLE                 PIC X(42)   VALUE
                '"DISPLAY RECORDS WITH PAGE, AREA, BRANCH, '.
             10 WS-H2-TITLE2                PIC X(25)   VALUE 
                'PARTNER AND REGION BREAK"'.
          05 WS-HEADER3-MSG.
             10 FILLER                      PIC X(92)   VALUE SPACES.
          05 WS-HEADER4-MSG.
             10 WS-H4-A-TITLE               PIC X(6)    VALUE 
                'AREA: '.
             10 WS-H4-A-CODE                PIC X(3).
             10 FILLER                      PIC X(3)    VALUE SPACES.
             10 WS-H4-B-TITLE               PIC X(8)    VALUE 
                'BRANCH: '.
             10 WS-H4-B-CODE                PIC X(4).
             10 FILLER                      PIC X(3)    VALUE SPACES.
             10 WS-H4-P-TITLE               PIC X(9)    VALUE 
                'PARTNER: '.
             10 WS-H4-P-CODE                PIC X(2).
             10 FILLER                      PIC X(3)    VALUE SPACES.
             10 WS-H4-R-TITLE               PIC X(8)    VALUE 
                'REGION: '.
             10 WS-H4-R-CODE                PIC X(2).
          05 WS-HEADER5-MSG.
             10 FILLER                      PIC X(20)   VALUE SPACES.
             10 WS-H5-ITEM                  PIC X(4)    VALUE 'ITEM'.
             10 FILLER                      PIC X(8)    VALUE SPACES.
             10 WS-H5-UNITS                 PIC X(5)    VALUE 'UNITS'.
             10 FILLER                      PIC X(6)    VALUE SPACES.
             10 WS-H5-COST-VALUE            PIC X(10)   VALUE
                'COST-VALUE'.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-H5-RETAIL-VALUE          PIC X(12)   VALUE
                'RETAIL-VALUE'.
             10 FILLER                      PIC X       VALUE SPACES.
          05 WS-TOTALS-OUT.
             10 WS-T-TEXT                   PIC X(9)    VALUE
                'TOTAL OF '.
             10 WS-T-TYPE                   PIC X(7)    VALUE SPACES.
             10 FILLER                      PIC X(3)    VALUE SPACES.
             10 WS-T-ITEMS                  PIC Z(5)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-T-UNITS                  PIC Z(7)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-T-COST-VALUES            PIC Z(7)9.99.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-T-RETAIL-VALUES          PIC Z(7)9.99.
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
       01 WS-DONE-P-TOTALS                  PIC X(5)    VALUE 'TRUE'.
       01 WS-DONE-B-TOTALS                  PIC X(5)    VALUE 'TRUE'.
       01 WS-DONE-A-TOTALS                  PIC X(5)    VALUE 'TRUE'.
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

           PERFORM 4300-INIT-REP-HEADERS

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

           PERFORM 5210-DISPLAY-REGION-TOTALS

           PERFORM 5220-DISPLAY-PARTNER-TOTALS

           PERFORM 5230-DISPLAY-BRANCH-TOTALS

           PERFORM 5240-DISPLAY-AREA-TOTALS.

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

       4300-INIT-REP-HEADERS.
           MOVE R-REGION TO WS-H4-R-CODE 
           MOVE R-PARTNER TO WS-H4-P-CODE 
           MOVE R-AREA TO WS-H4-A-CODE
           MOVE R-BRANCH TO WS-H4-B-CODE.

       5000-DISPLAY-HEADERS.
           MOVE WS-DISPLAY-DATE TO WS-H1-DATE

           MOVE WS-PAGE-COUNT TO WS-H1-PAGE-NUM

           DISPLAY WS-HEADER1-MSG
           DISPLAY WS-HEADER2-MSG
           DISPLAY WS-HEADER3-MSG
           DISPLAY WS-HEADER4-MSG
           DISPLAY WS-HEADER5-MSG.

       5100-DISPLAY-RECORDS.
           MOVE R-ITEM TO WS-REC-ITEM
           MOVE R-UNITS TO WS-REC-UNITS
           MOVE R-COST-VALUE TO WS-REC-COST-VALUE
           MOVE R-RETAIL-VALUE TO WS-REC-RETAIL-VALUE

           DISPLAY WS-REC-OUT.

       5210-DISPLAY-REGION-TOTALS.
           MOVE 'REGION' TO WS-T-TYPE
           MOVE WS-R-ITEM-COUNT TO WS-T-ITEMS 
           MOVE WS-R-UNIT-COUNT TO WS-T-UNITS
           MOVE WS-R-COST-VALUE-COUNT TO WS-T-COST-VALUES
           MOVE WS-R-RETAIL-VALUE-COUNT TO WS-T-RETAIL-VALUES

           MOVE ZEROS TO WS-REGION-COUNTS

           DISPLAY WS-TOTALS-OUT.

       5220-DISPLAY-PARTNER-TOTALS.
           MOVE 'PARTNER' TO WS-T-TYPE
           MOVE WS-P-ITEM-COUNT TO WS-T-ITEMS
           MOVE WS-P-UNIT-COUNT TO WS-T-UNITS
           MOVE WS-P-COST-VALUE-COUNT TO WS-T-COST-VALUES
           MOVE WS-P-RETAIL-VALUE-COUNT TO WS-T-RETAIL-VALUES

           MOVE ZEROS TO WS-PARTNER-COUNTS

           DISPLAY WS-TOTALS-OUT.

       5230-DISPLAY-BRANCH-TOTALS.
           MOVE 'BRANCH' TO WS-T-TYPE
           MOVE WS-B-ITEM-COUNT TO WS-T-ITEMS
           MOVE WS-B-UNIT-COUNT TO WS-T-UNITS
           MOVE WS-B-COST-VALUE-COUNT TO WS-T-COST-VALUES
           MOVE WS-B-RETAIL-VALUE-COUNT TO WS-T-RETAIL-VALUES

           MOVE ZEROS TO WS-BRANCH-COUNTS

           DISPLAY WS-TOTALS-OUT.

       5240-DISPLAY-AREA-TOTALS.
           MOVE 'AREA' TO WS-T-TYPE
           MOVE WS-A-ITEM-COUNT TO WS-T-ITEMS
           MOVE WS-A-UNIT-COUNT TO WS-T-UNITS
           MOVE WS-A-COST-VALUE-COUNT TO WS-T-COST-VALUES
           MOVE WS-A-RETAIL-VALUE-COUNT TO WS-T-RETAIL-VALUES

           MOVE ZEROS TO WS-AREA-COUNTS

           DISPLAY WS-TOTALS-OUT.

       6000-MOVE-CODES.
           IF WS-REP-IN-COUNT = 1
              MOVE R-REGION TO WS-CUR-REGION
              MOVE R-PARTNER TO WS-CUR-PARTNER
              MOVE R-BRANCH TO WS-CUR-BRANCH 
              MOVE R-AREA TO WS-CUR-AREA
           END-IF
           MOVE WS-CUR-REGION TO WS-PREV-REGION
           MOVE R-REGION TO WS-CUR-REGION WS-H4-R-CODE 
           MOVE WS-CUR-PARTNER TO WS-PREV-PARTNER
           MOVE R-PARTNER TO WS-CUR-PARTNER WS-H4-P-CODE
           MOVE WS-CUR-BRANCH TO WS-PREV-BRANCH
           MOVE R-BRANCH TO WS-CUR-BRANCH WS-H4-B-CODE
           MOVE WS-CUR-AREA TO WS-PREV-AREA
           MOVE R-AREA TO WS-CUR-AREA WS-H4-A-CODE.

       6100-CHECK-TOTALS.
           IF WS-CUR-AREA IS NOT EQUAL TO WS-PREV-AREA
              MOVE 'TRUE' TO WS-DONE-A-TOTALS
              PERFORM 5210-DISPLAY-REGION-TOTALS
              PERFORM 5220-DISPLAY-PARTNER-TOTALS
              PERFORM 5230-DISPLAY-BRANCH-TOTALS
              PERFORM 5240-DISPLAY-AREA-TOTALS
              DISPLAY ' '
              PERFORM 5000-DISPLAY-HEADERS
           ELSE MOVE 'FALSE' TO WS-DONE-A-TOTALS
           END-IF.
           
           IF WS-DONE-A-TOTALS = 'FALSE'
              IF WS-CUR-BRANCH IS NOT EQUAL TO WS-PREV-BRANCH
                 MOVE 'TRUE' TO WS-DONE-B-TOTALS
                 PERFORM 5210-DISPLAY-REGION-TOTALS
                 PERFORM 5220-DISPLAY-PARTNER-TOTALS
                 PERFORM 5230-DISPLAY-BRANCH-TOTALS
                 DISPLAY ' '
                 PERFORM 5000-DISPLAY-HEADERS
              ELSE MOVE 'FALSE' TO WS-DONE-B-TOTALS
              END-IF
           END-IF.
           
           IF WS-DONE-B-TOTALS = 'FALSE'
           AND WS-DONE-A-TOTALS = 'FALSE'
              IF WS-CUR-PARTNER IS NOT EQUAL TO WS-PREV-PARTNER
                 MOVE 'TRUE' TO WS-DONE-P-TOTALS
                 PERFORM 5210-DISPLAY-REGION-TOTALS
                 PERFORM 5220-DISPLAY-PARTNER-TOTALS
                 DISPLAY ' '
                 PERFORM 5000-DISPLAY-HEADERS
              ELSE MOVE 'FALSE' TO WS-DONE-P-TOTALS
              END-IF
           END-IF.

           IF WS-DONE-P-TOTALS = 'FALSE' 
           AND WS-DONE-B-TOTALS = 'FALSE'
           AND WS-DONE-A-TOTALS = 'FALSE'
              IF WS-CUR-REGION IS NOT EQUAL TO WS-PREV-REGION
                 PERFORM 5210-DISPLAY-REGION-TOTALS
                 DISPLAY ' '
                 PERFORM 5000-DISPLAY-HEADERS
              END-IF
           END-IF.

       6200-INCREMENTS.
           PERFORM 6210-INCREMENT-REGIONS
           PERFORM 6220-INCREMENT-PARTNERS
           PERFORM 6230-INCREMENT-BRANCHES
           PERFORM 6240-INCREMENT-AREAS.

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

       6230-INCREMENT-BRANCHES.
           ADD 1 TO WS-B-ITEM-COUNT
           ADD R-UNITS TO WS-B-UNIT-COUNT
           ADD R-COST-VALUE TO WS-B-COST-VALUE-COUNT
           ADD R-RETAIL-VALUE TO WS-B-RETAIL-VALUE-COUNT.

       6240-INCREMENT-AREAS.
           ADD 1 TO WS-A-ITEM-COUNT
           ADD R-UNITS TO WS-A-UNIT-COUNT
           ADD R-COST-VALUE TO WS-A-COST-VALUE-COUNT
           ADD R-RETAIL-VALUE TO WS-A-RETAIL-VALUE-COUNT.

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