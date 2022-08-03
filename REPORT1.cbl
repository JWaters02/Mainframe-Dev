       IDENTIFICATION DIVISION.
       PROGRAM-ID.   REPORT1.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 01/08/22.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT REP-IN ASSIGN TO REP1IN
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
             10 R-ITEM                     PIC X(06).
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
             10 FILLER                      PIC XX.
             10 WS-R-PARTNER                PIC X(2).
             10 FILLER                      PIC X(9).
             10 WS-R-REGION                 PIC X(02).
             10 FILLER                      PIC X(6).
             10 WS-R-AREA                   PIC X(03).
             10 FILLER                      PIC X(6).
             10 WS-R-BRANCH                 PIC X(04).
             10 FILLER                      PIC X(6).
             10 WS-R-ITEM                   PIC Z(06).
             10 FILLER                      PIC X(7).
             10 WS-R-UNITS                  PIC Z(04)9.
             10 FILLER                      PIC X(7).
             10 WS-R-COST-VALUE             PIC Z(04)9V99.
             10 FILLER                      PIC X(8).
             10 WS-R-RETAIL-VALUE           PIC Z(04)9V99.
             10 FILLER                      PIC X(3).
      *
      * Counters...
      *
       01 WS-PAGE-COUNT                     PIC 9       VALUE 1.
       01 WS-REP-IN-COUNT                   PIC 9(9).
       01 WS-UNIT-COUNT                     PIC 9(9).
       01 WS-COST-VALUE-COUNT               PIC 9(9).
       01 WS-RETAIL-VALUE-COUNT             PIC 9(9).
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
             10 FILLER                      PIC X(59)   VALUE SPACES.
             10 WS-H1-PAGE                  PIC X(4)    VALUE
                'PAGE'.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-H1-PAGE-NUM              PIC 9       VALUE 1.
          05 WS-HEADER2-MSG.
             10 FILLER                      PIC X(28)   VALUE SPACES.
             10 WS-H2-TITLE                 PIC X(41)   VALUE
                'REPORT "DISPLAY RECORDS WITHOUT BREAKS"'.
             10 FILLER                      PIC X(23)   VALUE SPACES.
          05 WS-HEADER3-MSG.
             10 FILLER                      PIC X(92)   VALUE SPACES.
          05 WS-HEADER4-MSG.
             10 WS-H4-PARTNER               PIC X(7)    VALUE 'PARTNER'.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-H4-REGION                PIC X(6)    VALUE 'REGION'.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-H4-AREA                  PIC X(4)    VALUE 'AREA'.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-H4-BRANCH                PIC X(6)    VALUE 'BRANCH'.
             10 FILLER                      PIC X(6)    VALUE SPACES.
             10 WS-H4-ITEM                  PIC X(4)    VALUE 'ITEM'.
             10 FILLER                      PIC X(8)    VALUE SPACES.
             10 WS-H4-UNITS                 PIC X(5)    VALUE 'UNITS'.
             10 FILLER                      PIC X(6)    VALUE SPACES.
             10 WS-H4-COST-VALUE            PIC X(10)   VALUE 
                'COST-VALUE'.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-H4-RETAIL-VALUE          PIC X(12)   VALUE 
                'RETAIL-VALUE'.
             10 FILLER                      PIC X       VALUE SPACES.
          05 WS-TOTALS.   
             10 WS-TEXT                     PIC X(5)    VALUE 'TOTAL'.
             10 FILLER                      PIC X(35)   VALUE SPACES.
             10 WS-ITEMS                    PIC Z(5)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-UNITS                    PIC Z(7)9.
             10 FILLER                      PIC X(4)    VALUE SPACES.
             10 WS-COST-VALUES              PIC Z(7)9V99.
             10 FILLER                      PIC X(5)    VALUE SPACES.
             10 WS-RETAIL-VALUES            PIC Z(7)9V99.
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

           PERFORM 4000-OPEN-REP-IN

           PERFORM 5000-DISPLAY-HEADERS.

       2000-MAIN-PROCESS.
           PERFORM 4100-READ-REP-IN

           PERFORM 5100-DISPLAY-RECORDS.

       3000-END-PROCESS.
           PERFORM 4200-CLOSE-REP-IN

           PERFORM 5200-DISPLAY-TOTALS.

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
               ELSE
                  IF REP-IN-OK
                     ADD 1 TO WS-REP-IN-COUNT
                     ADD R-UNITS TO WS-UNIT-COUNT
                     ADD R-COST-VALUE TO WS-COST-VALUE-COUNT
                     ADD R-RETAIL-VALUE TO WS-RETAIL-VALUE-COUNT
                  END-IF
               END-IF
           END-IF.

       4200-CLOSE-REP-IN.
           CLOSE REP-IN.
           IF NOT REP-IN-OK
              DISPLAY '** COULD NOT CLOSE REP-IN **'
              PERFORM 9999-ABEND
           END-IF.

       5000-DISPLAY-HEADERS.
           ACCEPT WS-DATE FROM DATE
           MOVE WS-DATE-YY TO WS-DISP-DATE-YY
           MOVE WS-DATE-MM TO WS-DISP-DATE-MM
           MOVE WS-DATE-DD TO WS-DISP-DATE-DD
           MOVE WS-DATE-CC TO WS-DISP-DATE-CC
           MOVE WS-DISPLAY-DATE TO WS-H1-DATE

           MOVE WS-PAGE-COUNT TO WS-H1-PAGE-NUM

           DISPLAY WS-HEADER1-MSG
           DISPLAY WS-HEADER2-MSG
           DISPLAY WS-HEADER3-MSG
           DISPLAY WS-HEADER4-MSG.

       5100-DISPLAY-RECORDS.
           MOVE R-PARTNER TO WS-R-PARTNER 
           MOVE R-REGION TO WS-R-REGION
           MOVE R-AREA TO WS-R-AREA 
           MOVE R-BRANCH TO WS-R-BRANCH
           MOVE R-ITEM TO WS-R-ITEM 
           MOVE R-UNITS TO WS-R-UNITS 
           MOVE R-COST-VALUE TO WS-R-COST-VALUE 
           MOVE R-RETAIL-VALUE TO WS-R-RETAIL-VALUE

           DISPLAY WS-REC-OUT.
           
       5200-DISPLAY-TOTALS.
           MOVE WS-REP-IN-COUNT TO WS-ITEMS
           MOVE WS-UNIT-COUNT TO WS-UNITS
           MOVE WS-COST-VALUE-COUNT TO WS-COST-VALUES
           MOVE WS-RETAIL-VALUE-COUNT TO WS-RETAIL-VALUES.

           DISPLAY WS-TOTALS.

       9999-ABEND.
           DISPLAY 'PROGRAM ENDED'.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.