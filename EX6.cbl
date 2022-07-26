       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX6.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 25/07/22.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ITEMINFO-IN ASSIGN TO SORTOUT
           FILE STATUS IS WS-ITEMINFO-IN-STATUS.
           SELECT BRANCH-IN ASSIGN TO BRANCHES
           FILE STATUS IS WS-BRANCH-IN-STATUS.
           SELECT BRANCHINFO-OUT ASSIGN TO BRCHOUT
           FILE STATUS IS WS-BRANCH-OUT-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ITEMINFO-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 22 CHARACTERS.
       01 ITEMINFO-RECORD.
           05 I-ITEM                         PIC X(6).
           05 I-OPTION                       PIC X(2).
           05 I-PARTNER-CODE                 PIC X(2).
           05 I-BRANCH                       PIC X(4).
           05 I-PRICE                        PIC 9(3)V99.
           05 I-QUANTITY                     PIC 9(3).
      *
       FD BRANCH-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 34 CHARACTERS.
       01 BRANCH-RECORD.
           05 BRANCH-NO                      PIC X(4).
           05 BRANCH-DESC                    PIC X(30).
      *
       FD BRANCHINFO-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 42 CHARACTERS.
       01 BRANCHINFO-OUT-RECORD.
           05 ITEM-OUT                       PIC X(6).
           05 OPTION-OUT                     PIC X(2).
           05 BRANCH-OUT                     PIC X(4).
           05 BRANCH-DESC-OUT                PIC X(30).
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-ITEMINFO-IN-STATUS              PIC X(2).
           88 ITEMINFO-IN-OK                 VALUE "00".
           88 ITEMINFO-IN-EOF                VALUE "10".
           88 ITEMINFO-IN-VALID              VALUE "00", "10".
       01 WS-BRANCH-IN-STATUS                PIC X(2).
           88 BRANCH-IN-OK                   VALUE "00".
           88 BRANCH-IN-EOF                  VALUE "10".
           88 BRANCH-IN-VALID                VALUE "00", "10".
       01 WS-BRANCH-OUT-STATUS               PIC X(2).
           88 BRANCH-OUT-OK                  VALUE "00".
           88 BRANCH-OUT-EOF                 VALUE "10".
           88 BRANCH-OUT-VALID               VALUE "00", "10".
      *
       01 WS-BRANCH-MOVE-COUNT               PIC 9(5) VALUE 0.
       01 WS-LAST-BRANCH                     PIC X(4).
       01 WS-BRANCH-DESC                     PIC X(30).
      *
       01 BRANCH-TABLE.
         03 BT-ENTRIES                       PIC 9(4).
         03 BRANCH-TABLE-ENTRY
           OCCURS 1 TO 5000 TIMES
           DEPENDING ON BT-ENTRIES
           ASCENDING KEY IS BT-BRANCH-NO INDEXED BY BT.
           05 BT-BRANCH-NO                   PIC X(4).
           05 BT-BRANCH-DESC                 PIC X(30).
      *
       PROCEDURE DIVISION.
      *
       PROGRAM-CONTROL.
           PERFORM 1000-INITIAL-PROCESS

           PERFORM 2000-BUILD-BRANCH-TABLE UNTIL BRANCH-IN-EOF

           PERFORM 3000-MAIN-PROCESS UNTIL ITEMINFO-IN-EOF

           PERFORM 4000-END-PROCESS

           STOP RUN.

       1000-INITIAL-PROCESS.
           PERFORM 6000-OPEN-BRANCH-IN

           PERFORM 6100-OPEN-ITEMINFO-IN

           PERFORM 6200-OPEN-BRANCHINFO-OUT.

       2000-BUILD-BRANCH-TABLE.
           READ BRANCH-IN.
           IF NOT BRANCH-IN-EOF
              ADD 1 TO BT-ENTRIES
              IF BT-ENTRIES > 5000
                DISPLAY '** BRANCH TABLE FULL **'
                PERFORM 9999-ABEND
              END-IF
              MOVE BRANCH-NO TO
                BT-BRANCH-NO(BT-ENTRIES)
                DISPLAY 'BRANCH-NO:' BRANCH-NO
              MOVE BRANCH-DESC TO
                BT-BRANCH-DESC(BT-ENTRIES)
                DISPLAY 'BRANCH-DESC:' BRANCH-DESC
           DISPLAY 'BT-ENTRIES: ' BT-ENTRIES
           END-IF.

       3000-MAIN-PROCESS.
           READ ITEMINFO-IN.
           IF NOT ITEMINFO-IN-EOF
              IF I-BRANCH NOT = WS-LAST-BRANCH
                 SEARCH ALL BRANCH-TABLE-ENTRY
                    AT END
                       DISPLAY 'BRANCH NOT IN BRANCH TABLE: ' I-BRANCH
                       PERFORM 9999-ABEND
                    WHEN BT-BRANCH-NO(BT) = I-BRANCH
                       MOVE I-BRANCH TO WS-LAST-BRANCH
                       MOVE BT-BRANCH-DESC(BT) TO WS-BRANCH-DESC
                       ADD 1 TO WS-BRANCH-MOVE-COUNT
                 END-SEARCH
              END-IF
              PERFORM 5000-WRITE-BRANCHINFO-OUT
           END-IF.

       4000-END-PROCESS.
           PERFORM 7000-CLOSE-BRANCH-IN

           PERFORM 7100-CLOSE-ITEMINFO-IN

           PERFORM 7200-CLOSE-BRANCHINFO-OUT

           DISPLAY 'BRANCH MOVE COUNT: ' WS-BRANCH-MOVE-COUNT.

       5000-WRITE-BRANCHINFO-OUT.
           MOVE I-ITEM TO ITEM-OUT
           MOVE I-OPTION TO OPTION-OUT
           MOVE I-BRANCH TO BRANCH-OUT
           MOVE WS-BRANCH-DESC TO BRANCH-DESC-OUT
           WRITE BRANCHINFO-OUT-RECORD.

       6000-OPEN-BRANCH-IN.
           OPEN INPUT BRANCH-IN.
           IF NOT BRANCH-IN-OK
              DISPLAY '** BRANCH-IN FILE IS NOT OK **'
              DISPLAY '** BRANCH-IN: ' BRANCH-RECORD
              PERFORM 9999-ABEND
           END-IF.

       6100-OPEN-ITEMINFO-IN.
           OPEN INPUT ITEMINFO-IN.
           IF NOT ITEMINFO-IN-OK
              DISPLAY '** ITEMINFO-IN FILE IS NOT OK **'
              DISPLAY '** ITEMINFO-IN: ' ITEMINFO-RECORD
              PERFORM 9999-ABEND
           END-IF.

       6200-OPEN-BRANCHINFO-OUT.
           OPEN OUTPUT BRANCHINFO-OUT.
           IF NOT BRANCH-OUT-OK
              DISPLAY '** BRANCH-OUT FILE IS NOT OK **'
              DISPLAY '** BRANCH-OUT: ' BRANCH-OUT
              PERFORM 9999-ABEND
           END-IF.

       7000-CLOSE-BRANCH-IN.
           CLOSE BRANCH-IN.
           IF NOT BRANCH-IN-OK
              DISPLAY '** COULD NOT CLOSE ITEMINFO-IN **'
              PERFORM 9999-ABEND
           END-IF.

       7100-CLOSE-ITEMINFO-IN.
           CLOSE ITEMINFO-IN.
           IF NOT ITEMINFO-IN-OK
              DISPLAY '** COULD NOT CLOSE ITEMINFO-IN **'
              PERFORM 9999-ABEND
           END-IF.

       7200-CLOSE-BRANCHINFO-OUT.
           CLOSE BRANCHINFO-OUT.
           IF NOT BRANCH-OUT-OK
              DISPLAY '** COULD NOT CLOSE BRANCHINFO-OUT **'
              PERFORM 9999-ABEND
           END-IF.

       9999-ABEND.
           DISPLAY 'PROGRAM ENDED'.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.