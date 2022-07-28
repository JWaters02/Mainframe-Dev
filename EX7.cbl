       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EX7.
       AUTHOR.       Joshua Waters.
       DATE-WRITTEN. 28/07/22.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT CMP1-IN ASSIGN TO CMP1
           FILE STATUS IS WS-CMP1-IN-STATUS.
           SELECT CMP2-IN ASSIGN TO CMP2
           FILE STATUS IS WS-CMP2-IN-STATUS.
           SELECT CMP-OUT ASSIGN TO CMPOUT
           FILE STATUS IS WS-CMP-OUT-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD CMP1-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 6 CHARACTERS.
       01 CMP1-RECORD.
           05 CMP1-ITEM                      PIC X(6).
      *
       FD CMP2-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 13 CHARACTERS.
       01 CMP2-RECORD.
           05 CMP2-ITEM                      PIC X(6).
           05 CMP2-OPTION                    PIC X(2).
           05 CMP2-PRICE                     PIC 9(3)V99.
      *
       FD CMP-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 13 CHARACTERS.
       01 CMP-OUT-RECORD.
           05 CMP-ITEM                       PIC X(6).
           05 CMP-BLCK                       PIC X(7).
           05 CMP-BLCK-DATA
           REDEFINES CMP-BLCK.
                07 CMP-OPTION                PIC X(2).
                07 CMP-PRICE                 PIC 9(3)V99.
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-CMP1-IN-STATUS                  PIC X(2).
           88 CMP1-IN-OK                     VALUE "00".
           88 CMP1-IN-EOF                    VALUE "10".
           88 CMP1-IN-VALID                  VALUE "00", "10".
       01 WS-CMP2-IN-STATUS                  PIC X(2).
           88 CMP2-IN-OK                     VALUE "00".
           88 CMP2-IN-EOF                    VALUE "10".
           88 CMP2-IN-VALID                  VALUE "00", "10".
       01 WS-CMP-OUT-STATUS                  PIC X(2).
           88 CMP-OUT-OK                     VALUE "00".
           88 CMP-OUT-EOF                    VALUE "10".
           88 CMP-OUT-VALID                  VALUE "00", "10".
      *
       01 WS-CMP1-IN-COUNT                   PIC 9(5) VALUE 0.
       01 WS-CMP2-IN-COUNT                   PIC 9(5) VALUE 0.
       01 WS-CMP-OUT-COUNT                   PIC 9(5) VALUE 0.
      *
       PROCEDURE DIVISION.
      *
       PROGRAM-CONTROL.
           PERFORM 1000-INITIAL-PROCESS

           PERFORM 2000-MAIN-PROCESS
              UNTIL CMP1-IN-EOF AND CMP2-IN-EOF

           PERFORM 3000-END-PROCESS

           STOP RUN.

       1000-INITIAL-PROCESS.
           PERFORM 6110-OPEN-CMP1-IN

           PERFORM 6120-READ-CMP1-IN

           PERFORM 6210-OPEN-CMP2-IN

           PERFORM 6220-READ-CMP2-IN

           PERFORM 6300-OPEN-CMP-OUT.

       2000-MAIN-PROCESS.
           EVALUATE TRUE
              WHEN CMP1-ITEM = CMP2-ITEM
                 PERFORM 4000-WRITE-CMP2-LAYOUT
                 PERFORM 6120-READ-CMP1-IN
                 PERFORM 6220-READ-CMP2-IN
                 DISPLAY 'MATCHING KEYS'
              WHEN CMP1-ITEM > CMP2-ITEM
                 PERFORM 4000-WRITE-CMP2-LAYOUT
                 PERFORM 6220-READ-CMP2-IN
                 DISPLAY 'FOUND IN ONLY CMP1'
              WHEN CMP1-ITEM < CMP2-ITEM
                 PERFORM 4100-WRITE-CMP1-LAYOUT
                 PERFORM 6120-READ-CMP1-IN
                 DISPLAY 'FOUND IN ONLY CMP2'
           END-EVALUATE.

       3000-END-PROCESS.
           PERFORM 7000-CLOSE-CMP1-IN

           PERFORM 7100-CLOSE-CMP2-IN

           PERFORM 7100-CLOSE-CMP-OUT

           DISPLAY 'CMP1 IN COUNT: ' WS-CMP1-IN-COUNT.
           DISPLAY 'CMP2 IN COUNT: ' WS-CMP2-IN-COUNT.
           DISPLAY 'CMP OUT COUNT: ' WS-CMP-OUT-COUNT.

       4000-WRITE-CMP2-LAYOUT.
           MOVE CMP2-ITEM TO CMP-ITEM
           MOVE CMP2-OPTION TO CMP-OPTION
           MOVE CMP2-PRICE TO CMP-PRICE
           WRITE CMP-OUT-RECORD.

       4100-WRITE-CMP1-LAYOUT.
           MOVE CMP1-ITEM TO CMP-ITEM
           MOVE SPACES TO CMP-BLCK
           WRITE CMP-OUT-RECORD.

       6110-OPEN-CMP1-IN.
           OPEN INPUT CMP1-IN.
           IF NOT CMP1-IN-OK
              DISPLAY '** CMP1-IN FILE IS NOT OK **'
              DISPLAY '** CMP1-IN: ' WS-CMP1-IN-STATUS
              PERFORM 9999-ABEND
           END-IF.

       6120-READ-CMP1-IN.
           IF NOT CMP1-IN-EOF
               READ CMP1-IN
               IF NOT CMP1-IN-OK AND NOT CMP1-IN-EOF
                  DISPLAY '** CMP1-IN FILE IS NOT OK **'
                  DISPLAY '** READ CMP1-IN: ' CMP1-RECORD
                  PERFORM 9999-ABEND
               ELSE
                  IF CMP1-IN-OK
                     ADD 1 TO WS-CMP1-IN-COUNT
                  END-IF
               END-IF
               IF CMP1-IN-EOF
                  MOVE HIGH-VALUES TO CMP1-ITEM
               END-IF
           END-IF.

       6210-OPEN-CMP2-IN.
           OPEN INPUT CMP2-IN.
           IF NOT CMP2-IN-OK
              DISPLAY '** CMP2-IN FILE IS NOT OK **'
              DISPLAY '** CMP2-IN: ' CMP2-RECORD
              PERFORM 9999-ABEND
           END-IF.

       6220-READ-CMP2-IN.
           IF NOT CMP2-IN-EOF
               READ CMP2-IN
               IF NOT CMP2-IN-OK AND NOT CMP2-IN-EOF
                  DISPLAY '** CMP2-IN FILE IS NOT OK **'
                  DISPLAY '** READ CMP2-IN: ' CMP2-RECORD
                  PERFORM 9999-ABEND
               ELSE
                  IF CMP2-IN-OK
                     ADD 1 TO WS-CMP2-IN-COUNT
                  END-IF
               END-IF
               IF CMP2-IN-EOF
                  MOVE HIGH-VALUES TO CMP2-ITEM
               END-IF
           END-IF.

       6300-OPEN-CMP-OUT.
           OPEN OUTPUT CMP-OUT.
           IF NOT CMP-OUT-OK
              DISPLAY '** CMP-OUT FILE IS NOT OK **'
              DISPLAY '** CMP-OUT: ' CMP-OUT-RECORD
              PERFORM 9999-ABEND
           END-IF.

       7000-CLOSE-CMP1-IN.
           CLOSE CMP1-IN.
           IF NOT CMP1-IN-OK
              DISPLAY '** COULD NOT CLOSE CMP2-IN **'
              PERFORM 9999-ABEND
           END-IF.

       7100-CLOSE-CMP2-IN.
           CLOSE CMP2-IN.
           IF NOT CMP2-IN-OK
              DISPLAY '** COULD NOT CLOSE CMP2-IN **'
              PERFORM 9999-ABEND
           END-IF.

       7100-CLOSE-CMP-OUT.
           CLOSE CMP-OUT.
           IF NOT CMP-OUT-OK
              DISPLAY '** COULD NOT CLOSE CMP-OUT **'
              PERFORM 9999-ABEND
           END-IF.

       9999-ABEND.
           DISPLAY 'PROGRAM ENDED'.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.
