       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM2.
       AUTHOR. JACOB SHUMATE.
      ****************************************************************
      *This program creates a sales report for ASHRALS LTD.
     ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE    
               ASSIGN TO 'PR2FA20.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

     

           SELECT REPORT-FILE 
             ASSIGN TO PRINTER 'OUTPUT.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD    INPUT-FILE.

       01    INPUT-REC.
         05  CUSTOMER-ID                 PIC 9(5).
         05  CUSTOMER-NAME               PIC X(25).
         05  PROD-ID                     PIC X(3).
         05                              PIC X(5).
         05  PROD-NAME                   PIC X(14).
         05  QTY-SOLD                    PIC 9(3).
         05  COST-PER-ITEM               PIC 999V99.

       FD    REPORT-FILE.
       01    REPORT-LINE    PIC X(80).

       WORKING-STORAGE SECTION.
       01    WS-WORK-AREAS.
             05    ARE-THERE-MORE-RECORDS    PIC X(3) VALUE 'YES'.

       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC 9       VALUE 1.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  FIRST-RECORD                PIC X(3)    VALUE 'YES'.
           05  CUSTOMER-ID-HOLD            PIC 9(5).
           05  FIRST-GROUP-LINE                        VALUE 'YES'.
      *
       01 CALC-FIELDS.
           05  CALC-QTY-SOLD-TOTAL           PIC 9(4).
           05  CALC-SVALUE                   PIC 9(6)V99.
           05  CALC-SVALUE-TOTAL             PIC 9(6)V99.
       01 TOTAL-FIELDS.
           05  GTL-QUANTITY-SOLD           PIC 9(4).
           05  GTL-SALES-VALUE             PIC 9(7)V99.
      *************************OUTPUT AREA********************************
       01  HEADING-ONE.
           05                              PIC X(35) VALUE SPACES.
           05                              PIC X(25) VALUE 'ASHRAL LTD'.
      *
       01  HEADING-TWO.
           05  FILLER                      PIC X(10) VALUE SPACES.
           05  H1-DATE.
               10  H1-MONTH                PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(7) VALUE SPACES.
           05                              PIC X(30) VALUE
                                           'SALES SPECULATION REPORT'.
           05 FILLER                       PIC X(10) VALUE SPACES.
           05                              PIC X(4) VALUE 'JHS'.
      *
       01  HEADING-THREE.
           05  FILLER                      PIC X(27) VALUE SPACES.
           05                              PIC X(4) VALUE 'PROD'.
           05 FILLER                       PIC X(4) VALUE SPACES.
           05                              PIC X(7) VALUE 'PRODUCT'.
           05 FILLER                       PIC X(8) VALUE SPACES.
           05                              PIC X(3) VALUE 'QTY'.
           05 FILLER                       PIC X(9) VALUE SPACES.
           05                              PIC X(5) VALUE 'SALES'.
      *
       01  HEADING-FOUR.
           05  FILLER                      PIC X(5) VALUE SPACES.
           05                              PIC X(8) VALUE 'CUSTOMER'.
           05 FILLER                       PIC X(1) VALUE SPACES. 
           05                              PIC X(4) VALUE 'NAME'.
           05 FILLER                       PIC X(10) VALUE SPACES.
           05                              PIC X(2) VALUE 'ID'.
           05 FILLER                       PIC X(7) VALUE SPACES.
           05                              PIC X(4) VALUE 'NAME'.
           05 FILLER                       PIC X(9) VALUE SPACES.
           05                              PIC X(4) VALUE 'SOLD'.
           05 FILLER                       PIC X(8) VALUE SPACES.
           05                              PIC X(5) VALUE 'VALUE'.     
       01  DETAIL-LINE.
           05  DL-CUSTOMER-NAME                PIC X(25).
           05                                  PIC X(3) VALUE SPACES.
           05 DL-PROD-ID                       PIC X(3).
           05                                  PIC X(2) VALUE SPACES.
           05  DL-PROD-NAME                    PIC X(14).
           05                                  PIC X(4) VALUE SPACES.
           05  DL-QTY-SOLD                     PIC ZZZ9.
           05                                  PIC X(5) VALUE SPACES.
           05 DL-SVALUE                        PIC $ZZZ,ZZ9.99.
      *
       01  GROUP-TOTAL-LINE.
           05                                  PIC X(40) VALUE ' '.
           05                                  PIC X(6) VALUE 'TOTAL:'.
           05                                  PIC X(5) VALUE ' '.
           05 QTY-SOLD-TOTAL                   PIC Z,ZZZ.
           05                                  PIC X(5) VALUE ' '.
           05 SVALUE-TOTAL                     PIC $ZZZ,ZZ9.99.
       01  GTL-QS-LINE.
           05                                  PIC X(25) VALUE ' '.
           05             PIC X(26) VALUE 'GRAND TOTAL QUANTITY SOLD:'.
           05 FILLER                           PIC X(10).
           05 GTL-LINE-QUANTITY-SOLD           PIC Z,ZZZ.
      *
       01  GTL-SV-LINE.
           05                                  PIC X(25) VALUE ' '.
           05             PIC X(26) VALUE 'GRAND TOTAL SALES VALUE:'.
           05 FILLER                           PIC X(10).
           05 GTL-LINE-SALES-VALUE             PIC $Z,ZZZ,ZZ9.99.
      *
       PROCEDURE DIVISION.
      *
       100-PRINT-REPORT.
           PERFORM 200-HSKPING-ROUTINE
           PERFORM 300-COLUMN-HEADERS
           PERFORM 400-READ-FILE
           PERFORM 800-END-OF-JOB-ROUTINE
           PERFORM 900-FINAL-ROUTINE
       .

       200-HSKPING-ROUTINE.
           OPEN INPUT  INPUT-FILE
                OUTPUT REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
       .


       300-COLUMN-HEADERS.

           MOVE HEADING-ONE TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE HEADING-TWO TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           .
           MOVE HEADING-THREE TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           .
           MOVE HEADING-FOUR TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           .

       400-READ-FILE.
           PERFORM UNTIL NO-MORE-DATA
               READ INPUT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 500-PROCESS-RECORD
               END-READ
           END-PERFORM

       .

       500-PROCESS-RECORD.
           IF FIRST-RECORD = 'YES'
                  MOVE CUSTOMER-ID TO CUSTOMER-ID-HOLD
                  MOVE 'NO' TO FIRST-RECORD 
           ELSE 
              IF CUSTOMER-ID NOT = CUSTOMER-ID-HOLD
              MOVE 'YES' TO FIRST-GROUP-LINE
              PERFORM 700-CONTROL-BREAK
              END-IF 
           END-IF
           IF FIRST-GROUP-LINE = 'YES'
                  MOVE CUSTOMER-NAME TO DL-CUSTOMER-NAME
                  MOVE PROD-ID TO DL-PROD-ID
                  MOVE PROD-NAME TO DL-PROD-NAME
                  MOVE QTY-SOLD TO DL-QTY-SOLD
                  ADD QTY-SOLD TO CALC-QTY-SOLD-TOTAL
                  MOVE CALC-QTY-SOLD-TOTAL TO QTY-SOLD-TOTAL
                  MULTIPLY QTY-SOLD BY COST-PER-ITEM GIVING CALC-SVALUE
                  ADD CALC-SVALUE TO CALC-SVALUE-TOTAL
                  MOVE CALC-SVALUE-TOTAL TO SVALUE-TOTAL
                  MOVE CALC-SVALUE TO DL-SVALUE
                  MOVE DETAIL-LINE TO REPORT-LINE
                  MOVE 'NO' TO FIRST-GROUP-LINE
                  PERFORM 600-WRITE-A-LINE
                  MOVE 1 TO PROPER-SPACING
            ELSE 
                  MOVE '        ' TO DL-CUSTOMER-NAME
                  MOVE PROD-ID TO DL-PROD-ID
                  MOVE PROD-NAME TO DL-PROD-NAME
                  MOVE QTY-SOLD TO DL-QTY-SOLD
                  ADD QTY-SOLD TO CALC-QTY-SOLD-TOTAL
                  MOVE CALC-QTY-SOLD-TOTAL TO QTY-SOLD-TOTAL
                  MULTIPLY QTY-SOLD BY COST-PER-ITEM GIVING CALC-SVALUE
                  ADD CALC-SVALUE TO CALC-SVALUE-TOTAL
                  MOVE CALC-SVALUE-TOTAL TO SVALUE-TOTAL
                  MOVE CALC-SVALUE TO DL-SVALUE
                  MOVE DETAIL-LINE TO REPORT-LINE
                  PERFORM 600-WRITE-A-LINE
                  MOVE 1 TO PROPER-SPACING
        .

       600-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
           .
       700-CONTROL-BREAK.
           MOVE CUSTOMER-ID TO CUSTOMER-ID-HOLD
           WRITE REPORT-LINE FROM GROUP-TOTAL-LINE
               AFTER ADVANCING 2 LINES
           ADD CALC-QTY-SOLD-TOTAL TO GTL-QUANTITY-SOLD
           ADD CALC-SVALUE-TOTAL TO GTL-SALES-VALUE
           MOVE 0 TO CALC-QTY-SOLD-TOTAL
           MOVE 0 TO CALC-SVALUE-TOTAL
           MOVE 2 TO PROPER-SPACING
           .
       800-END-OF-JOB-ROUTINE.
           PERFORM 700-CONTROL-BREAK
           MOVE GTL-QUANTITY-SOLD TO GTL-LINE-QUANTITY-SOLD
           MOVE GTL-QS-LINE TO REPORT-LINE
           MOVE 3 TO PROPER-SPACING
           PERFORM 600-WRITE-A-LINE
           MOVE GTL-SALES-VALUE TO GTL-LINE-SALES-VALUE
           MOVE GTL-SV-LINE TO REPORT-LINE
           MOVE 3 TO PROPER-SPACING
           PERFORM 600-WRITE-A-LINE
           .
       900-FINAL-ROUTINE.
           CLOSE INPUT-FILE
                 REPORT-FILE
            STOP RUN
            .
