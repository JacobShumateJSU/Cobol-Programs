       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGRAM4.
       AUTHOR.     JACOB SHUMATE.
       
      *****************************************************************
      *                PROGRAM4 ASHRALS LTD MERGED SUMMARY REPORT 
      *   -This program produces a summary report for the vendors and
      *    warehouses containing costume assets for the company ASHRALS
      *    LTD. 
      *    
      *   -This program accomplishes this by receiving three unsorted 
      *    files as input and then merges them into one file used for 
      *    the report. There is a table containing indexes for 
      *    costume sizes that is utilized by a search statement 
      *    to expand the costume size label from a letter to the 
      *    corresponding word. This program also validates and verifies
      *    vendor and warehouse id to ensure that they are inputted 
      *    correctly and if they're not they are submitted into the 
      *    error file.
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            
           SELECT UNSORTED-CH20P4F20
               ASSIGN TO 'UNSORTED-CH20P4F20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UNSORTED-LA10P4F20
               ASSIGN TO 'UNSORTED-LA10P4F20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UNSORTED-NY30P4F20
               ASSIGN TO 'UNSORTED-NY30P4F20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT SORTED-CH20P4F20
               ASSIGN TO 'OUTPUT-SORTED-CH20P4F20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-LA10P4F20
               ASSIGN TO 'OUTPUT-SORTED-LA10P4F20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-NY30P4F20
               ASSIGN TO 'OUTPUT-SORTED-NY30P4F20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE
               ASSIGN TO 'SORTINGFILE.TMP'.
            
           SELECT MERGED-FILE 
               ASSIGN TO 'MERGEDSORTED.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-FILE
               ASSIGN TO PRINTER 'ERROR-FILE.TXT'.

           SELECT SUMMARY-REPORT
                ASSIGN TO PRINTER 'SUMMARY-REPORT.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD    UNSORTED-CH20P4F20
             RECORD CONTAINS 136 CHARACTERS.
       01    UNSORTED-CH20P4F20-RECORD.
             05    U-CH20-VENDOR-ID                 PIC X(4).
             05    U-CH20-WAREHOUSE-ID              PIC X(3).
             05    U-CH20-COSTUME-ID                PIC X(3).
             05    UCH20-COSTUME-DATA OCCURS 6 TIMES.
                   10  UCH20-COSTUME-NAME          PIC X(9).
                   10  UCH20-COSTUME-SIZE          PIC A.
                   10  UCH20-COSTUME-TYPE          PIC A.
                   10  UCH20-QUANTITY-IN-STOCK     PIC 9(3).
                   10  UCH20-REORDER-PT            PIC 9(3).
                   10  UCH20-PURCH-PRICE           PIC 99V99.
       FD    UNSORTED-LA10P4F20
             RECORD CONTAINS 136 CHARACTERS.
       01    UNSORTED-LA10P4F20-RECORD.
             05    U-LA10-VENDOR-ID                 PIC X(4).
             05    U-LA10-WAREHOUSE-ID              PIC X(3).
             05    U-LA10-COSTUME-ID                PIC X(3).
             05    ULA10-COSTUME-DATA OCCURS 6 TIMES.
                   10  ULA10-COSTUME-NAME          PIC X(9).
                   10  ULA10-COSTUME-SIZE          PIC A.
                   10  ULA10-COSTUME-TYPE          PIC A.
                   10  ULA10-QUANTITY-IN-STOCK     PIC 9(3).
                   10  ULA10-REORDER-PT            PIC 9(3).
                   10  ULA10-PURCH-PRICE           PIC 99V99.

       FD    UNSORTED-NY30P4F20
             RECORD CONTAINS 136 CHARACTERS.
       01    UNSORTED-NY30P4F20-RECORD.
             05    U-NY30-VENDOR-ID                 PIC X(4).
             05    U-NY30-WAREHOUSE-ID              PIC X(3).
             05    U-NY30-COSTUME-ID                PIC X(3).
             05    UNY30-COSTUME-DATA OCCURS 6 TIMES.
                   10  UNY30-COSTUME-NAME          PIC X(9).
                   10  UNY30-COSTUME-SIZE          PIC A.
                   10  UNY30-COSTUME-TYPE          PIC A.
                   10  UNY30-QUANTITY-IN-STOCK     PIC 9(3).
                   10  UNY30-REORDER-PT            PIC 9(3).
                   10  UNY30-PURCH-PRICE           PIC 99V99.
       FD    SORTED-CH20P4F20
             RECORD CONTAINS 136 CHARACTERS.
       01    SORTED-CH20P4F20-RECORD.
             05    S-CH20-VENDOR-ID                 PIC X(4).
             05    S-CH20-WAREHOUSE-ID              PIC X(3).
             05    S-CH20-COSTUME-ID                PIC X(3).
             05    SCH20-COSTUME-DATA OCCURS 6 TIMES.
                   10  SCH20-COSTUME-NAME          PIC X(9).
                   10  SCH20-COSTUME-SIZE          PIC A.
                   10  SCH20-COSTUME-TYPE          PIC A.
                   10  SCH20-QUANTITY-IN-STOCK     PIC 9(3).
                   10  SCH20-REORDER-PT            PIC 9(3).
                   10  SCH20-PURCH-PRICE           PIC 99V99.
       FD    SORTED-LA10P4F20
             RECORD CONTAINS 136 CHARACTERS.
       01    SORTED-LA10P4F20-RECORD.
             05    S-LA10-VENDOR-ID                 PIC X(4).
             05    S-LA10-WAREHOUSE-ID              PIC X(3).
             05    S-LA10-COSTUME-ID                PIC X(3).
             05    SLA10-COSTUME-DATA OCCURS 6 TIMES.
                   10  SLA10-COSTUME-NAME          PIC X(9).
                   10  SLA10-COSTUME-SIZE          PIC A.
                   10  SLA10-COSTUME-TYPE          PIC A.
                   10  SLA10-QUANTITY-IN-STOCK     PIC 9(3).
                   10  SLA10-REORDER-PT            PIC 9(3).
                   10  SLA10-PURCH-PRICE           PIC 99V99.
       FD    SORTED-NY30P4F20
             RECORD CONTAINS 136 CHARACTERS.
       01    SORTED-NY30P4F20-RECORD.
             05    S-NY30-VENDOR-ID                 PIC X(4).
             05    S-NY30-WAREHOUSE-ID              PIC X(3).
             05    S-NY30-COSTUME-ID                PIC X(3).
             05    SNY30-COSTUME-DATA OCCURS 6 TIMES.
                   10  SNY30-COSTUME-NAME          PIC X(9).
                   10  SNY30-COSTUME-SIZE          PIC A.
                   10  SNY30-COSTUME-TYPE          PIC A.
                   10  SNY30-QUANTITY-IN-STOCK     PIC 9(3).
                   10  SNY30-REORDER-PT            PIC 9(3).
                   10  SNY30-PURCH-PRICE           PIC 99V99.
       SD    SORT-FILE
             RECORD CONTAINS 136 CHARACTERS.
       01    SORT-RECORD.
             05    SORT-VENDOR-ID                 PIC X(4).
             05    SORT-WAREHOUSE-ID              PIC X(3).
             05    SORT-COSTUME-ID                PIC X(3).
             05    SORT-COSTUME-DATA OCCURS 6 TIMES.
                   10  SORT-COSTUME-NAME          PIC X(9).
                   10  SORT-COSTUME-SIZE          PIC A.
                   10  SORT-COSTUME-TYPE          PIC A.
                   10  SORT-QUANTITY-IN-STOCK     PIC 9(3).
                   10  SORT-REORDER-PT            PIC 9(3).
                   10  SORT-PURCH-PRICE           PIC 99V99.
       FD    MERGED-FILE
             RECORD CONTAINS 136 CHARACTERS.
       01    MERGED-RECORD.
             05    M-VENDOR-ID                 PIC X(4).
             05    M-WAREHOUSE-ID              PIC X(3).
             05    M-COSTUME-ID                PIC X(3).
             05    M-COSTUME-DATA OCCURS 6 TIMES.
                   10  M-COSTUME-NAME          PIC X(9).
                   10  M-COSTUME-SIZE          PIC A.
                   10  M-COSTUME-TYPE          PIC A.
                   10  M-QUANTITY-IN-STOCK     PIC 9(3).
                   10  M-REORDER-PT            PIC 9(3).
                   10  M-PURCH-PRICE           PIC 99V99.

       FD  SUMMARY-REPORT
           RECORD CONTAINS 70 CHARACTERS.

       01  REPORT-LINE                  PIC X(70).

       FD  ERROR-FILE
           RECORD CONTAINS 136 CHARACTERS.

       01  ERROR-FILE-RECORD            PIC X(136).     


       WORKING-STORAGE SECTION.
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X        VALUE 'Y'.
               88  MORE-RECORDS                     VALUE 'Y'.
               88  NO-MORE-RECORDS                  VALUE 'N'.
           05  FIRST-RECORD            PIC X(3)     VALUE 'YES'.
           05  FIRST-NAME              PIC X(3)     VALUE 'YES'.

       01  HOLD-FIELDS.
           05  HF-VENDOR-HOLD          PIC X(4)     VALUE ' '.
           05  HF-WAREHOUSE-HOLD       PIC X(3)     VALUE ' '.
           05  HF-COSTUME-HOLD         PIC X(3)     VALUE ' '.

       01  CONSTANT-FIELDS.
           05  CF-INVALID              PIC X(8)  VALUE 'INVALID-'.
           05  CF-BAD                  PIC X(4)  VALUE 'BAD-'.

       01  DETAIL-FIELDS.
           05  DF-CCOST-TOT            PIC 9(5)V99  VALUE 0.

       01  TOTAL-FIELDS.
           05  TF-GRAND-TOT            PIC 9(9)V99  VALUE 0.
           05  TF-VENDOR-TOT           PIC 9(8)V99  VALUE 0.
           05  TF-WAREHOUSE-TOT        PIC 9(7)V99  VALUE 0.
           05  TF-COSTUME-TOT          PIC 9(6)V99  VALUE 0.

       01 SUBSCRIPTS.
           05  SUB                     PIC 99       VALUE 1.

       01  REPORT-FIELDS.
           05  PROPER-SPACING          PIC 9        VALUE 1.

       01  CURRENT-DATE.
           05  RUN-YEAR                PIC 9(4) VALUE 0.
           05  RUN-MONTH               PIC 99   VALUE 0.
           05  RUN-DAY                 PIC 99   VALUE 0.

       01 COSTUME-SIZE-TEXT.
          05        PIC X(20) VALUE "LARGE".
          05        PIC X(20) VALUE "MEDIUM".
          05        PIC X(20) VALUE "SMALL".
          05        PIC X(20) VALUE "PLUS".

       01 SIZE-TABLE REDEFINES COSTUME-SIZE-TEXT.
          05 COSTUME-ITEM OCCURS 10 TIMES
             INDEXED BY COSTUME-SIZE-INDEX.
             10 L           PIC X(20).
             10 M           PIC X(20).
             10 S           PIC X(20).
             10 P           PIC X(20).  

      ********************REPORT SECTION***********************
       01  REPORT-HEADER1.
           05                                PIC X(33).
           05                                PIC X(11)
                           VALUE 'ASHRALS LTD'.

       01  REPORT-HEADER2.
           05                                PIC X(10).
           05    H1-DATE.
                 10    H1-MONTH              PIC 99.
                 10                          PIC X    VALUE '/'.
                 10    H1-DAY                PIC 99.
                 10                          PIC X    VALUE '/'.
                 10    H1-YEAR               PIC 9(4).
           05                                PIC X(12).
           05                                PIC X(16)
                           VALUE 'SUMMARY REPORT'.
           05                                PIC X(21).
           05                                PIC A(3) VALUE 'JHS'.

       01  VENDOR-HEADER.
           05                       PIC X(5).
           05                       PIC X(8) VALUE 'VENDOR: '.
           05  VH-VENDOR-NAME       PIC X(13).

       01  WAREHOUSE-HEADER.
           05                       PIC X(2).
           05                       PIC X(11) VALUE 'WAREHOUSE: '.
           05  WH-WAREHOUSE-NAME    PIC X(11).

       01  COSTUME-HEADER-1.
           05                                PIC X(11).
           05                                PIC A(7) VALUE 'COSTUME'.
           05                                PIC X(7).
           05                                PIC A(7) VALUE 'COSTUME'.
           05                                PIC X(4).
           05                                PIC A(7) VALUE 'COSTUME'.
           05                                PIC X(3).
           05                                PIC A(6) VALUE 'QTY IN'.
           05                                PIC X(5).
           05                                PIC A(5) VALUE 'TOTAL'.

       01  COSTUME-HEADER-2.
           05                                PIC X(13).
           05                                PIC A(4) VALUE 'NAME'.
           05                                PIC X(9).
           05                                PIC A(4) VALUE 'SIZE'.
           05                                PIC X(7).
           05                                PIC A(4) VALUE 'TYPE'.
           05                                PIC X(5).
           05                                PIC A(5) VALUE 'STOCK'.
           05                                PIC X(6).
           05                                PIC A(4) VALUE 'COST'.

       01  DETAIL-LINE.
           05                                PIC X(10).
           05    DL-NAME                     PIC A(9).
           05                                PIC X(5).
           05    DL-SIZE                     PIC X(8).
           05                                PIC X(5).
           05    DL-TYPE                     PIC X(5).
           05                                PIC X(5).
           05    DL-QTY-IN-STOCK             PIC ZZ9.
           05                                PIC X(4).
           05    DL-TOTAL-COST               PIC $ZZ,ZZ9.99.

       01  COSTUME-TOTAL-LINE.
           05                               PIC X(43).
           05                               PIC X(6)   VALUE 'TOTAL:'.
           05                               PIC X(4).
           05  CTL-COSTUME-TOTAL            PIC $ZZZ,ZZ9.99.

       01  WAREHOUSE-TOTAL-LINE.
           05                                PIC X(14).
           05      PIC X(20) VALUE 'TOTAL FOR WAREHOUSE:'.
           05                                PIC X(2) VALUE SPACES.
           05  WTL-WAREHOUSE-NAME            PIC X(11).
           05                                PIC X(4).
           05  WTL-WAREHOUSE-TOTAL           PIC $Z,ZZZ,ZZ9.99.

       01  VENDOR-TOTAL-LINE.
           05                                PIC X(17).
           05                                PIC X(17) 
                                      VALUE 'TOTAL FOR VENDOR:'.
           05                                PIC X(2).
           05  VTL-VENDOR-NAME               PIC X(13).
           05                                PIC X(1).
           05  VTL-VENDOR-TOTAL              PIC $ZZ,ZZZ,ZZ9.99.

       01  GRAND-TOTAL-LINE.
           05                                PIC X(29).
           05                                PIC X(17) 
                                      VALUE 'GRAND TOTAL COST:'.
           05                                PIC X(3).
           05    GTL-GRAND-TOTAL             PIC $ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       100-MAIN-MODULE.
           PERFORM 150-SORTMERGE-INPUT-FILES
           PERFORM 125-HOUSEKEEPING
           PERFORM 150-READ-RECORDS
           PERFORM 500-CLOSE-ROUTINE
       .

       150-SORTMERGE-INPUT-FILES.
           SORT SORT-FILE 
                ON ASCENDING KEY SORT-VENDOR-ID
                ON ASCENDING KEY SORT-WAREHOUSE-ID
                ON ASCENDING KEY SORT-COSTUME-ID
                ON ASCENDING KEY SORT-COSTUME-DATA
                USING UNSORTED-CH20P4F20
                GIVING SORTED-CH20P4F20

           SORT SORT-FILE 
                ON ASCENDING KEY SORT-VENDOR-ID
                ON ASCENDING KEY SORT-WAREHOUSE-ID
                ON ASCENDING KEY SORT-COSTUME-ID
                ON ASCENDING KEY SORT-COSTUME-DATA
                USING UNSORTED-LA10P4F20
                GIVING SORTED-LA10P4F20

           SORT SORT-FILE 
                ON ASCENDING KEY SORT-VENDOR-ID
                ON ASCENDING KEY SORT-WAREHOUSE-ID
                ON ASCENDING KEY SORT-COSTUME-ID
                ON ASCENDING KEY SORT-COSTUME-DATA
                USING UNSORTED-NY30P4F20
                GIVING SORTED-NY30P4F20
           
           MERGE SORT-FILE
                ON ASCENDING KEY SORT-VENDOR-ID
                ON ASCENDING KEY SORT-WAREHOUSE-ID
                ON ASCENDING KEY SORT-COSTUME-ID
                ON ASCENDING KEY SORT-COSTUME-DATA 
            USING SORTED-CH20P4F20, 
                             SORTED-LA10P4F20,
            SORTED-NY30P4F20
            GIVING MERGED-FILE
            
            .
       125-HOUSEKEEPING.

           OPEN    INPUT   MERGED-FILE
                   OUTPUT  ERROR-FILE
                   OUTPUT  SUMMARY-REPORT

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE
           MOVE RUN-MONTH TO H1-MONTH
           MOVE RUN-DAY   TO H1-DAY
           MOVE RUN-YEAR  TO H1-YEAR

           MOVE 1 TO PROPER-SPACING
       .

       150-READ-RECORDS.
             PERFORM UNTIL NO-MORE-RECORDS
                  READ MERGED-FILE
                      AT END
                          MOVE 'N' TO EOF-FLAG
                          PERFORM 475-END-OF-JOB-ROUTINE
                      NOT AT END
                          EVALUATE M-VENDOR-ID
                              WHEN 'CH20'
                                PERFORM 275-PROCESS-RTN
                              WHEN 'LA10'
                                PERFORM 275-PROCESS-RTN
                              WHEN 'NY30'
                                PERFORM 275-PROCESS-RTN
                              WHEN OTHER
                                 MOVE MERGED-RECORD
                                 TO ERROR-FILE-RECORD
                                 WRITE ERROR-FILE-RECORD
                                 AFTER ADVANCING 1 LINE
                            END-EVALUATE

                  END-READ
              END-PERFORM
       .

       175-REPORT-HEADERS.

             WRITE REPORT-LINE FROM REPORT-HEADER1
                   AFTER ADVANCING PAGE
             MOVE 1 TO PROPER-SPACING

             WRITE REPORT-LINE FROM REPORT-HEADER2
                   AFTER ADVANCING PROPER-SPACING
             MOVE 3 TO PROPER-SPACING
       .
       200-VENDOR-HEADER.
           PERFORM 175-REPORT-HEADERS
           PERFORM 325-VENDOR-NAME
           MOVE VENDOR-HEADER TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
       .

       225-WAREHOUSE-HEADER.

           PERFORM 350-WAREHOUSE-NAME
           MOVE WAREHOUSE-HEADER TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 3 TO PROPER-SPACING
       .

       250-COSTUME-HEADER.

           MOVE COSTUME-HEADER-1 TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE COSTUME-HEADER-2 TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
       .

       275-PROCESS-RTN.

           EVALUATE TRUE
               WHEN FIRST-RECORD = 'YES'
                   MOVE M-VENDOR-ID    TO HF-VENDOR-HOLD
                   MOVE M-WAREHOUSE-ID TO HF-WAREHOUSE-HOLD
                   MOVE M-COSTUME-ID   TO HF-COSTUME-HOLD
                   MOVE 'NO' TO FIRST-RECORD
                   PERFORM 200-VENDOR-HEADER
                   PERFORM 225-WAREHOUSE-HEADER
                   PERFORM 250-COSTUME-HEADER
               WHEN M-VENDOR-ID NOT = HF-VENDOR-HOLD
                   PERFORM 375-VENDOR-MAJOR-BREAK
                   PERFORM 200-VENDOR-HEADER
                   PERFORM 225-WAREHOUSE-HEADER
                   PERFORM 250-COSTUME-HEADER
               WHEN M-WAREHOUSE-ID NOT = HF-WAREHOUSE-HOLD
                   PERFORM 400-WAREHOUSE-INTERMEDIATE-BREAK
                   PERFORM 225-WAREHOUSE-HEADER
                   PERFORM 250-COSTUME-HEADER
               WHEN M-COSTUME-ID NOT = HF-COSTUME-HOLD
                   PERFORM 425-COSTUME-MINOR-BREAK
                   PERFORM 250-COSTUME-HEADER
           END-EVALUATE

           PERFORM 300-TRAVERSE-ARRAY
               VARYING SUB FROM 1 BY 1
                   UNTIL SUB > 6
 
         .

       300-TRAVERSE-ARRAY.

           EVALUATE TRUE
               WHEN FIRST-NAME = 'YES'
                   MOVE 'NO' TO FIRST-NAME
                   MOVE M-COSTUME-NAME(SUB) TO DL-NAME
               WHEN FIRST-NAME = 'NO'
                   MOVE ' ' TO DL-NAME
           END-EVALUATE



           EVALUATE M-COSTUME-TYPE(SUB)
              WHEN 'A'
                   MOVE 'ADULT' TO DL-TYPE
               WHEN 'C'
                   MOVE 'CHILD' TO DL-TYPE
               WHEN OTHER 
                   STRING 
                   CF-BAD DELIMITED BY SIZE
                   M-COSTUME-TYPE(SUB) DELIMITED BY SIZE
                    INTO DL-TYPE
                  END-STRING 
           END-EVALUATE

           SET COSTUME-SIZE-INDEX TO 1
           SEARCH COSTUME-ITEM
             AT END MOVE 'ERROR' TO DL-SIZE 
             WHEN M-COSTUME-SIZE(SUB) = 'L'
                MOVE L(COSTUME-SIZE-INDEX) TO DL-SIZE 
             WHEN M-COSTUME-SIZE(SUB) = 'S'
                MOVE S(COSTUME-SIZE-INDEX) TO DL-SIZE 
             WHEN M-COSTUME-SIZE(SUB) = 'M'
                MOVE M(COSTUME-SIZE-INDEX) TO DL-SIZE 
             WHEN M-COSTUME-SIZE(SUB) = 'P'
                MOVE P(COSTUME-SIZE-INDEX) TO DL-SIZE 
           END-SEARCH

           IF M-QUANTITY-IN-STOCK(SUB) IS NUMERIC AND
                M-PURCH-PRICE(SUB) IS NUMERIC
                MOVE M-QUANTITY-IN-STOCK(SUB) TO DL-QTY-IN-STOCK
                COMPUTE DF-CCOST-TOT = 
                    M-QUANTITY-IN-STOCK(SUB) * M-PURCH-PRICE(SUB)
                MOVE DF-CCOST-TOT TO DL-TOTAL-COST
           ELSE
                MOVE 0 TO DL-QTY-IN-STOCK
                MOVE 0 TO DL-TOTAL-COST
           END-IF

           ADD DF-CCOST-TOT TO TF-COSTUME-TOT
           MOVE ZEROS TO DF-CCOST-TOT

           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
       .

       325-VENDOR-NAME.
         EVALUATE TRUE
            WHEN M-VENDOR-ID = 'CH20'
                  MOVE 'CHICAGO' TO VH-VENDOR-NAME
                                    VTL-VENDOR-NAME
            WHEN M-VENDOR-ID = 'LA10'
                 MOVE 'LOS ANGELES' TO VH-VENDOR-NAME
                                       VTL-VENDOR-NAME
            WHEN M-VENDOR-ID = 'NY30'
                 MOVE 'NEW YORK CITY' TO VH-VENDOR-NAME
                                         VTL-VENDOR-NAME
            WHEN OTHER
                  STRING 
                   CF-INVALID DELIMITED BY SIZE
                   M-VENDOR-ID DELIMITED BY SIZE
                    INTO VH-VENDOR-NAME
                  END-STRING

                 MOVE VH-VENDOR-NAME TO VTL-VENDOR-NAME
         END-EVALUATE
       .

       350-WAREHOUSE-NAME.
         EVALUATE TRUE
            WHEN M-WAREHOUSE-ID = 'BHM'
                  MOVE 'BIRMINGHAM' TO WH-WAREHOUSE-NAME
                                       WTL-WAREHOUSE-NAME
            WHEN M-WAREHOUSE-ID = 'HUN'
                 MOVE 'HUNTSVILLE'  TO WH-WAREHOUSE-NAME
                                       WTL-WAREHOUSE-NAME
            WHEN OTHER
                  MOVE MERGED-RECORD
                  TO ERROR-FILE-RECORD
                  WRITE ERROR-FILE-RECORD
                  AFTER ADVANCING 1 LINE

         END-EVALUATE
       .

       375-VENDOR-MAJOR-BREAK.

           PERFORM 400-WAREHOUSE-INTERMEDIATE-BREAK

           MOVE TF-VENDOR-TOT TO VTL-VENDOR-TOTAL
           ADD TF-VENDOR-TOT  TO TF-GRAND-TOT
           MOVE ZERO TO TF-VENDOR-TOT

           MOVE M-VENDOR-ID TO HF-VENDOR-HOLD

           MOVE VENDOR-TOTAL-LINE TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
       .

       400-WAREHOUSE-INTERMEDIATE-BREAK.
           PERFORM 425-COSTUME-MINOR-BREAK

           MOVE TF-WAREHOUSE-TOT TO WTL-WAREHOUSE-TOTAL
           ADD TF-WAREHOUSE-TOT  TO TF-VENDOR-TOT

           MOVE ZERO TO TF-WAREHOUSE-TOT

           MOVE M-WAREHOUSE-ID TO HF-WAREHOUSE-HOLD

           MOVE WAREHOUSE-TOTAL-LINE TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
       .

       425-COSTUME-MINOR-BREAK.
           MOVE TF-COSTUME-TOT TO CTL-COSTUME-TOTAL
           ADD TF-COSTUME-TOT  TO TF-WAREHOUSE-TOT
           MOVE ZERO TO TF-COSTUME-TOT

           MOVE M-COSTUME-ID TO HF-COSTUME-HOLD
           MOVE 'YES' TO FIRST-NAME

           MOVE 2 TO PROPER-SPACING
           MOVE COSTUME-TOTAL-LINE TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
       .

       450-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
       .

       475-END-OF-JOB-ROUTINE.
           PERFORM 375-VENDOR-MAJOR-BREAK
 
           MOVE TF-GRAND-TOT TO GTL-GRAND-TOTAL
           MOVE GRAND-TOTAL-LINE TO REPORT-LINE
           PERFORM 450-WRITE-A-LINE
       .

       500-CLOSE-ROUTINE.
              CLOSE    MERGED-FILE
              CLOSE    ERROR-FILE 
              CLOSE    SUMMARY-REPORT
              STOP RUN
       .
