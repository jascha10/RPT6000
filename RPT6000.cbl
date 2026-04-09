       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPT6000.
      ****************************************************************
      * PROGRAM NAME: RPT6000
      * AUTHORS: Jacob Schamp
      * DATE: 03/25/2026
      * Github:
      * DESCRIPTION: The program reads customer master records and
      * produces a Year-To-Date Sales report. It then prints customer
      * sales for the current and previous year, calculates the
      * change amount and percentage, and displays salesrep totals,
      * branch totals, and grand totals.
      ****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT I_CUSTMAST ASSIGN TO CUSTMAST.
           SELECT O_RPT6000 ASSIGN TO RPT6000.

       DATA DIVISION.
       FILE SECTION.

       FD  I_CUSTMAST
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 130 CHARACTERS.
       01  CUSTOMER-MASTER-RECORD.
           05 CM-BRANCH-NUMBER       PIC 9(2).
           05 CM-SALESREP-NUMBER     PIC 9(2).
           05 CM-CUSTOMER-NUMBER     PIC 9(5).
           05 CM-CUSTOMER-NAME       PIC X(20).
           05 CM-SALES-THIS-YTD      PIC S9(5)V99.
           05 CM-SALES-LAST-YTD      PIC S9(5)V99.
           05 FILLER                 PIC X(87).

       FD  O_RPT6000
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 130 CHARACTERS.
       01  PRINT-AREA                PIC X(130).

       WORKING-STORAGE SECTION.

       01  CUSTMAST-EOF-SWITCH       PIC X VALUE "N".

       01  CONTROL-FIELDS.
           05 WS-CURRENT-BRANCH      PIC 99 VALUE ZERO.
           05 WS-PREVIOUS-BRANCH     PIC 99 VALUE ZERO.

       01  BRANCH-TOTALS.
           05 BT-THIS-YTD            PIC S9(9)V99 VALUE ZERO.
           05 BT-LAST-YTD            PIC S9(9)V99 VALUE ZERO.
           05 BT-CHANGE              PIC S9(9)V99 VALUE ZERO.

       01  PAGE-INFO.
           05 PAGE-COUNT             PIC 9(3) VALUE ZERO.
           05 LINE-COUNT             PIC 9(3) VALUE 99.
           05 LINES-ON-PAGE          PIC 9(3) VALUE 55.

       01  TOTAL-FIELDS.
           05 GRAND-TOTAL-THIS-YTD   PIC S9(9)V99 VALUE ZERO.
           05 GRAND-TOTAL-LAST-YTD   PIC S9(9)V99 VALUE ZERO.
           05 GRAND-TOTAL-CHANGE     PIC S9(9)V99 VALUE ZERO.

       01  CALC-FIELDS.
           05 WS-CHANGE-AMOUNT       PIC S9(9)V99 VALUE ZERO.
           05 WS-CHANGE-PERCENT      PIC S9(5)V9(1) VALUE ZERO.

       01  CURRENT-DATE-AND-TIME.
           05 CD-YEAR                PIC 9(4).
           05 CD-MONTH               PIC 9(2).
           05 CD-DAY                 PIC 9(2).
           05 CD-HOURS               PIC 9(2).
           05 CD-MINUTES             PIC 9(2).
           05 FILLER                 PIC X(9).

       01  HEADING-LINE-1.
           05 FILLER                 PIC X(7)  VALUE "DATE:  ".
           05 HL1-MONTH              PIC 9(2).
           05 FILLER                 PIC X     VALUE "/".
           05 HL1-DAY                PIC 9(2).
           05 FILLER                 PIC X     VALUE "/".
           05 HL1-YEAR               PIC 9(4).
           05 FILLER                 PIC X(16) VALUE SPACE.
           05 FILLER               PIC X(26) VALUE "YEAR-TO-DATE SALES".
           05 FILLER                 PIC X(6)  VALUE "REPORT".
           05 FILLER                 PIC X(22) VALUE SPACE.
           05 FILLER                 PIC X(7)  VALUE "PAGE:  ".
           05 HL1-PAGE               PIC ZZZ9.

       01  HEADING-LINE-2.
           05 FILLER                 PIC X(7)  VALUE "TIME:  ".
           05 HL2-HOURS              PIC 9(2).
           05 FILLER                 PIC X     VALUE ":".
           05 HL2-MINUTES            PIC 9(2).
           05 FILLER                 PIC X(68) VALUE SPACE.
           05 FILLER                 PIC X(7)  VALUE "RPT6000".

       01  HEADING-LINE-3.
           05 FILLER PIC X(6)  VALUE "BRANCH".
           05 FILLER PIC X     VALUE SPACE.
           05 FILLER PIC X(5)  VALUE "SALES".
           05 FILLER PIC X     VALUE SPACE.
           05 FILLER PIC X(4)  VALUE "CUST".
           05 FILLER PIC X(28) VALUE SPACE.
           05 FILLER PIC X(10) VALUE "SALES".
           05 FILLER PIC X(4)  VALUE SPACE.
           05 FILLER PIC X(10) VALUE "SALES".
           05 FILLER PIC X(4)  VALUE SPACE.
           05 FILLER PIC X(6)  VALUE "CHANGE".
           05 FILLER PIC X(4)  VALUE SPACE.
           05 FILLER PIC X(7)  VALUE "CHANGE".

       01  HEADING-LINE-4.
           05 FILLER PIC X(3)  VALUE "NUM".
           05 FILLER PIC X(5)  VALUE SPACE.
           05 FILLER PIC X(3)  VALUE "REP".
           05 FILLER PIC X(3)  VALUE SPACE.
           05 FILLER PIC X(3)  VALUE "NUM".
           05 FILLER PIC X(2)  VALUE SPACE.
           05 FILLER PIC X(20) VALUE "CUSTOMER NAME".
           05 FILLER PIC X(3)  VALUE SPACE.
           05 FILLER PIC X(10) VALUE "THIS YTD".
           05 FILLER PIC X(4)  VALUE SPACE.
           05 FILLER PIC X(10) VALUE "LAST YTD".
           05 FILLER PIC X(4)  VALUE SPACE.
           05 FILLER PIC X(6)  VALUE "AMOUNT".
           05 FILLER PIC X(4)  VALUE SPACE.
           05 FILLER PIC X(7)  VALUE "PERCENT".

       01  CUSTOMER-LINE.
           05 CL-BRANCH              PIC 99.
           05 FILLER                 PIC X VALUE SPACE.
           05 CL-REP                 PIC 99.
           05 FILLER                 PIC X VALUE SPACE.
           05 CL-CUST                PIC 9(5).
           05 FILLER                 PIC X(7) VALUE SPACE.
           05 CL-NAME                PIC X(20).
           05 FILLER                 PIC X(6) VALUE SPACE.
           05 CL-THIS                PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 CL-LAST                PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 CL-CHANGE              PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 CL-PERCENT             PIC ZZZ9.9-.

       01  BRANCH-TOTAL-LINE.
           05 FILLER                 PIC X(8) VALUE SPACE.
           05 FILLER                 PIC X(13) VALUE "BRANCH TOTAL".
           05 FILLER                 PIC X(8) VALUE SPACE.
           05 BTL-THIS               PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 BTL-LAST               PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 BTL-CHANGE             PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 BTL-PERCENT            PIC ZZZ9.9-.

       01  GRAND-TOTAL-LINE.
           05 FILLER                 PIC X(5) VALUE SPACE.
           05 FILLER                 PIC X(12) VALUE "GRAND TOTAL".
           05 FILLER                 PIC X(10) VALUE SPACE.
           05 GT-THIS                PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 GT-LAST                PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 GT-CHANGE              PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                 PIC X(4) VALUE SPACE.
           05 GT-PERCENT             PIC ZZZ9.9-.

       PROCEDURE DIVISION.

       000-PREPARE-SALES-REPORT.
           OPEN INPUT  I_CUSTMAST
                OUTPUT O_RPT6000

           PERFORM 230-PRINT-HEADINGS
           PERFORM 210-READ-CUSTOMER-RECORD

           IF CUSTMAST-EOF-SWITCH = "N"
               MOVE CM-BRANCH-NUMBER TO WS-PREVIOUS-BRANCH
           END-IF

           PERFORM UNTIL CUSTMAST-EOF-SWITCH = "Y"
               PERFORM 220-PROCESS-CUSTOMER-RECORD
               PERFORM 210-READ-CUSTOMER-RECORD
           END-PERFORM

           PERFORM 400-PRINT-BRANCH-TOTAL
           PERFORM 300-PRINT-GRAND-TOTALS

           CLOSE I_CUSTMAST O_RPT6000
           STOP RUN.

       210-READ-CUSTOMER-RECORD.
           READ I_CUSTMAST
               AT END
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH
           END-READ.

       220-PROCESS-CUSTOMER-RECORD.
           MOVE CM-BRANCH-NUMBER TO WS-CURRENT-BRANCH

           IF WS-CURRENT-BRANCH NOT = WS-PREVIOUS-BRANCH
               PERFORM 400-PRINT-BRANCH-TOTAL
               PERFORM 410-CLEAR-BRANCH-TOTALS
               MOVE WS-CURRENT-BRANCH TO WS-PREVIOUS-BRANCH
           END-IF

           IF LINE-COUNT >= LINES-ON-PAGE
               PERFORM 230-PRINT-HEADINGS
           END-IF

           SUBTRACT CM-SALES-LAST-YTD
               FROM CM-SALES-THIS-YTD
               GIVING WS-CHANGE-AMOUNT

           IF CM-SALES-LAST-YTD NOT = ZERO
               COMPUTE WS-CHANGE-PERCENT =
                   (WS-CHANGE-AMOUNT / CM-SALES-LAST-YTD) * 100
           ELSE
               MOVE 999.9 TO WS-CHANGE-PERCENT
           END-IF

           MOVE CM-BRANCH-NUMBER   TO CL-BRANCH
           MOVE CM-SALESREP-NUMBER TO CL-REP
           MOVE CM-CUSTOMER-NUMBER TO CL-CUST
           MOVE CM-CUSTOMER-NAME   TO CL-NAME
           MOVE CM-SALES-THIS-YTD  TO CL-THIS
           MOVE CM-SALES-LAST-YTD  TO CL-LAST
           MOVE WS-CHANGE-AMOUNT   TO CL-CHANGE
           MOVE WS-CHANGE-PERCENT  TO CL-PERCENT

           MOVE CUSTOMER-LINE TO PRINT-AREA
           WRITE PRINT-AREA

           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD
           ADD WS-CHANGE-AMOUNT  TO GRAND-TOTAL-CHANGE

           ADD CM-SALES-THIS-YTD TO BT-THIS-YTD
           ADD CM-SALES-LAST-YTD TO BT-LAST-YTD
           ADD WS-CHANGE-AMOUNT  TO BT-CHANGE

           ADD 1 TO LINE-COUNT.

       230-PRINT-HEADINGS.
           ADD 1 TO PAGE-COUNT
           MOVE ZERO TO LINE-COUNT

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME

           MOVE CD-MONTH   TO HL1-MONTH
           MOVE CD-DAY     TO HL1-DAY
           MOVE CD-YEAR    TO HL1-YEAR
           MOVE PAGE-COUNT TO HL1-PAGE
           MOVE CD-HOURS   TO HL2-HOURS
           MOVE CD-MINUTES TO HL2-MINUTES

           MOVE HEADING-LINE-1 TO PRINT-AREA
           WRITE PRINT-AREA

           MOVE HEADING-LINE-2 TO PRINT-AREA
           WRITE PRINT-AREA

           MOVE HEADING-LINE-3 TO PRINT-AREA
           WRITE PRINT-AREA

           MOVE HEADING-LINE-4 TO PRINT-AREA
           WRITE PRINT-AREA.

       300-PRINT-GRAND-TOTALS.
           IF GRAND-TOTAL-LAST-YTD NOT = ZERO
               COMPUTE WS-CHANGE-PERCENT =
                   (GRAND-TOTAL-CHANGE / GRAND-TOTAL-LAST-YTD) * 100
           ELSE
               MOVE 999.9 TO WS-CHANGE-PERCENT
           END-IF

           MOVE GRAND-TOTAL-THIS-YTD TO GT-THIS
           MOVE GRAND-TOTAL-LAST-YTD TO GT-LAST
           MOVE GRAND-TOTAL-CHANGE   TO GT-CHANGE
           MOVE WS-CHANGE-PERCENT    TO GT-PERCENT

           MOVE GRAND-TOTAL-LINE TO PRINT-AREA
           WRITE PRINT-AREA.

       400-PRINT-BRANCH-TOTAL.
           MOVE SPACES TO PRINT-AREA
           WRITE PRINT-AREA

           IF BT-LAST-YTD NOT = ZERO
               COMPUTE WS-CHANGE-PERCENT =
                   (BT-CHANGE / BT-LAST-YTD) * 100
           ELSE
               MOVE 999.9 TO WS-CHANGE-PERCENT
           END-IF

           MOVE BT-THIS-YTD TO BTL-THIS
           MOVE BT-LAST-YTD TO BTL-LAST
           MOVE BT-CHANGE   TO BTL-CHANGE
           MOVE WS-CHANGE-PERCENT TO BTL-PERCENT

           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA
           WRITE PRINT-AREA.

       410-CLEAR-BRANCH-TOTALS.
           INITIALIZE BRANCH-TOTALS.
