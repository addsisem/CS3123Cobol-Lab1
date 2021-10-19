       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB1.
       AUTHOR. Addyson Sisemore
      * LAB EXERCISE 1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DA-S-INPUT'.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC     PIC X(80).
       FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01 PRNT-REC      PIC X(125).
      WORKING-STORAGE SECTION.
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
       01 INPUT-DATA.
         03 I-NAME       PIC X(20).
         03 FILLER       PIC X(60).
      **************************************************************
      * LAYOUT FOR THE 1ST DATA LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-DATA1.
         03 FILLER       PIC X(10)  VALUE SPACES.
         03 L-NAME1      PIC X(20).
      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-HEADING1.
         03 FILLER      PIC X(10)   VALUE SPACES.
         03 FILLER      PIC X(9)   VALUE 'NAME'.
       01 MISC.
      **************************************************************
      *       END OF FILE (EOF) SWITCHES *
      *       0 = NOT AT EOF 1 = AT EOF *
       **************************************************************
         03 EOF-I      PIC 9   VALUE 0.
      **************************************************************
      *       START OF PROCEDURE DIVISION       *
      **************************************************************
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
             OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
           CLOSE INPUT-FILE
             PRNT-FILE.
           STOP RUN.
       1400-PRINT-HEAD.
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING PAGE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.
       1500-LOOP.
                  PERFORM 1600-PRINT-NAMES.
           PERFORM 2000-READ-INPUT.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
       1600-PRINT-NAMES.
           MOVE I-NAME          TO L-NAME1.
             WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
             AT END MOVE 1 TO EOF-I.
