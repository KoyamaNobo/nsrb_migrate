       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     acctest.
      *FILE書き込みテスト用(INDEXED(IS) )
       ENVIRONMENT     DIVISION.
      *ARGUMENT-VALUE    IS A-VAL
       INPUT-OUTPUT    SECTION.
       FILE-CONTROL.
       SELECT F1 ASSIGN TO "TEST-A"
           FILE STATUS IS ERR-STAT.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  W-CNT       PIC 9(002).
       01  W-HM.
           02  W-HMC   PIC S9(006) VALUE 9995.
           02  W-HM    PIC N(024).
       01  JS-SIGN     PIC 9(002).
       01  JS-SIG2     PIC 9(003) COMP-3.
       01  TESTN       PIC ZZZZZ9.
       01  A-DATE      PIC 99B99B99.
       01  R-HMC       PIC -999999.
       01  COTEST.
           02 C1TEST   PIC 9(003).
           02 C2TEST   PIC s9(005).
       01  ERR-STAT    PIC x(002).
      *処理    
       PROCEDURE       DIVISION.
       MOVE 12 TO C1TEST.
       MOVE -365 TO C2TEST.
       DISPLAY "********** start 　***********".
       DISPLAY W-HMC.
       DISPLAY TESTN.
       DISPLAY JS-SIG2.
       MOVE W-HMC TO R-HMC.
       DISPLAY R-HMC.
       DISPLAY COTEST.
       DISPLAY "********** end ***********".
       STOP RUN.
       
