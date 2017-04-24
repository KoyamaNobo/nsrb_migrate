       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ASM010.
      *********************************************************
      *    PROGRAM         :  処理日付入力　　　　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCAM01                          *
      *    DATA WRITEN     :  57/06/07                        *
      *        変更　　　  :  97/03/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  WORK-AREA.
           02  W-NO           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-NGPF.
             03  W-NENF       PIC  9(004).
             03  W-GETF       PIC  9(002).
             03  W-PEYF       PIC  9(002).
           02  W-NGPR         PIC  9(008).
           02  W-SNEN         PIC  9(004).
       01  W-DATA.
           02  W-02R          PIC  9(006).
           02  W-02   REDEFINES W-02R.                                  履物
             03  W-021        PIC  9(002).
             03  W-022        PIC  9(002).
             03  W-023        PIC  9(002).
           02  W-03R          PIC  9(006).
           02  W-03   REDEFINES W-03R.                                  工品
             03  W-031        PIC  9(002).
             03  W-032        PIC  9(002).
             03  W-033        PIC  9(002).
           02  W-04R          PIC  9(006).
           02  W-04   REDEFINES W-04R.                                  手形
             03  W-041        PIC  9(002).
             03  W-042        PIC  9(002).
             03  W-043        PIC  9(002).
           02  W-05R          PIC  9(006).
           02  W-05   REDEFINES W-05R.                                  購買
             03  W-051        PIC  9(002).
             03  W-052        PIC  9(002).
             03  W-053        PIC  9(002).
           02  W-06R          PIC  9(006).
           02  W-06   REDEFINES W-06R.                                  その他１
             03  W-061        PIC  9(002).
             03  W-062        PIC  9(002).
             03  W-063        PIC  9(002).
           02  W-WC.
             03  W-WC1.
               04  W-YF1      PIC  9(002).
               04  W-YT1      PIC  9(002).
               04  W-YC1      PIC  9(004).
             03  W-WC2.
               04  W-YF2      PIC  9(002).
               04  W-YT2      PIC  9(002).
               04  W-YC2      PIC  9(004).
           02  W-SC.                                                    西暦C
             03  W-SC1.                                                 1900年
               04  W-NF1      PIC  9(002).                              開始年
               04  W-NT1      PIC  9(002).                              終了年
               04  W-NC1      PIC  9(004).                              +1900
               04  W-NCD1  REDEFINES W-NC1.
                 05  W-NC11   PIC  9(002).
                 05  W-NC12   PIC  9(002).
             03  W-SC2.                                                 2000年
               04  W-NF2      PIC  9(002).                              開始年
               04  W-NT2      PIC  9(002).                              終了年
               04  W-NC2      PIC  9(004).                              +2000
      *
           COPY LSTAT.
      *
           COPY  LIBFDD.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-NO    PIC  9(001).
           02  FILLER.
             03  A-02R   PIC  9(006).
             03  A-03R   PIC  9(006).
             03  A-04R   PIC  9(006).
             03  A-05R   PIC  9(006).
             03  A-06R   PIC  9(006).
           02  FILLER.
             03  A-NF1   PIC  9(002).
             03  A-NT1   PIC  9(002).
             03  A-NC1   PIC  9(004).
             03  A-YF1   PIC  9(002).
             03  A-YT1   PIC  9(002).
             03  A-YC1   PIC  9(004).
           02  FILLER.
             03  A-NF2   PIC  9(002).
             03  A-NT2   PIC  9(002).
             03  A-NC2   PIC  9(004).
             03  A-YF2   PIC  9(002).
             03  A-YT2   PIC  9(002).
             03  A-YC2   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DATE  PIC 99/99/99 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(024) VALUE
                  "***  DATEM READ ｴﾗｰ  ***".
             03  E-ME2   PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  日付　チェック  ***".
           COPY LSSEM.
           COPY LIBSCR.
      **
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "6" "61" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "9" "0" "30" "A-NO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-02R" "9" "9" "12" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-02R" BY REFERENCE W-02R "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-03R" "9" "9" "24" "6" "A-02R" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-03R" BY REFERENCE W-03R "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-04R" "9" "9" "36" "6" "A-03R" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-04R" BY REFERENCE W-04R "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-05R" "9" "9" "48" "6" "A-04R" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-05R" BY REFERENCE W-05R "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-06R" "9" "9" "60" "6" "A-05R" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-06R" BY REFERENCE W-06R "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "17" "0" "16" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NF1" "9" "17" "18" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NF1" BY REFERENCE W-NF1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NT1" "9" "17" "25" "2" "A-NF1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NT1" BY REFERENCE W-NT1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NC1" "9" "17" "32" "4" "A-NT1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NC1" BY REFERENCE W-NC1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YF1" "9" "17" "46" "2" "A-NC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YF1" BY REFERENCE W-YF1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YT1" "9" "17" "53" "2" "A-YF1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YT1" BY REFERENCE W-YT1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YC1" "9" "17" "60" "4" "A-YT1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YC1" BY REFERENCE W-YC1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "19" "0" "16" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NF2" "9" "19" "18" "2" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NF2" BY REFERENCE W-NF2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NT2" "9" "19" "25" "2" "A-NF2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NT2" BY REFERENCE W-NT2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NC2" "9" "19" "32" "4" "A-NT2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NC2" BY REFERENCE W-NC2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YF2" "9" "19" "46" "2" "A-NC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YF2" BY REFERENCE W-YF2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YT2" "9" "19" "53" "2" "A-YF2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YT2" BY REFERENCE W-YT2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YC2" "9" "19" "60" "4" "A-YT2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YC2" BY REFERENCE W-YC2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "63" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DATE" "99/99/99" "2" "61" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "75" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "75" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "27" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCAM01" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE WITH UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" M-DATE_PNAME1 BY REFERENCE DATE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           MOVE ZERO TO WORK-AREA.
           ACCEPT W-NGPS FROM DATE.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-NGPF W-NGPR.
           SUBTRACT 1 FROM W-GETF.
           IF  W-GETF = ZERO
               MOVE 12 TO W-GETF
               SUBTRACT 1 FROM W-NENF
           END-IF
      *
           MOVE DATE-02R TO W-02R.
           MOVE DATE-03R TO W-03R.
           MOVE DATE-04R TO W-04R.
           MOVE DATE-05R TO W-05R.
           MOVE DATE-06R TO W-06R.
           MOVE DATE-WC TO W-WC.
           MOVE DATE-SC TO W-SC.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-10.
           CALL "SD_Output" USING "A-02R" A-02R "p" RETURNING RESU.
           CALL "SD_Output" USING "A-03R" A-03R "p" RETURNING RESU.
           CALL "SD_Output" USING "A-04R" A-04R "p" RETURNING RESU.
           CALL "SD_Output" USING "A-05R" A-05R "p" RETURNING RESU.
           CALL "SD_Output" USING "A-06R" A-06R "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NF1" A-NF1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NT1" A-NT1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NC1" A-NC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NF2" A-NF2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NT2" A-NT2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NC2" A-NC2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YF1" A-YF1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YT1" A-YT1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YC1" A-YC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YF2" A-YF2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YT2" A-YT2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YC2" A-YC2 "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-NO = 1
               GO TO M-20
           END-IF
           IF  W-NO = 2
               GO TO M-25
           END-IF
           IF  W-NO = 3
               GO TO M-30
           END-IF
           IF  W-NO = 4
               GO TO M-35
           END-IF
           IF  W-NO = 5
               GO TO M-40
           END-IF
           IF  W-NO = 8
               GO TO M-50
           END-IF
           GO TO M-15.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-02R "A-02R" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-022 < 1 OR > 12
               GO TO M-20
           END-IF
           IF  W-023 < 1 OR > 31
               GO TO M-20
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE W-02R TO W-NGPS.
           PERFORM NEN-RTN THRU NEN-EX.
           GO TO M-90.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-03R "A-03R" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-032 < 1 OR > 12
               GO TO M-25
           END-IF
           IF  W-033 < 1 OR > 31
               GO TO M-25
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE W-03R TO W-NGPS.
           PERFORM NEN-RTN THRU NEN-EX.
           GO TO M-90.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-04R "A-04R" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-042 < 1 OR > 12
               GO TO M-30
           END-IF
           IF  W-043 < 1 OR > 31
               GO TO M-30
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE W-04R TO W-NGPS.
           PERFORM NEN-RTN THRU NEN-EX.
           GO TO M-90.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-05R "A-05R" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-052 < 1 OR > 12
               GO TO M-35
           END-IF
           IF  W-053 < 1 OR > 31
               GO TO M-35
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE W-05R TO W-NGPS.
           PERFORM NEN-RTN THRU NEN-EX.
           GO TO M-90.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-06R "A-06R" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-062 < 1 OR > 12
               GO TO M-40
           END-IF
           IF  W-063 < 1 OR > 31
               GO TO M-40
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE W-06R TO W-NGPS.
           PERFORM NEN-RTN THRU NEN-EX.
           GO TO M-90.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-NF1 "A-NF1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF.
       M-52.
           CALL "SD_Accept" USING BY REFERENCE A-NT1 "A-NT1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-52
           END-IF
           IF  W-NF1 > W-NT1
               GO TO M-52
           END-IF.
       M-54.
           CALL "SD_Accept" USING BY REFERENCE A-NC1 "A-NC1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-52
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-54
           END-IF
           IF  W-NC1 < 1900
               GO TO M-54
           END-IF
           IF  W-NC12 NOT = ZERO
               GO TO M-54
           END-IF
           COMPUTE W-SNEN = W-NC1 + 100.
       M-56.
           CALL "SD_Accept" USING BY REFERENCE A-NF2 "A-NF2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-54
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-56
           END-IF.
       M-58.
           CALL "SD_Accept" USING BY REFERENCE A-NT2 "A-NT2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-56
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-58
           END-IF
           IF  W-NF2 > W-NT2
               GO TO M-58
           END-IF
           IF  W-NT2 >= W-NF1
               GO TO M-58
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-NC2 "A-NC2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-58
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-NC2 NOT = ZERO AND W-SNEN
               GO TO M-60
           END-IF.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-YF1 "A-YF1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-60
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF.
       M-72.
           CALL "SD_Accept" USING BY REFERENCE A-YT1 "A-YT1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-70
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-72
           END-IF
           IF  W-YF1 > W-YT1
               GO TO M-72
           END-IF.
       M-74.
           CALL "SD_Accept" USING BY REFERENCE A-YC1 "A-YC1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-72
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-74
           END-IF.
       M-76.
           CALL "SD_Accept" USING BY REFERENCE A-YF2 "A-YF2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-74
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-76
           END-IF.
       M-78.
           CALL "SD_Accept" USING BY REFERENCE A-YT2 "A-YT2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-76
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-78
           END-IF
           IF  W-YF2 > W-YT2
               GO TO M-78
           END-IF
           IF  W-YT2 <= W-YF1
               GO TO M-78
           END-IF.
       M-80.
           CALL "SD_Accept" USING BY REFERENCE A-YC2 "A-YC2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-78
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-80
           END-IF.
       M-90.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-90
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-90
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE W-02R TO DATE-02R.
           MOVE W-03R TO DATE-03R.
           MOVE W-04R TO DATE-04R.
           MOVE W-05R TO DATE-05R.
           MOVE W-06R TO DATE-06R.
           MOVE W-WC  TO DATE-WC.
           MOVE W-SC  TO DATE-SC.
      *           REWRITE DATE-R INVALID
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "DB_Rollback"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_Commit".
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       NEN-RTN.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NGP > W-NGPR OR < W-NGPF
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       NEN-EX.
           EXIT.
