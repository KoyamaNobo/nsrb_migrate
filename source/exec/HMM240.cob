       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM240.
      *********************************************************
      *    PROGRAM         :  直送先問合せ                    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ------                          *
      *        変更　　　  :  62/03/27                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  15K                PIC  X(005) VALUE X"1A24212078".
       01  W-D.
           02  W-UC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-CCD          PIC  9(003).
           02  W-L.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
           02  W-CNA          PIC  N(026).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LITCM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
           02  A-CCD   PIC  9(003).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID.
             03  FILLER  PIC  N(017) VALUE
                  "＊＊＊　　直送先　問合せ　　＊＊＊".
             03  FILLER  PIC  X(010) VALUE "得意先ｺｰﾄﾞ".
             03  FILLER  PIC  X(008) VALUE "終了=PF9".
             03  FILLER.
               04  FILLER  PIC  X(025) VALUE
                    "ｺｰﾄﾞ 直　　送　　先　　名".
               04  FILLER  PIC  N(010) VALUE
                    "ＴＥＬ　　　　　運送".
             03  FILLER.
               04  FILLER  PIC  N(012) VALUE
                    "住　　　　　所　　（上）".
               04  FILLER  PIC  N(003) VALUE "（下）".
           02  D-CNA   PIC  N(026).
           02  D-CD.
             03  FILLER.
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  N(026).
               04  FILLER  PIC  X(014).
               04  FILLER  PIC  9(001).
             03  FILLER.
               04  FILLER  PIC  N(020).
               04  FILLER  PIC  N(020).
           02  FILLER.
             03  D-NM    PIC  X(031) VALUE
                  "次ページ=ﾘﾀｰﾝ  得意先入力=BSKIP".
             03  D-EM    PIC  X(023) VALUE
                  "ＥＮＤ　ＤＡＴＡ   ﾘﾀｰﾝ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(019) VALUE
                  "***  得意先 ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "8" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "3" "12" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CCD" "9" "7" "1" "3" "A-TCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CCD" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "64" "1" "A-CCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "383" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID" " " "0" "0" "127" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID" "N" "1" "22" "34" " " "D-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID" "X" "3" "1" "10" "01D-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID" "X" "4" "10" "8" "02D-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MID" " " "5" "0" "45" "03D-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-MID" "X" "5" "1" "25" " " "04D-MID" RETURNING RESU.
       CALL "SD_Init" USING 
           "0204D-MID" "N" "5" "59" "20" "0104D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MID" " " "6" "0" "30" "04D-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-MID" "N" "6" "1" "24" " " "05D-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-MID" "N" "6" "41" "6" "0105D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CNA" "N" "3" "18" "52" "D-MID" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-CNA" BY REFERENCE W-CNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CD" " " "0" "0" "150" "D-CNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CD" " " "W-L1" "0" "70" " " "D-CD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-CD" "R9" "W-L1" "1" "3" " " "01D-CD" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-CD" BY REFERENCE TC-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0201D-CD" "RN" "W-L1" "6" "52" "0101D-CD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-CD" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0301D-CD" "X" "W-L1" "59" "14" "0201D-CD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-CD" BY REFERENCE TC-TEL "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0401D-CD" "9" "W-L1" "77" "1" "0301D-CD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-CD" BY REFERENCE TC-UCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CD" " " "W-L2" "0" "80" "01D-CD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-CD" "N" "W-L2" "1" "40" " " "02D-CD" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-CD" BY REFERENCE TC-JSU "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0202D-CD" "N" "W-L2" "41" "40" "0102D-CD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-CD" BY REFERENCE TC-JSS "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "23" "0" "54" "D-CD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "31" "31" " " "04C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "46" "23" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "81" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "81" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "19" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
       M-10.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE ZERO TO TC-KEY.
           MOVE W-TCD TO TC-TCD.
      *           START TC-M KEY NOT < TC-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TC-M_PNAME1 "TC-KEY" " NOT < " TC-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE TC-TCD TO W-TCD.
           MOVE SPACE TO W-CNA.
           IF  TC-CCD = 001
               MOVE TC-NAME TO W-CNA
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
       M-18.
           CALL "SD_Accept" USING BY REFERENCE A-CCD "A-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-18
           END-IF
           MOVE W-TCD TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           START TC-M KEY NOT < TC-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TC-M_PNAME1 "TC-KEY" " NOT < " TC-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  W-TCD NOT = TC-TCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE 5 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-20.
           MOVE TC-UCD TO W-UC.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 < 23
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
           MOVE 7 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-30.
           CALL "SD_Output" USING "D-CD" D-CD "p" RETURNING RESU.
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  TC-TCD = W-TCD
               GO TO M-20
           END-IF.
       M-35.
           CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB
               GO TO M-35
           END-IF
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
