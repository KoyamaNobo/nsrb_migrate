       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY200.
      ***********************************************************
      *    PROGRAM         :  分類　年間販売実績表・製品受払表  *
      *    PRINTER TYPE    :  JIPS                              *
      *    SCREEN          :  ******                            *
      *    COMPILE TYPE    :  COBOL                             *
      ***********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD11.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN1          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SG1          PIC Z9.
           02  F              PIC  N(004) VALUE "月　～　".
           02  H-EN1          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EG1          PIC Z9.
           02  F              PIC  N(015) VALUE
                "月　分類　販売実績表　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE1        PIC 99B99B99.
       01  HEAD21.
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(004) VALUE "販売足数".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "足単価".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "販売原価".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "足単価".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(004) VALUE "販売利益".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(003) VALUE "利益率".
           02  F              PIC  X(001) VALUE "%".
       01  HEAD12.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN2          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SG2          PIC Z9.
           02  F              PIC  N(004) VALUE "月　～　".
           02  H-EN2          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EG2          PIC Z9.
           02  F              PIC  N(015) VALUE
                "月　分類　製品受払表　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE2        PIC 99B99B99.
       01  HEAD22.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(007) VALUE "I----  ".
           02  F              PIC  N(004) VALUE "前期繰越".
           02  F              PIC  X(017) VALUE "  -----I I-----  ".
           02  F              PIC  N(004) VALUE "当期受入".
           02  F              PIC  X(017) VALUE "  -----I I-----  ".
           02  F              PIC  N(004) VALUE "当期払出".
           02  F              PIC  X(016) VALUE "  -----I I----  ".
           02  F              PIC  N(004) VALUE "次期繰越".
           02  F              PIC  X(008) VALUE "  -----I".
       01  HEAD32.
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  P-TM           PIC  N(018).
           02  P-MD    REDEFINES P-TM.
             03  P-M0         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M1         PIC  N(003).
             03  F            PIC  X(004).
             03  P-M2         PIC  N(008).
           02  P-D1.
             03  P-US1        PIC -----,---,--9.
             03  P-UK1        PIC -----,---,---,--9.
             03  P-UT1        PIC -----,--9.
             03  P-UG1        PIC -----,---,---,--9.
             03  P-GT1        PIC -----,--9.
             03  P-UR1        PIC -----,---,---,--9.
             03  P-RR1        PIC ------9.9.
             03  F            PIC  X(007).
           02  P-D2    REDEFINES P-D1.
             03  P-ZS2        PIC --,---,--9.
             03  P-ZK2        PIC --,---,---,--9.
             03  P-NS2        PIC ---,---,--9.
             03  P-NK2        PIC --,---,---,--9.
             03  P-US2        PIC ---,---,--9.
             03  P-UG2        PIC --,---,---,--9.
             03  P-YS2        PIC --,---,--9.
             03  P-YK2        PIC --,---,---,--9.
       01  W-DATA.
           02  W-SNGD.
             03  W-SNEN       PIC  9(004).
             03  W-SND   REDEFINES W-SNEN.
               04  W-SN1      PIC  9(002).
               04  W-SN2      PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNGD.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-ENGD.
             03  W-ENEN       PIC  9(004).
             03  W-END   REDEFINES W-ENEN.
               04  W-EN1      PIC  9(002).
               04  W-EN2      PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENGD.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001) VALUE 0.
           02  W-BCD.
             03  W-BC3        PIC  9(002).
             03  W-BMC        PIC  9(002).
             03  W-BC1        PIC  9(002).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
           02  WN-D.
             03  WN-ZS        PIC S9(007).
             03  WN-ZK        PIC S9(010).
             03  WN-NS        PIC S9(008).
             03  WN-NK        PIC S9(010).
             03  WN-US        PIC S9(008).
             03  WN-UG        PIC S9(010).
             03  WN-YS        PIC S9(007).
             03  WN-YK        PIC S9(010).
             03  WN-UK        PIC S9(010).
           02  W-D.
             03  W-UT         PIC S9(005).
             03  W-GT         PIC S9(005).
             03  W-UR         PIC S9(010).
             03  W-RR         PIC S9(003)V9(01).
             03  W-RI         PIC S9(001)V9(03).
       01  WP-D.
           02  WP-ZS          PIC S9(007).
           02  WP-ZK          PIC S9(010).
           02  WP-NS          PIC S9(008).
           02  WP-NK          PIC S9(010).
           02  WP-US          PIC S9(008).
           02  WP-UG          PIC S9(010).
           02  WP-YS          PIC S9(007).
           02  WP-YK          PIC S9(010).
           02  WP-UK          PIC S9(010).
       01  WT-D.
           02  WT-ZS          PIC S9(007).
           02  WT-ZK          PIC S9(010).
           02  WT-NS          PIC S9(008).
           02  WT-NK          PIC S9(010).
           02  WT-US          PIC S9(008).
           02  WT-UG          PIC S9(010).
           02  WT-YS          PIC S9(007).
           02  WT-YK          PIC S9(010).
           02  WT-UK          PIC S9(010).
       01  WS-D.
           02  WS-ZS          PIC S9(007).
           02  WS-ZK          PIC S9(010).
           02  WS-NS          PIC S9(008).
           02  WS-NK          PIC S9(010).
           02  WS-US          PIC S9(008).
           02  WS-UG          PIC S9(010).
           02  WS-YS          PIC S9(007).
           02  WS-YK          PIC S9(010).
           02  WS-UK          PIC S9(010).
       01  WA-D.
           02  WA-ZS          PIC S9(007).
           02  WA-ZK          PIC S9(010).
           02  WA-NS          PIC S9(008).
           02  WA-NK          PIC S9(010).
           02  WA-US          PIC S9(008).
           02  WA-UG          PIC S9(010).
           02  WA-YS          PIC S9(007).
           02  WA-YK          PIC S9(010).
           02  WA-UK          PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIHKBM.
           COPY LSPF.
       01  HUHY-F_HMY200.
           02  HUHY-F_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  HUHY-F_LNAME   PIC  X(013)  VALUE "HUHY-F_HMY200".
           02  F              PIC  X(001).
           02  HUHY-F_KEY1    PIC  X(100)  VALUE SPACE.
           02  HUHY-F_KEY2    PIC  X(100)  VALUE SPACE.
           02  HUHY-F_SORT    PIC  X(100)  VALUE SPACE.
           02  HUHY-F_IDLST   PIC  X(100)  VALUE SPACE.
           02  HUHY-F_RES     USAGE  POINTER.
       01  HUHY-R.
           02  UH-BCB.
             03  UH-BC1       PIC  9(002).
             03  UH-BC2.
               04  UH-BC21    PIC  9(001).
               04  UH-BC22    PIC  9(001).
             03  UH-BC3       PIC  9(002).
             03  UH-BMC       PIC  9(002).
             03  UH-BMNO      PIC  9(001).
           02  UH-ZS          PIC S9(007).
           02  UH-ZK          PIC S9(010).
           02  UH-NS          PIC S9(007).
           02  UH-NK          PIC S9(010).
           02  UH-US          PIC S9(007).
           02  UH-UK          PIC S9(010).
           02  UH-YS          PIC S9(007).
           02  UH-YK          PIC S9(010).
           02  UH-UG          PIC S9(010).
           02  UH-NG          PIC  9(006).
           02  F              PIC  X(035).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　年間　分類　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                  "[   年  月 ～   年  月 ]".
           02  FILLER  PIC  X(022) VALUE
                  "確認　OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACT.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-MID1    PIC  N(005) VALUE "販売実績表".
             03  D-MID2    PIC  N(005) VALUE "製品受払表".
           02  D-NG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  FILLER  PIC  X(007).
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "354" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "13" "15" "24" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "19" "22" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-ACT" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "36" "1" " " "C-ACT" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "6" "0" "20" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID1" "N" "6" "32" "10" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID2" "N" "6" "32" "10" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "13" "0" "8" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "9" "13" "17" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "9" "13" "21" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "9" "13" "29" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-EN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "9" "13" "33" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" " " "24" "0" "34" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME1" "X" "24" "15" "27" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME1" "X" "24" "29" "7" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME1" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-SNGD W-ENGD
           MOVE D-SPNG TO W-SNGS.
           MOVE D-EPNG TO W-ENGS.
           IF  W-SN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF.
           IF  W-SN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF.
           IF  W-EN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF.
           IF  W-EN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-MID1" D-MID1 "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                              RETURNING RESU.
           MOVE DATE-02R TO H-DATE1 H-DATE2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
           MOVE W-SN2 TO H-SN1 H-SN2.
           MOVE W-SGET TO H-SG1 H-SG2.
           MOVE W-EN2 TO H-EN1 H-EN2.
           MOVE W-EGET TO H-EG1 H-EG2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HUHY-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HUHY-F_PNAME1 " " BY REFERENCE HUHY-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-15.
      *           READ HUHY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HUHY-F_PNAME1 BY REFERENCE HUHY-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HUHY-F_IDLST HUHY-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ZERO = UH-ZS AND UH-ZK AND UH-NS AND UH-NK AND
                     UH-US AND UH-UK AND UH-YS AND UH-YK AND UH-UG
               GO TO M-15
           END-IF.
           IF  UH-NG < W-SNGD OR > W-ENGD
               GO TO M-15
           END-IF.
           CALL "PR_Open" RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-DC = 0
               MOVE HEAD11 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD21 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               MOVE HEAD12 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD22 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD32 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WA-D.
       M-20.
           MOVE UH-BC3 TO W-BC3.
           MOVE ZERO TO WS-D CHK.
       M-25.
           MOVE UH-BMC TO W-BMC.
           MOVE ZERO TO WT-D CHK2 CNT.
       M-30.
           MOVE UH-BC1 TO W-BC1.
           MOVE ZERO TO WN-D.
       M-35.
           ADD UH-US TO WN-US.
           ADD UH-UG TO WN-UG.
           IF  W-DC = 0
               ADD UH-UK TO WN-UK
               GO TO M-40
           END-IF.
           IF  UH-NG = W-SNGD
               MOVE UH-ZS TO WN-ZS
               MOVE UH-ZK TO WN-ZK
           END-IF.
           ADD UH-NS TO WN-NS.
           ADD UH-NK TO WN-NK.
           IF  UH-NG = W-ENGD
               MOVE UH-YS TO WN-YS
               MOVE UH-YK TO WN-YK
           END-IF.
       M-40.
      *           READ HUHY-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HUHY-F_PNAME1 BY REFERENCE HUHY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF.
           IF  ZERO = UH-ZS AND UH-ZK AND UH-NS AND UH-NK AND
                     UH-US AND UH-UK AND UH-YS AND UH-YK AND UH-UG
               GO TO M-40
           END-IF.
           IF  UH-NG < W-SNGD OR > W-ENGD
               GO TO M-40
           END-IF.
           IF  UH-BC3 NOT = W-BC3
               GO TO M-50
           END-IF.
           IF  UH-BMC NOT = W-BMC
               GO TO M-45
           END-IF.
           IF  UH-BC1 = W-BC1
               GO TO M-35
           END-IF.
           PERFORM S-05 THRU S-20.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           PERFORM S-45 THRU S-50.
           GO TO M-30.
       M-45.
           PERFORM S-05 THRU S-20.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-65.
           GO TO M-25.
       M-50.
           PERFORM S-05 THRU S-20.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-65.
           PERFORM S-70 THRU S-75.
           GO TO M-20.
       M-85.
           PERFORM S-05 THRU S-20.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           PERFORM S-45 THRU S-50.
           PERFORM S-55 THRU S-65.
           PERFORM S-70 THRU S-75.
           MOVE SPACE TO W-P.
           MOVE "　　【　総　合　計　】　　　　　　　" TO P-TM.
           MOVE ZERO TO WP-D.
           MOVE WA-D TO WP-D.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           IF  W-DC = 0
               CALL "DB_F_Close" USING
                BY REFERENCE HUHY-F_IDLST HUHY-F_PNAME1
               CALL "PR_Close" RETURNING RESP
               MOVE 1 TO W-DC
               CALL "SD_Output" USING "D-MID2" D-MID2 "p" 
                                              RETURNING RESU
               CALL "DB_F_Open" USING
                "INPUT" HUHY-F_PNAME1 " " BY REFERENCE HUHY-F_IDLST "0"
               GO TO M-15
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUHY-F_IDLST HUHY-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TM.
           IF  CHK1 NOT = 0
               GO TO S-10
           END-IF.
           MOVE 1 TO CHK1.
           MOVE SPACE TO HKB-KEY.
           MOVE "14" TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               MOVE SPACE TO HKB-BRN3
           END-IF.
           MOVE HKB-BRN3 TO P-M0.
       S-10.
           IF  CHK2 NOT = 0
               GO TO S-15
           END-IF.
           MOVE 1 TO CHK2.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           MOVE W-BMC TO HKB-BMC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               MOVE SPACE TO HKB-BMN
           END-IF.
           MOVE HKB-BMN TO P-M1.
       S-15.
           MOVE SPACE TO HKB-KEY.
           MOVE "11" TO HKB-NO.
           MOVE W-BC1 TO HKB-BR1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                              RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                              RETURNING RESU
               MOVE SPACE TO HKB-BRN1
           END-IF.
           MOVE HKB-BRN1 TO P-M2.
           MOVE ZERO TO WP-D.
           MOVE WN-D TO WP-D.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF.
       S-20.
           EXIT.
       S-25.
           MOVE ZERO TO W-D.
           MOVE WP-US TO P-US1.
           MOVE WP-UK TO P-UK1.
           MOVE WP-UG TO P-UG1.
           COMPUTE W-UR = WP-UK - WP-UG.
           MOVE W-UR TO P-UR1.
           IF  WP-US NOT = ZERO
               IF  WP-UK NOT = ZERO
                   COMPUTE W-UT ROUNDED = WP-UK / WP-US
                   MOVE W-UT TO P-UT1
               END-IF
           END-IF.
           IF  WP-US NOT = ZERO
               IF  WP-UG NOT = ZERO
                   COMPUTE W-GT ROUNDED = WP-UG / WP-US
                   MOVE W-GT TO P-GT1
               END-IF
           END-IF.
           IF  W-UR NOT = ZERO
               IF  WP-UK NOT = ZERO
                   COMPUTE W-RI ROUNDED = W-UR / WP-UK
                   COMPUTE W-RR ROUNDED = W-RI * 100
                   MOVE W-RR TO P-RR1
               END-IF
           END-IF.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-30.
           EXIT.
       S-35.
           MOVE WP-ZS TO P-ZS2.
           MOVE WP-ZK TO P-ZK2.
           MOVE WP-NS TO P-NS2.
           MOVE WP-NK TO P-NK2.
           MOVE WP-US TO P-US2.
           MOVE WP-UG TO P-UG2.
           MOVE WP-YS TO P-YS2.
           MOVE WP-YK TO P-YK2.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-40.
           EXIT.
       S-45.
           ADD WN-US TO WT-US.
           ADD WN-UG TO WT-UG.
           IF  W-DC = 0
               ADD WN-UK TO WT-UK
               GO TO S-50
           END-IF.
           ADD WN-ZS TO WT-ZS.
           ADD WN-ZK TO WT-ZK.
           ADD WN-NS TO WT-NS.
           ADD WN-NK TO WT-NK.
           ADD WN-YS TO WT-YS.
           ADD WN-YK TO WT-YK.
       S-50.
           EXIT.
       S-55.
           IF  CNT NOT = 2
               GO TO S-60
           END-IF.
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　（　小　計　）　　　　" TO P-TM.
           MOVE ZERO TO WP-D.
           MOVE WT-D TO WP-D.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-60.
           ADD WT-US TO WS-US.
           ADD WT-UG TO WS-UG.
           IF  W-DC = 0
               ADD WT-UK TO WS-UK
               GO TO S-65
           END-IF.
           ADD WT-ZS TO WS-ZS.
           ADD WT-ZK TO WS-ZK.
           ADD WT-NS TO WS-NS.
           ADD WT-NK TO WS-NK.
           ADD WT-YS TO WS-YS.
           ADD WT-YK TO WS-YK.
       S-65.
           EXIT.
       S-70.
           MOVE SPACE TO W-P.
           MOVE "　　　　［　合　計　］　　　　　　　" TO P-TM.
           MOVE ZERO TO WP-D.
           MOVE WS-D TO WP-D.
           IF  W-DC = 0
               PERFORM S-25 THRU S-30
           ELSE
               PERFORM S-35 THRU S-40
           END-IF.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-US TO WA-US.
           ADD WS-UG TO WA-UG.
           IF  W-DC = 0
               ADD WS-UK TO WA-UK
               GO TO S-75
           END-IF.
           ADD WS-ZS TO WA-ZS.
           ADD WS-ZK TO WA-ZK.
           ADD WS-NS TO WA-NS.
           ADD WS-NK TO WA-NK.
           ADD WS-YS TO WA-YS.
           ADD WS-YK TO WA-YK.
       S-75.
           EXIT.
