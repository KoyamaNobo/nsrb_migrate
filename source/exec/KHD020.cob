       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD020.
      *****************************************************************
      *    PROGRAM         :  工品その他・材料　売上伝票              *
      *    PRINTER TYPE    :  JIPS                                    *
      *    SCREEN          :  ______                                  *
      *    COMPILE TYPE    :  COBOL                                   *
      *    JS-SIGN         :  発行=0 , 再発行(更新後)=1 , 東海発行=3  *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-ECHK             PIC  9(001).
       77  JS-SIGN            PIC  9(001).
       77  W-MSG              PIC  X(030).
       77  W-FILE             PIC  X(013).
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  W-20K              PIC  X(005) VALUE X"1A24212474".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-P1.
           02  P-15K1         PIC  X(005).
           02  F              PIC  X(089).
           02  P-DC           PIC  9(001).
           02  F              PIC  X(003).
           02  P-DNO          PIC  9(006).
       01  W-P2.
           02  F              PIC  X(010).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(068).
           02  P-NM           PIC  X(001).
           02  P-NEN          PIC  9(002).
           02  F              PIC  X(002).
           02  P-GET          PIC Z9.
           02  F              PIC  X(002).
           02  P-PEY          PIC Z9.
           02  F              PIC  X(002).
       01  W-P3.
           02  F              PIC  X(009).
           02  P-JSU          PIC  N(020).
           02  F              PIC  X(054).
       01  W-P4.
           02  F              PIC  X(009).
           02  P-JSS          PIC  N(020).
           02  F              PIC  X(060).
       01  W-P5.
           02  F              PIC  X(011).
           02  P-OTNA         PIC  N(018).
           02  F              PIC  X(061).
       01  W-P5A.
           02  F              PIC  X(011).
           02  P-UTNA         PIC  N(018).
           02  F              PIC  X(061).
       01  W-P6.
           02  P-20K1         PIC  X(005).
           02  F              PIC  X(012).
           02  P-NO           PIC  N(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-SS           PIC  X(002).
           02  F              PIC  X(077).
       01  W-P7.
           02  P-15K2         PIC  X(005).
           02  F              PIC  X(008).
           02  P-JCD          PIC  9(006).
           02  P-HCDD  REDEFINES P-JCD.
             03  P-HCD        PIC  X(005).
             03  F            PIC  X(001).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SU           PIC ----,--9.99.
           02  F              PIC  X(001).
           02  P-T            PIC ----,--9.99.
           02  F              PIC  X(001).
           02  P-KIN          PIC ----,---,--9.
           02  F              PIC  X(002).
           02  P-AZM          PIC  N(004).
       01  W-P8.
           02  P-15K3         PIC  X(005).
           02  F              PIC  X(018).
           02  P-TEK          PIC  N(018).
           02  F              PIC  X(032).
           02  P-TKIN         PIC ----,---,--9.
           02  F              PIC  X(010).
       01  W-R.
           02  WR-DC          PIC  9(001).
           02  WR-DATE.
             03  F            PIC  9(002).
             03  WR-NGP.
               04  WR-NEN     PIC  9(002).
               04  WR-GET     PIC  9(002).
               04  WR-PEY     PIC  9(002).
           02  WR-TCD         PIC  9(004).
           02  WR-HCD         PIC  X(005).
           02  WR-SU          PIC S9(006)V9(02).
           02  WR-T           PIC S9(006)V9(02).
           02  WR-KIN         PIC S9(008).
           02  WR-YC          PIC  9(002).
           02  WR-SD          PIC  9(004).
           02  WR-NNO         PIC  X(006).
           02  WR-HYC         PIC  9(001).
           02  WR-CSC         PIC  9(001).
           02  WR-BKC         PIC  9(002).
           02  WR-SKD         PIC  9(008).
           02  WR-DNO         PIC  9(006).
           02  WR-GNO         PIC  9(001).
           02  WR-JCD         PIC  9(006).
           02  WR-GT          PIC S9(006)V9(02).
           02  WR-TEK         PIC  N(018).
           02  WR-BMC         PIC  9(001).
           02  F              PIC  X(003).
           02  WR-PRC         PIC  9(001).
       01  W-DATA.
           02  W-TPC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SDNO         PIC  9(006).
           02  W-EDNO         PIC  9(006).
           02  W-AF           PIC  9(002).
           02  W-DNO          PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-T            PIC S9(006)V9(02).
           02  W-KIN          PIC S9(008).
           02  W-GKIN         PIC S9(008).
           02  W-SHZ          PIC S9(008).
           02  W-TKIN         PIC S9(008).
           02  CHK            PIC  9(001).
           02  W-DTC          PIC  9(001).
           02  W-TEK          PIC  N(018).
           02  W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
       01  W-TORI.
           02  W-DC           PIC  9(002).
           02  W-SC           PIC  9(001).
           02  CNT1           PIC  9(002).
           02  CNT2           PIC  9(002).
           02  W-NAME         PIC  N(026).
           02  W-ANMD  REDEFINES W-NAME.
             03  W-NMD   OCCURS  26.
               04  W-NM       PIC  N(001).
           02  W-TNA   REDEFINES W-NAME.
             03  W-FTNA.
               04  W-FTNA1    PIC  N(008).
               04  W-FTNA2    PIC  N(010).
             03  W-RTNA       PIC  N(008).
           02  W-ONA          PIC  N(018).
           02  W-AONA  REDEFINES W-ONA.
             03  W-ONAD  OCCURS  18.
               04  W-ON       PIC  N(001).
           02  W-UNA          PIC  N(018).
           02  W-AUNA  REDEFINES W-UNA.
             03  W-UNAD  OCCURS  18.
               04  W-UN       PIC  N(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKKBM.
           COPY LIKHM.
           COPY LITM.
           COPY LIJM.
      *FD  URI-F
       01  URI-F_KHD020.
           02  URI-F_PNAME1   PIC  X(004) VALUE "URIF".
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_KHD020".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R.
           02  U-DC           PIC  9(001).
           02  U-DATE.
             03  F            PIC  9(002).
             03  U-NGP.
               04  U-NEN      PIC  9(002).
               04  U-GET      PIC  9(002).
               04  U-PEY      PIC  9(002).
           02  U-TCD          PIC  9(004).
           02  U-HCD          PIC  X(005).
           02  U-SU           PIC S9(006)V9(02).
           02  U-T            PIC S9(006)V9(02).
           02  U-KIN          PIC S9(008).
           02  U-YC           PIC  9(002).
           02  U-SD           PIC  9(004).
           02  U-NNO          PIC  X(006).
           02  U-HYC          PIC  9(001).
           02  U-CSC          PIC  9(001).
           02  U-BKC          PIC  9(002).
           02  U-SKD          PIC  9(008).
           02  U-DNO          PIC  9(006).
           02  U-GNO          PIC  9(001).
           02  U-JCD          PIC  9(006).
           02  U-GT           PIC S9(006)V9(02).
           02  U-TEK          PIC  N(018).
           02  U-BMC          PIC  9(001).
           02  F              PIC  X(003).
           02  U-PRC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  URIW-F
       01  URIW-F_KHD020.
           02  URIW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  URIW-F_LNAME   PIC  X(013) VALUE "URIW-F_KHD020".
           02  F              PIC  X(001).
           02  URIW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIW-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIW-F_RES     USAGE  POINTER.
       01  URIW-R.
           02  UW-DC          PIC  9(001).
           02  F              PIC  X(008).
           02  UW-TCD         PIC  9(004).
           02  UW-HCD         PIC  X(005).
           02  UW-SU          PIC S9(006)V9(02).
           02  UW-T           PIC S9(006)V9(02).
           02  UW-KIN         PIC S9(008).
           02  F              PIC  X(086).
       77  F                  PIC  X(001).
      *FD  URIR-F
       01  URIR-F_KHD020.
           02  URIR-F_PNAME1  PIC  X(006) VALUE "URIRYR".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHD020".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  UR-DC          PIC  9(001).
           02  UR-DATE.
             03  F            PIC  9(002).
             03  UR-NGP.
               04  UR-NEN     PIC  9(002).
               04  UR-GET     PIC  9(002).
               04  UR-PEY     PIC  9(002).
           02  UR-TCD         PIC  9(004).
           02  UR-HCD         PIC  X(005).
           02  UR-SU          PIC S9(006)V9(02).
           02  UR-T           PIC S9(006)V9(02).
           02  UR-KIN         PIC S9(008).
           02  UR-YC          PIC  9(002).
           02  UR-SD          PIC  9(004).
           02  UR-NNO         PIC  X(006).
           02  UR-HYC         PIC  9(001).
           02  UR-CSC         PIC  9(001).
           02  UR-BKC         PIC  9(002).
           02  UR-SKD         PIC  9(008).
           02  UR-DNO         PIC  9(006).
           02  UR-GNO         PIC  9(001).
           02  UR-JCD         PIC  9(006).
           02  UR-GT          PIC S9(006)V9(02).
           02  UR-TEK         PIC  N(018).
           02  UR-BMC         PIC  9(001).
           02  F              PIC  X(003).
           02  UR-PRC         PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  SP-R               PIC  X(206).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　工品その他・材料　売上伝票　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(028) VALUE
                "テスト印刷   ｽﾙ=9  ｼﾅｲ=1    ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  FILLER.
             03  A-SDNO  PIC  9(006).
             03  A-EDNO  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SHM.
             03  FILLER  PIC  N(005) VALUE
                  "（再発行）".
             03  FILLER  PIC  X(024) VALUE
                  "伝票№  000000 ～ 999999".
       01  C-ERR.
           02  FILLER.
             03  E-ECHK  PIC  9(001).
             03  E-ME    PIC  X(030).
             03  E-FILE  PIC  X(013).
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
           COPY LIBSCR.
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "372" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "13" "19" "28" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TPC" "9" "13" "46" "1" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "16" "0" "12" "A-TPC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SDNO" "9" "16" "29" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SDNO" BY REFERENCE W-SDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EDNO" "9" "16" "39" "6" "A-SDNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EDNO" BY REFERENCE W-EDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "39" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHM" " " "0" "0" "34" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHM" "N" "7" "28" "10" " " "D-SHM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHM" "X" "16" "21" "24" "01D-SHM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "88" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "88" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ECHK" "9" "24" "1" "1" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
        "E-ECHK" BY REFERENCE W-ECHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "30" "E-ECHK" " " RETURNING RESU.
       CALL "SD_From" USING
        "E-ME" BY REFERENCE W-MSG "30" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-FILE" "X" "24" "46" "13" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING
        "E-FILE" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME90" "N" "24" "5" "44" "E-FILE" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 0 AND 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-DATA.
           MOVE 999999 TO W-EDNO.
           IF  JS-SIGN = 0
               PERFORM DTC-RTN THRU DTC-EX
           END-IF
           IF  JS-SIGN = 0
               IF  W-DTC = 0
                   GO TO M-95
               END-IF
           END-IF
           MOVE SPACE TO W-P1 W-P2 W-P3 W-P4 W-P5 W-P5A W-P6 W-P7 W-P8.
           MOVE W-15K TO P-15K1 P-15K2 P-15K3.
           MOVE W-20K TO P-20K1.
           MOVE "'" TO P-NM.
           MOVE "№" TO P-NO.
           MOVE "**" TO P-SS.
           MOVE 9 TO P-DC.
           MOVE 99 TO P-NEN P-GET P-PEY.
           MOVE 9999 TO P-TCD.
           MOVE 999999 TO P-DNO P-JCD.
           MOVE ALL "X" TO P-UNO.
           MOVE ALL "Ｎ" TO P-JSU P-JSS P-OTNA P-UTNA
                                                P-HNA P-AZM P-TEK.
           MOVE 999999.99 TO P-SU P-T.
           MOVE 99999999 TO P-KIN P-TKIN.
           CALL "PR_Open" RETURNING RESP.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TPC = 1
               IF  JS-SIGN NOT = 1
                   GO TO M-25
               ELSE
                   GO TO M-15
               END-IF
           END-IF
           IF  W-TPC = 9
               PERFORM TST-RTN THRU TST-EX
           END-IF
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN NOT = 1
                   GO TO M-10
               ELSE
                   GO TO M-20
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
           COPY LIBCPR.
      *
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           IF  JS-SIGN = 0
               CALL "DB_F_Open" USING
                "I-O" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0"
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0"
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           IF  JS-SIGN = 1
               GO TO M-31
           END-IF.
       M-30.
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  U-PRC NOT = 0
               GO TO M-30
           END-IF
           INITIALIZE W-R.
           MOVE URI-R TO W-R.
           GO TO M-35.
       M-31.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  UR-DNO < W-SDNO OR > W-EDNO
               GO TO M-31
           END-IF
           INITIALIZE W-R.
           MOVE URIR-R TO W-R.
       M-35.
           MOVE WR-TCD TO W-TCD.
           MOVE WR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME T-JSU T-JSS T-UNO
               MOVE ZERO TO T-SS
               MOVE SPACE TO W-MSG
               MOVE "***  ﾄｸｲｻｷ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
      *
           MOVE SPACE TO W-P1 W-P2 W-P3 W-P4 W-P5 W-P6.
           MOVE W-15K TO P-15K1.
           MOVE W-20K TO P-20K1.
           MOVE "'" TO P-NM.
           MOVE "№" TO P-NO.
           IF  T-SS NOT = 20 AND 99
               IF  T-SS > 29
                   MOVE "**" TO P-SS
               ELSE
                   IF  T-SS NOT = ZERO
                       MOVE "*" TO P-SS
                   END-IF
               END-IF
           END-IF
           MOVE WR-DNO TO P-DNO.
           MOVE WR-NGP TO W-NGP.
           MOVE W-NEN TO P-NEN.
           MOVE W-GET TO P-GET.
           MOVE W-PEY TO P-PEY.
           MOVE WR-TCD TO P-TCD.
           MOVE T-UNO TO P-UNO.
           MOVE T-JSU TO P-JSU.
           MOVE T-JSS TO P-JSS.
           PERFORM TNA-RTN THRU TNA-EX.
           MOVE W-ONA TO P-OTNA.
           MOVE W-UNA TO P-UTNA.
           MOVE WR-DNO TO W-DNO.
           MOVE ZERO TO W-GKIN W-SHZ W-TKIN CHK.
           MOVE 14 TO W-AF.
           PERFORM MID-RTN THRU MID-EX.
       M-40.
           IF (W-AF = ZERO) OR ((WR-GNO = 1) AND (W-AF NOT = 14))
                            OR ((WR-GNO = 2) AND (W-AF NOT = 12))
                            OR ((WR-GNO = 3) AND (W-AF NOT = 10))
                            OR ((WR-GNO = 4) AND (W-AF NOT = 08))
                            OR ((WR-GNO = 5) AND (W-AF NOT = 06))
                            OR ((WR-GNO = 6) AND (W-AF NOT = 04))
               MOVE SPACE TO W-MSG
               MOVE "***  ｷﾞｮｳｽｳ ｴﾗｰ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE WR-T TO W-T.
           MOVE WR-KIN TO W-KIN.
           IF  WR-DC = 8 OR 9
               COMPUTE W-T = -1 * W-T
               COMPUTE W-KIN = -1 * W-KIN
           END-IF
           MOVE WR-TEK TO W-TEK.
           IF  WR-DC = 5 OR 9
               MOVE W-KIN TO W-SHZ
               MOVE 0 TO W-KIN
               GO TO M-50
           END-IF
           PERFORM HCD-RTN THRU HCD-EX.
           MOVE SPACE TO W-P7.
           MOVE SPACE TO P-HNA P-AZM.
           MOVE W-15K TO P-15K2.
           IF  WR-HCD = "00800"
               MOVE WR-JCD TO P-JCD
               MOVE J-NAME TO P-HNA
           ELSE
               MOVE WR-HCD TO P-HCD
               MOVE KH-NAME TO P-HNA
           END-IF
           IF  WR-SU NOT = ZERO
               MOVE WR-SU TO P-SU
           END-IF
           IF  WR-DC = 4
               MOVE "預り出荷" TO P-AZM
               GO TO M-42
           END-IF
           IF  W-T NOT = ZERO
               MOVE W-T TO P-T
           END-IF
           MOVE W-KIN TO P-KIN.
           ADD W-KIN TO W-GKIN.
           IF  WR-DC = 3
               MOVE "預り保管" TO P-AZM
           ELSE
               IF  WR-DC = 1
                   MOVE "返品　　" TO P-AZM
               ELSE
                   IF  WR-DC = 2
                       MOVE "不良返品" TO P-AZM
                   ELSE
                       IF  WR-DC = 8
                           MOVE "値引き　" TO P-AZM
                       END-IF
                   END-IF
               END-IF
           END-IF.
       M-42.
           MOVE SPACE TO SP-R.
           MOVE W-P7 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           SUBTRACT 2 FROM W-AF.
           IF  JS-SIGN = 0
               MOVE 9 TO U-PRC
      *               REWRITE URI-R.
      *///////////////
               CALL "DB_Update" USING
                URI-F_PNAME1 URI-F_LNAME URI-R RETURNING RET
           END-IF
           IF  JS-SIGN = 1
               GO TO M-46
           END-IF.
       M-45.
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO W-T W-KIN
               GO TO M-85
           END-IF
           IF  U-PRC NOT = 0
               GO TO M-45
           END-IF
           INITIALIZE W-R.
           MOVE URI-R TO W-R.
           IF  WR-DNO = W-DNO
               GO TO M-40
           END-IF
           MOVE 1 TO CHK.
           GO TO M-50.
       M-46.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO W-T W-KIN
               GO TO M-85
           END-IF
           IF  UR-DNO < W-SDNO OR > W-EDNO
               GO TO M-46
           END-IF
           INITIALIZE W-R.
           MOVE URIR-R TO W-R.
           IF  WR-DNO = W-DNO
               GO TO M-40
           END-IF
           MOVE 1 TO CHK.
       M-50.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  CHK NOT = 0
               GO TO M-35
           END-IF
           IF  JS-SIGN = 1
               GO TO M-31
           END-IF
           MOVE 9 TO U-PRC.
      *           REWRITE URI-R.
      *///////////////
           CALL "DB_Update" USING
            URI-F_PNAME1 URI-F_LNAME URI-R RETURNING RET.
           GO TO M-30.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
       M-90.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE URI-F_IDLST URI-F_PNAME1
           END-IF.
       M-95.
           IF  JS-SIGN = 0
               PERFORM KKB3-RTN THRU KKB3-EX
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DTC-RTN.
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
       DTC-010.
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               GO TO DTC-020
           END-IF
           IF  U-PRC NOT = 0
               GO TO DTC-010
           END-IF
           MOVE 1 TO W-DTC.
       DTC-020.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
       DTC-EX.
           EXIT.
       TST-RTN.
           PERFORM MID-RTN THRU MID-EX.
           MOVE ZERO TO CHK.
       TST-010.
           ADD 1 TO CHK.
           IF  CHK NOT = 7
               MOVE SPACE TO SP-R
               MOVE W-P7 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO TST-010
           END-IF
           MOVE ALL "Ｎ" TO P-TEK.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO P-TEK.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       TST-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5A TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P6 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       HCD-RTN.
           IF  WR-HCD = "00800"
               GO TO HCD-010
           END-IF
           MOVE WR-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KH-NAME
               MOVE SPACE TO W-MSG
               MOVE "***  ｺｳﾋﾝﾋﾝﾒｲ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO HCD-EX.
       HCD-010.
           MOVE WR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE SPACE TO W-MSG
               MOVE "***  ｻﾞｲﾘｮｳﾋﾝﾒｲ ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       HCD-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P8.
           MOVE W-15K TO P-15K3.
           MOVE SPACE TO P-TEK.
           MOVE W-TEK TO P-TEK.
           MOVE W-GKIN TO P-TKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING W-AF RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P8.
           MOVE W-15K TO P-15K3.
           MOVE SPACE TO P-TEK.
           MOVE W-SHZ TO P-TKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           COMPUTE W-TKIN = W-SHZ + W-GKIN.
      *
           MOVE SPACE TO W-P8.
           MOVE W-15K TO P-15K3.
           MOVE SPACE TO P-TEK.
           MOVE W-TKIN TO P-TKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       KEI-EX.
           EXIT.
       KKB3-RTN.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  KKBM ﾅｼ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KKB3-010
           END-IF
           MOVE 0 TO KKB-SC01.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-MSG
               MOVE "***  KKBM REWRITE ｴﾗｰ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       KKB3-010.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB3-EX.
           EXIT.
       TNA-RTN.
           MOVE SPACE TO W-NAME W-ONA W-UNA.
           MOVE T-NAME TO W-NAME.
           IF  W-RTNA = SPACE
               MOVE W-FTNA TO W-UNA
               GO TO TNA-EX
           END-IF
           IF  W-NM(08) = SPACE
               MOVE W-FTNA1 TO W-ONA
               MOVE 8 TO W-DC
               GO TO TNA-040
           END-IF
           IF  W-NM(19) = SPACE
               MOVE W-FTNA TO W-ONA
               MOVE 19 TO W-DC
               GO TO TNA-040
           END-IF
           IF (W-NM(09) NOT = SPACE) AND (W-NM(10) NOT = SPACE) AND
              (W-NM(11) NOT = SPACE) AND (W-NM(12) NOT = SPACE) AND
              (W-NM(13) NOT = SPACE) AND (W-NM(14) NOT = SPACE) AND
              (W-NM(15) NOT = SPACE) AND (W-NM(16) NOT = SPACE) AND
              (W-NM(17) NOT = SPACE) AND (W-NM(18) NOT = SPACE)
               MOVE W-FTNA TO W-ONA
               MOVE 18 TO W-DC
               GO TO TNA-040
           END-IF
      *
           MOVE W-FTNA1 TO W-ONA.
           MOVE 8 TO CNT1 CNT2.
       TNA-010.
           ADD 1 TO CNT1 CNT2.
           IF  CNT1 = 19
               MOVE 18 TO W-DC
               GO TO TNA-040
           END-IF
           MOVE W-NM(CNT1) TO W-ON(CNT2).
           IF  W-NM(CNT1) = SPACE
               MOVE CNT1 TO W-DC
               GO TO TNA-040
           END-IF
           MOVE SPACE TO W-NM(CNT1).
           GO TO TNA-010.
       TNA-040.
           MOVE 27 TO CNT1.
           MOVE 19 TO CNT2.
           MOVE 0 TO W-SC.
       TNA-050.
           SUBTRACT 1 FROM CNT1.
           IF  CNT1 = W-DC
               GO TO TNA-EX
           END-IF
           IF  W-SC = 0
               IF  W-NM(CNT1) = SPACE
                   GO TO TNA-050
               ELSE
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           SUBTRACT 1 FROM CNT2.
           MOVE W-NM(CNT1) TO W-UN(CNT2).
           GO TO TNA-050.
       TNA-EX.
           EXIT.
