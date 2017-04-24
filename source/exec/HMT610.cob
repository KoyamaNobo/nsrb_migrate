       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT010.
       AUTHOR. 1996-10-07.
      ************************************
      ******    ›™•¨»•i@Žó•¥•\    ******
      ************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(009) VALUE 0.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-PEY.
             03  W-PEY1       PIC  9(002).
             03  W-PEY2       PIC  9(002).
           02  W-KEY          PIC  9(006).
           02  W-AK           PIC S9(008).
           02  W-ACT          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-SBC3         PIC  9(002).
           02  W-EBC3         PIC  9(002) VALUE 99.
           02  W-SBMNO        PIC  9(001).
           02  W-EBMNO        PIC  9(001) VALUE 9.
           02  W-SBC1         PIC  9(002).
           02  W-EBC1         PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
      *FD  HUH-M
       01  HUH-M_HMT610.
           02  HUH-M_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HUH-M_LNAME    PIC  X(012) VALUE "HUH-M_HMT610".
           02  F              PIC  X(001).
           02  HUH-M_KEY1     PIC  X(100) VALUE SPACE.
           02  HUH-M_SORT     PIC  X(100) VALUE SPACE.
           02  HUH-M_IDLST    PIC  X(100) VALUE SPACE.
           02  HUH-M_RES      USAGE  POINTER.
       01  HUH-R.
           02  HUH-KEY.
             03  HUH-HCD.
               04  HUH-HCD1   PIC  9(004).
               04  HUH-HCD2   PIC  9(002).
           02  HUH-NGD.
             03  F            PIC  9(002).
             03  HUH-NEN      PIC  9(002).
             03  HUH-GET      PIC  9(002).
           02  HUH-ZS         PIC S9(006).
           02  HUH-ZK         PIC S9(009).
           02  HUH-NS         PIC S9(007).
           02  HUH-NK         PIC S9(010).
           02  HUH-SS         PIC S9(008).
           02  HUH-SK         PIC S9(010).
           02  HUH-YS         PIC S9(006).
           02  HUH-YK         PIC S9(009).
           02  HUH-UG         PIC S9(010).
           02  HUH-BC1        PIC  9(002).
           02  HUH-BC2        PIC  9(002).
           02  HUH-BC3        PIC  9(002).
           02  HUH-BMC        PIC  9(002).
           02  HUH-BMNO       PIC  9(001).
           02  F              PIC  X(032).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(018) VALUE
                "–––@@—š•¨»•i@Žó•¥•\@@–––".
           02  FILLER.
             03  FILLER  PIC  X(019) VALUE
                  "•ª—Þ‡B   00  `  99".
             03  FILLER  PIC  X(031) VALUE
                  "(ƒJƒWƒ…=10,ƒ[ƒN=20,‹³  ˆç=30)".
           02  FILLER.
             03  FILLER  PIC  X(019) VALUE
                  "•”–å‡‚    0  `   9".
             03  FILLER  PIC  X(038) VALUE
                  "(‘“à=1,ãŠC=2,Žd“ü=3,ƒ[ƒN=4,‹³ˆç=5)".
           02  FILLER  PIC  X(019) VALUE
                "•ª—Þ‡@   00  `  99".
           02  FILLER  PIC  X(022) VALUE
                "Šm”F  OK=1 NO=9   ØÀ°Ý".
       01  C-ACP.
           02  FILLER.
             03  A-SBC3  PIC  9(002).
             03  A-EBC3  PIC  9(002).
           02  FILLER.
             03  A-SBMNO PIC  9(001).
             03  A-EBMNO PIC  9(001).
           02  FILLER.
             03  A-SBC1  PIC  9(002).
             03  A-EBC1  PIC  9(002).
           02  A-DMM   PIC  9(001).
      *
           02  A-KEY   PIC  9(006).
           02  A-ACT   PIC  9(001).
       01  C-DSP.
           02  D-PEY.
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  Z(002).
           02  FILLER.
             03  D-DATA1.
               04  01D-DATA1  PIC  9(006).
               04  02D-DATA1  PIC  N(024).
               04  03D-DATA1  PIC --,---,--9 .
             03  D-DATA2.
               04  01D-DATA2  PIC ---,--9 .
               04  02D-DATA2  PIC ---,---,--9 .
               04  03D-DATA2  PIC ---,--9 .
               04  04D-DATA2  PIC ---,---,--9 .
               04  05D-DATA2  PIC ---,--9 .
               04  06D-DATA2  PIC ---,---,--9 .
               04  07D-DATA2  PIC ---,--9 .
               04  08D-DATA2  PIC ---,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC N(012) VALUE
                  "m@‚d‚m‚c@‚c‚`‚s‚`@n".
             03  E-STAT  PIC  X(010).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "184" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "1" "20" "36" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" " " "14" "0" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "X" "14" "21" "19" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "X" "14" "43" "31" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" " " "16" "0" "57" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "X" "16" "21" "19" " " "05C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "X" "16" "43" "38" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "18" "21" "19" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "23" "31" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC3" "9" "14" "30" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC3" BY REFERENCE W-SBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC3" "9" "14" "38" "2" "A-SBC3" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC3" BY REFERENCE W-EBC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "16" "0" "2" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBMNO" "9" "16" "31" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBMNO" BY REFERENCE W-SBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBMNO" "9" "16" "39" "1" "A-SBMNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBMNO" BY REFERENCE W-EBMNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03C-ACP" " " "18" "0" "4" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SBC1" "9" "18" "30" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SBC1" BY REFERENCE W-SBC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EBC1" "9" "18" "38" "2" "A-SBC1" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EBC1" BY REFERENCE W-EBC1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "48" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-KEY" "9" "5" "1" "6" "A-DMM" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-KEY" BY REFERENCE W-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ACT" "9" "23" "80" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "140" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PEY" " " "1" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PEY" "Z" "1" "65" "2" " " "D-PEY" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-PEY" BY REFERENCE W-PEY1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PEY" "Z" "1" "77" "2" "01D-PEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-PEY" BY REFERENCE W-PEY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "136" "D-PEY" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA1" " " "W-L" "0" "64" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA1" "9" "W-L" "1" "6" " " "D-DATA1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA1" BY REFERENCE HUH-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA1" "N" "W-L" "8" "48" "01D-DATA1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA1" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA1" "--,---,--9" "W-L" "71" "10" "02D-DATA1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATA1" BY REFERENCE W-AK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA2" " " "W-L" "0" "72" "D-DATA1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA2" "---,--9" "W-L" "9" "7" " " "D-DATA2"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA2" BY REFERENCE HUH-ZS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA2" "---,---,--9" "W-L" "16" "11" "01D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA2" BY REFERENCE HUH-ZK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA2" "---,--9" "W-L" "27" "7" "02D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATA2" BY REFERENCE HUH-NS "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DATA2" "---,---,--9" "W-L" "34" "11" "03D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DATA2" BY REFERENCE HUH-NK "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-DATA2" "---,--9" "W-L" "45" "7" "04D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-DATA2" BY REFERENCE HUH-SS "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-DATA2" "---,---,--9" "W-L" "52" "11" "05D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-DATA2" BY REFERENCE HUH-SK "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-DATA2" "---,--9" "W-L" "63" "7" "06D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-DATA2" BY REFERENCE HUH-YS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-DATA2" "---,---,--9" "W-L" "70" "11" "07D-DATA2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-DATA2" BY REFERENCE HUH-YK "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "10" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SBC3 "A-SBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EBC3 "A-EBC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SBC3 > W-EBC3
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SBMNO "A-SBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-EBMNO "A-EBMNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-SBMNO > W-EBMNO
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-SBC1 "A-SBC1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-EBC1 "A-EBC1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-SBC1 > W-EBC1
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-DMM = 9
               GO TO M-05
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-40
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT61" RETURNING RESU.
           COPY LIBCPR.
           MOVE D-HSP TO W-PEY1.
           MOVE D-HNP TO W-PEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HUH-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE 0 TO W-END.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-70
           END-IF
           IF  ESTAT = BTB
               GO TO M-70
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-55.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-60.
      *           READ HUH-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HUH-M_PNAME1 BY REFERENCE HUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO M-65
           END-IF
           IF  HUH-BC3 < W-SBC3
               GO TO M-60
           END-IF
           IF  HUH-BC3 > W-EBC3
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO M-65
           END-IF
           IF  HUH-BMNO < W-SBMNO OR > W-EBMNO
               GO TO M-60
           END-IF
           IF  HUH-BC1 < W-SBC1 OR > W-EBC1
               GO TO M-60
           END-IF
           IF  W-KEY NOT = ZERO
               IF  HUH-KEY = W-KEY
                   MOVE ZERO TO W-KEY
               ELSE
                   GO TO M-60
               END-IF
           END-IF
           COMPUTE W-AK = HUH-SK - HUH-UG.
           IF  ZERO = HUH-ZS AND HUH-ZK AND HUH-NS AND HUH-NK AND
                     HUH-SS AND HUH-SK AND W-AK
               GO TO M-60
           END-IF
           MOVE HUH-KEY TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE HUH-KEY TO HI-NAME
           END-IF
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
           CALL "SD_Output" USING "D-DATA1" D-DATA1 "p" RETURNING RESU.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-DATA2" D-DATA2 "p" RETURNING RESU.
           IF  W-END = 0
               ADD 1 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               IF  W-L < 23
                   GO TO M-60
               END-IF
           END-IF.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-END = 1
               GO TO M-70
           END-IF
           IF  ESTAT = PF9
               GO TO M-70
           END-IF
           IF  ESTAT = HTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCHT61" RETURNING RESU
               CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU
               GO TO M-55
           END-IF
           IF  ESTAT NOT = BTB
               GO TO M-65
           END-IF
           CALL "SD_Screen_Output" USING "SCHT61" RETURNING RESU.
           CALL "SD_Output" USING "D-PEY" D-PEY "p" RETURNING RESU.
           GO TO M-50.
       M-70.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  ESTAT NOT = PF9
               GO TO M-05
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
