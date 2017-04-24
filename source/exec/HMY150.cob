       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY150.
      **************************************************************
      *    PROGRAM         :  得意先月別販売対比ファイル　メンテ　 *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  SCHY15                               *
      *        変更　　　  :  95/09/21                             *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  WR-D.
           02  WR-KEY.
             03  WR-TCD       PIC  9(004).
             03  WR-IKC       PIC  9(001).
           02  WR-UD.
             03  WR-UK.
               04  WR-OUK1.
                 05  WR-OU1   OCCURS   6  PIC S9(009).
               04  WR-OUK2.
                 05  WR-OU2   OCCURS   6  PIC S9(009).
               04  WR-NUK1.
                 05  WR-NU1   OCCURS   6  PIC S9(009).
               04  WR-NUK2.
                 05  WR-NU2   OCCURS   6  PIC S9(009).
             03  WR-TU.
               04  WR-AOTU    PIC S9(010).
               04  WR-ANTU    PIC S9(010).
           02  WR-AD.
             03  WR-AK.
               04  WR-OAK1.
                 05  WR-OA1   OCCURS   6  PIC S9(009).
               04  WR-OAK2.
                 05  WR-OA2   OCCURS   6  PIC S9(009).
               04  WR-NAK1.
                 05  WR-NA1   OCCURS   6  PIC S9(009).
               04  WR-NAK2.
                 05  WR-NA2   OCCURS   6  PIC S9(009).
             03  WR-TA.
               04  WR-AOTA    PIC S9(010).
               04  WR-ANTA    PIC S9(010).
           02  WR-NG          PIC  9(006).
           02  WR-TC          PIC  9(002).
           02  WR-BC          PIC  9(001).
           02  F              PIC  X(026).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-NGD.
             03  W-NEND       PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-ANGD.
             03  W-ANG   OCCURS  24  PIC  9(004).
           02  W-GNGD  REDEFINES W-ANGD.
             03  W-GNG1  OCCURS   6.
               04  W-GNGD1    PIC  9(004).
             03  W-GNG2  OCCURS   6.
               04  W-GNGD2    PIC  9(004).
             03  W-GNG3  OCCURS   6.
               04  W-GNGD3    PIC  9(004).
             03  W-GNG4  OCCURS   6.
               04  W-GNGD4    PIC  9(004).
           02  W-GDD.
             03  W-GD    OCCURS   6  PIC S9(009).
           02  W-KIN          PIC S9(009).
           02  DCNT           PIC  9(002).
           02  GCNT           PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-LC.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
             03  W-L3         PIC  9(002).
             03  W-L4         PIC  9(002).
             03  W-L          PIC  9(002).
             03  W-C          PIC  9(002).
           02  W-KEY.
             03  W-TCD        PIC  9(004).
             03  W-IKC        PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-IKM          PIC  N(003).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
      *FD  TZNT-M
       01  TZNT-M_HMY150.
           02  TZNT-M_PNAME1  PIC  X(005) VALUE "TZNTM".
           02  F              PIC  X(001).
           02  TZNT-M_LNAME   PIC  X(013) VALUE "TZNT-M_HMY150".
           02  F              PIC  X(001).
           02  TZNT-M_KEY1    PIC  X(100) VALUE SPACE.
           02  TZNT-M_KEY2    PIC  X(100) VALUE SPACE.
           02  TZNT-M_SORT    PIC  X(100) VALUE SPACE.
           02  TZNT-M_IDLST   PIC  X(100) VALUE SPACE.
           02  TZNT-M_RES     USAGE  POINTER.
       01  TZNT-R.
           02  TZNT-KEY.
             03  TZNT-TCD     PIC  9(004).
             03  TZNT-IKC     PIC  9(001).
           02  TZNT-UD.
             03  TZNT-UK.
               04  TZNT-OUK1.
                 05  TZNT-OU1   OCCURS   6  PIC S9(009).
               04  TZNT-OUK2.
                 05  TZNT-OU2   OCCURS   6  PIC S9(009).
               04  TZNT-NUK1.
                 05  TZNT-NU1   OCCURS   6  PIC S9(009).
               04  TZNT-NUK2.
                 05  TZNT-NU2   OCCURS   6  PIC S9(009).
             03  TZNT-TU.
               04  TZNT-AOTU  PIC S9(010).
               04  TZNT-ANTU  PIC S9(010).
           02  TZNT-AD.
             03  TZNT-AK.
               04  TZNT-OAK1.
                 05  TZNT-OA1   OCCURS   6  PIC S9(009).
               04  TZNT-OAK2.
                 05  TZNT-OA2   OCCURS   6  PIC S9(009).
               04  TZNT-NAK1.
                 05  TZNT-NA1   OCCURS   6  PIC S9(009).
               04  TZNT-NAK2.
                 05  TZNT-NA2   OCCURS   6  PIC S9(009).
             03  TZNT-TA.
               04  TZNT-AOTA  PIC S9(010).
               04  TZNT-ANTA  PIC S9(010).
           02  TZNT-NG.
             03  TZNT-N       PIC  9(004).
             03  TZNT-G       PIC  9(002).
           02  TZNT-TC.
             03  TZNT-TC1     PIC  9(001).
             03  TZNT-TC2     PIC  9(001).
           02  TZNT-BC        PIC  9(001).
           02  F              PIC  X(026).
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-TCD   PIC  9(004).
             03  A-IKC   PIC  9(001).
           02  A-GD    PIC S9(009).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NEN.
             03  FILLER.
               04  02D-NEN  PIC 99/99 .
               04  03D-NEN  PIC 99/99 .
               04  04D-NEN  PIC 99/99 .
               04  05D-NEN  PIC 99/99 .
               04  06D-NEN  PIC 99/99 .
               04  07D-NEN  PIC 99/99 .
             03  FILLER.
               04  09D-NEN  PIC 99/99 .
               04  10D-NEN  PIC 99/99 .
               04  11D-NEN  PIC 99/99 .
               04  12D-NEN  PIC 99/99 .
               04  13D-NEN  PIC 99/99 .
               04  14D-NEN  PIC 99/99 .
             03  FILLER.
               04  16D-NEN  PIC 99/99 .
               04  17D-NEN  PIC 99/99 .
               04  18D-NEN  PIC 99/99 .
               04  19D-NEN  PIC 99/99 .
               04  20D-NEN  PIC 99/99 .
               04  21D-NEN  PIC 99/99 .
             03  FILLER.
               04  23D-NEN  PIC 99/99 .
               04  24D-NEN  PIC 99/99 .
               04  25D-NEN  PIC 99/99 .
               04  26D-NEN  PIC 99/99 .
               04  27D-NEN  PIC 99/99 .
               04  28D-NEN  PIC 99/99 .
           02  FILLER.
             03  D-NAME  PIC  N(026).
             03  D-IKM   PIC  N(003).
           02  D-GD    PIC ZZZZZZZZ9- .
           02  D-TOTAL.
             03  01D-TOTAL  PIC ZZZZZZZZZ9- .
             03  02D-TOTAL  PIC ZZZZZZZZZ9- .
             03  03D-TOTAL  PIC ZZZZZZZZZ9- .
             03  04D-TOTAL  PIC ZZZZZZZZZ9- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(014) VALUE
                  "現在使用　不可".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  DATA ｱﾘ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(020) VALUE
                  "***  DELETE ｴﾗｰ  ***".
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
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
           "C-ACP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-ACT" "9" "3" "51" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "5" "0" "5" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD" "9" "5" "9" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-IKC" "9" "5" "72" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-IKC" BY REFERENCE W-IKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-GD" "S9" "W-L" "W-C" "9" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-GD" BY REFERENCE W-GD(1) "9" "1" BY REFERENCE CNT 9
            RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "51" "1" "A-GD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "232" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NEN" " " "0" "0" "120" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
        "01D-NEN" " " "W-L1" "0" "30" " " "D-NEN" RETURNING RESU.
       CALL "SD_Init" USING
        "02D-NEN" "R99/99" "W-L1" "7" "5" " " "01D-NEN" RETURNING RESU.
       CALL "SD_From" USING
        "02D-NEN" BY REFERENCE W-GNGD1(1) "4" "1" "1" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "03D-NEN" "R99/99" "W-L1" "18" "5" "02D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "03D-NEN" BY REFERENCE W-GNGD1(1) "4" "1" "2" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "04D-NEN" "R99/99" "W-L1" "29" "5" "03D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "04D-NEN" BY REFERENCE W-GNGD1(1) "4" "1" "3" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "05D-NEN" "R99/99" "W-L1" "40" "5" "04D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "05D-NEN" BY REFERENCE W-GNGD1(1) "4" "1" "4" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "06D-NEN" "R99/99" "W-L1" "51" "5" "05D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "06D-NEN" BY REFERENCE W-GNGD1(1) "4" "1" "5" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "07D-NEN" "R99/99" "W-L1" "62" "5" "06D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "07D-NEN" BY REFERENCE W-GNGD1(1) "4" "1" "6" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "08D-NEN" " " "W-L2" "0" "30" "01D-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING
        "09D-NEN" "R99/99" "W-L2" "7" "5" " " "08D-NEN" RETURNING RESU.
       CALL "SD_From" USING
        "09D-NEN" BY REFERENCE W-GNGD2(1) "4" "1" "1" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "10D-NEN" "R99/99" "W-L2" "18" "5" "09D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "10D-NEN" BY REFERENCE W-GNGD2(1) "4" "1" "2" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "11D-NEN" "R99/99" "W-L2" "29" "5" "10D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "11D-NEN" BY REFERENCE W-GNGD2(1) "4" "1" "3" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "12D-NEN" "R99/99" "W-L2" "40" "5" "11D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "12D-NEN" BY REFERENCE W-GNGD2(1) "4" "1" "4" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "13D-NEN" "R99/99" "W-L2" "51" "5" "12D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "13D-NEN" BY REFERENCE W-GNGD2(1) "4" "1" "5" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "14D-NEN" "R99/99" "W-L2" "62" "5" "13D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "14D-NEN" BY REFERENCE W-GNGD2(1) "4" "1" "6" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "15D-NEN" " " "W-L3" "0" "30" "08D-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING
        "16D-NEN" "R99/99" "W-L3" "7" "5" " " "15D-NEN" RETURNING RESU.
       CALL "SD_From" USING
        "16D-NEN" BY REFERENCE W-GNGD3(1) "4" "1" "1" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "17D-NEN" "R99/99" "W-L3" "18" "5" "16D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "17D-NEN" BY REFERENCE W-GNGD3(1) "4" "1" "2" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "18D-NEN" "R99/99" "W-L3" "29" "5" "17D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "18D-NEN" BY REFERENCE W-GNGD3(1) "4" "1" "3" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "19D-NEN" "R99/99" "W-L3" "40" "5" "18D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "19D-NEN" BY REFERENCE W-GNGD3(1) "4" "1" "4" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "20D-NEN" "R99/99" "W-L3" "51" "5" "19D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "20D-NEN" BY REFERENCE W-GNGD3(1) "4" "1" "5" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "21D-NEN" "R99/99" "W-L3" "62" "5" "20D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "21D-NEN" BY REFERENCE W-GNGD3(1) "4" "1" "6" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "22D-NEN" " " "W-L4" "0" "30" "15D-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING
        "23D-NEN" "R99/99" "W-L4" "7" "5" " " "22D-NEN" RETURNING RESU.
       CALL "SD_From" USING
        "23D-NEN" BY REFERENCE W-GNGD4(1) "4" "1" "1" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "24D-NEN" "R99/99" "W-L4" "18" "5" "23D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "24D-NEN" BY REFERENCE W-GNGD4(1) "4" "1" "2" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "25D-NEN" "R99/99" "W-L4" "29" "5" "24D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "25D-NEN" BY REFERENCE W-GNGD4(1) "4" "1" "3" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "26D-NEN" "R99/99" "W-L4" "40" "5" "25D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "26D-NEN" BY REFERENCE W-GNGD4(1) "4" "1" "4" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "27D-NEN" "R99/99" "W-L4" "51" "5" "26D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "27D-NEN" BY REFERENCE W-GNGD4(1) "4" "1" "5" 4 RETURNING RESU.
       CALL "SD_Init" USING
        "28D-NEN" "R99/99" "W-L4" "62" "5" "27D-NEN" " " RETURNING RESU.
       CALL "SD_From" USING
        "28D-NEN" BY REFERENCE W-GNGD4(1) "4" "1" "6" 4 RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "5" "0" "58" "D-NEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "5" "14" "52" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-IKM" "N" "5" "74" "6" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-IKM" BY REFERENCE W-IKM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-GD" "ZZZZZZZZ9-" "W-L" "W-C" "10" "01C-DSP" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "D-GD" BY REFERENCE W-GD(1) "9" "1" CNT 9 RETURNING RESU.
       CALL "SD_Init" USING
           "D-TOTAL" " " "0" "0" "44" "D-GD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TOTAL" "ZZZZZZZZZ9-" "9" "68" "11" " " "D-TOTAL"
            RETURNING RESU.
       CALL "SD_From" USING
           "01D-TOTAL" BY REFERENCE WR-AOTU "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TOTAL" "ZZZZZZZZZ9-" "13" "68" "11" "01D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TOTAL" BY REFERENCE WR-ANTU "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TOTAL" "ZZZZZZZZZ9-" "17" "68" "11" "02D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-TOTAL" BY REFERENCE WR-AOTA "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-TOTAL" "ZZZZZZZZZ9-" "21" "68" "11" "03D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-TOTAL" BY REFERENCE WR-ANTA "10" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "108" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "108" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "14" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "X" "24" "15" "19" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME12" "X" "24" "15" "21" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME13" "X" "24" "15" "20" "E-ME12" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           PERFORM S-020 THRU S-060.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHY15" RETURNING RESU.
           MOVE  6 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  8 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 10 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE 12 TO W-L4.
           CALL "SD_Arg_Match_Line" USING
            "W-L4" "2" W-L4 RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p" RETURNING RESU.
           MOVE 14 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 16 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 18 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE 20 TO W-L4.
           CALL "SD_Arg_Match_Line" USING
            "W-L4" "2" W-L4 RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
             "I-O" TZNT-M_PNAME1 "SHARED" BY REFERENCE TZNT-M_IDLST "1"
            "TZNT-KEY" BY REFERENCE TZNT-KEY.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHY15" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE  6 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE  8 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 10 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE 12 TO W-L4.
           CALL "SD_Arg_Match_Line" USING
            "W-L4" "2" W-L4 RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p" RETURNING RESU.
           MOVE 14 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 16 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 18 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE 20 TO W-L4.
           CALL "SD_Arg_Match_Line" USING
            "W-L4" "2" W-L4 RETURNING RESU.
           CALL "SD_Output" USING "D-NEN" D-NEN "p" RETURNING RESU.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-080
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  T-BC NOT = 0
               MOVE 0 TO W-IKC
               MOVE "工品他" TO W-IKM
               CALL "SD_Output" USING "A-IKC" A-IKC "p" RETURNING RESU
               CALL "SD_Output" USING "D-IKM" D-IKM "p" RETURNING RESU
               GO TO M-120
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-IKC "A-IKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           IF  W-IKC NOT = 1 AND 2 AND 3
               GO TO M-100
           END-IF
           IF  W-IKC = 1
               MOVE "一　般" TO W-IKM
           END-IF
           IF  W-IKC = 2
               MOVE "教　育" TO W-IKM
           END-IF
           IF  W-IKC = 3
               MOVE "ＶＩＶ" TO W-IKM
           END-IF
           CALL "SD_Output" USING "D-IKM" D-IKM "p" RETURNING RESU.
       M-120.
           MOVE ZERO TO WR-D.
           MOVE W-KEY TO TZNT-KEY.
      *           READ TZNT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TZNT-M_PNAME1 BY REFERENCE TZNT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-140
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           MOVE TZNT-R TO WR-D.
           PERFORM S-080 THRU S-140.
           IF  W-ACT = 3
               GO TO M-320
           END-IF
           GO TO M-160.
       M-140.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           MOVE W-KEY TO WR-KEY.
           PERFORM S-080 THRU S-140.
       M-160.
           MOVE ZERO TO GCNT.
       M-180.
           ADD 1 TO GCNT.
           IF  GCNT = 9
               GO TO M-320
           END-IF
           PERFORM S-160 THRU S-200.
           MOVE ZERO TO CNT.
           MOVE 2 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       M-200.
           ADD 1 TO CNT.
           IF  CNT = 7
               GO TO M-180
           END-IF.
       M-220.
           MOVE W-GD(CNT) TO W-KIN.
           CALL "SD_Accept" USING BY REFERENCE A-GD "A-GD" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF22 OR PF7
               MOVE W-KIN TO W-GD(CNT)
               CALL "SD_Output" USING "D-GD" D-GD "p" RETURNING RESU
               GO TO M-260
           END-IF
           IF  ESTAT = PF21 OR PF6
               MOVE W-KIN TO W-GD(CNT)
               CALL "SD_Output" USING "D-GD" D-GD "p" RETURNING RESU
               GO TO M-280
           END-IF
           IF  ESTAT = ADV
               MOVE W-KIN TO W-GD(CNT)
               CALL "SD_Output" USING "D-GD" D-GD "p" RETURNING RESU
               GO TO M-300
           END-IF
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           PERFORM S-220 THRU S-240.
           CALL "SD_Output" USING "D-GD" D-GD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TOTAL" D-TOTAL "p" RETURNING RESU.
           ADD 11 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-200.
       M-240.
           SUBTRACT 1 FROM CNT.
           IF  CNT NOT = 0
               SUBTRACT 11 FROM W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               GO TO M-220
           END-IF
           SUBTRACT 1 FROM GCNT.
           IF  GCNT = 0
               GO TO M-080
           END-IF
           PERFORM S-160 THRU S-200.
           MOVE 6 TO CNT.
           MOVE 57 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-220.
       M-260.
           PERFORM S-220 THRU S-240.
           GO TO M-180.
       M-280.
           PERFORM S-220 THRU S-240.
           SUBTRACT 1 FROM GCNT.
           IF  GCNT = 0
               GO TO M-080
           END-IF
           PERFORM S-160 THRU S-200.
           MOVE 1 TO CNT.
           MOVE 2 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-220.
       M-300.
           PERFORM S-220 THRU S-240.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-340
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-DMM = 9
               GO TO M-160
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-320
           END-IF
           GO TO M-360.
       M-340.
           IF  W-ACT = 3
               GO TO M-080
           END-IF
           MOVE 8 TO GCNT.
           PERFORM S-160 THRU S-200.
           MOVE 6 TO CNT.
           MOVE 57 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-220.
       M-360.
           IF  W-ACT = 3
               GO TO M-400
           END-IF
           MOVE WR-D TO TZNT-R.
           IF  W-ACT NOT = 1
               GO TO M-380
           END-IF
      *           WRITE TZNT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-060.
       M-380.
      *           REWRITE TZNT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TZNT-M_PNAME1 TZNT-M_LNAME TZNT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-060.
       M-400.
      *           DELETE TZNT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TZNT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-060.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TZNT-M_IDLST TZNT-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *****  日付セット　
       S-020.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE D-NKNG TO W-NGD.
           IF  W-NGS NOT = W-NGD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           IF  W-GET < 5
               COMPUTE W-NEND = W-NEN - 1
           END-IF
           IF  W-NEND = ZERO
               MOVE 99 TO W-NEND
           ELSE
               SUBTRACT 1 FROM W-NEND
           END-IF
           MOVE 4 TO W-GETD.
           MOVE ZERO TO DCNT W-ANGD.
       S-040.
           ADD 1 TO W-GETD DCNT.
           IF  DCNT = 25
               GO TO S-060
           END-IF
           IF  W-GETD = 13
               MOVE 1 TO W-GETD
               ADD 1 TO W-NEND
           END-IF
           MOVE W-NGD TO W-ANG(DCNT).
           GO TO S-040.
       S-060.
           EXIT.
      *****   画面表示
       S-080.
           MOVE W-NG TO WR-NG.
           MOVE T-TNC TO WR-TC.
           MOVE T-BC TO WR-BC.
           MOVE ZERO TO GCNT.
       S-100.
           ADD 1 TO GCNT.
           IF  GCNT = 9
               CALL "SD_Output" USING
                "D-TOTAL" D-TOTAL "p" RETURNING RESU
               GO TO S-140
           END-IF
           PERFORM S-160 THRU S-200.
           MOVE ZERO TO CNT.
           MOVE 2 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       S-120.
           ADD 1 TO CNT.
           IF  CNT NOT = 7
               CALL "SD_Output" USING "D-GD" D-GD "p" RETURNING RESU
               ADD 11 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               GO TO S-120
           END-IF
           GO TO S-100.
       S-140.
           EXIT.
      *****   行ＤＡＴＡ　入力ＤＡＴＡセット
       S-160.
           MOVE ZERO TO W-GDD.
       S-180.
           IF  GCNT = 1
               MOVE  7 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-OUK1 TO W-GDD
           END-IF
           IF  GCNT = 2
               MOVE  9 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-OUK2 TO W-GDD
           END-IF
           IF  GCNT = 3
               MOVE 11 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-NUK1 TO W-GDD
           END-IF
           IF  GCNT = 4
               MOVE 13 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-NUK2 TO W-GDD
           END-IF
           IF  GCNT = 5
               MOVE 15 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-OAK1 TO W-GDD
           END-IF
           IF  GCNT = 6
               MOVE 17 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-OAK2 TO W-GDD
           END-IF
           IF  GCNT = 7
               MOVE 19 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-NAK1 TO W-GDD
           END-IF
           IF  GCNT = 8
               MOVE 21 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               MOVE WR-NAK2 TO W-GDD
           END-IF.
       S-200.
           EXIT.
      *****   合計セット
       S-220.
           IF  GCNT = 1
               MOVE W-GDD TO WR-OUK1
           END-IF
           IF  GCNT = 2
               MOVE W-GDD TO WR-OUK2
           END-IF
           IF  GCNT = 3
               MOVE W-GDD TO WR-NUK1
           END-IF
           IF  GCNT = 4
               MOVE W-GDD TO WR-NUK2
           END-IF
           IF  GCNT = 5
               MOVE W-GDD TO WR-OAK1
           END-IF
           IF  GCNT = 6
               MOVE W-GDD TO WR-OAK2
           END-IF
           IF  GCNT = 7
               MOVE W-GDD TO WR-NAK1
           END-IF
           IF  GCNT = 8
               MOVE W-GDD TO WR-NAK2
           END-IF
           COMPUTE WR-AOTU = WR-OU1(1) + WR-OU1(2) + WR-OU1(3) +
                             WR-OU1(4) + WR-OU1(5) + WR-OU1(6) +
                             WR-OU2(1) + WR-OU2(2) + WR-OU2(3) +
                             WR-OU2(4) + WR-OU2(5) + WR-OU2(6).
           COMPUTE WR-ANTU = WR-NU1(1) + WR-NU1(2) + WR-NU1(3) +
                             WR-NU1(4) + WR-NU1(5) + WR-NU1(6) +
                             WR-NU2(1) + WR-NU2(2) + WR-NU2(3) +
                             WR-NU2(4) + WR-NU2(5) + WR-NU2(6).
           COMPUTE WR-AOTA = WR-OA1(1) + WR-OA1(2) + WR-OA1(3) +
                             WR-OA1(4) + WR-OA1(5) + WR-OA1(6) +
                             WR-OA2(1) + WR-OA2(2) + WR-OA2(3) +
                             WR-OA2(4) + WR-OA2(5) + WR-OA2(6).
           COMPUTE WR-ANTA = WR-NA1(1) + WR-NA1(2) + WR-NA1(3) +
                             WR-NA1(4) + WR-NA1(5) + WR-NA1(6) +
                             WR-NA2(1) + WR-NA2(2) + WR-NA2(3) +
                             WR-NA2(4) + WR-NA2(5) + WR-NA2(6).
       S-240.
           EXIT.
