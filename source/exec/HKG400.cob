       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG410.
      *********************************************************
      *    PROGRAM         :  部門別日計表                    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　部門別　日計　集計表　　＊＊＊".
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(025) VALUE
                "<----------------------  ".
           02  F              PIC  N(004) VALUE "売　　上".
           02  F              PIC  X(025) VALUE
                "  ---------------------->".
           02  F              PIC  X(008) VALUE "<-----  ".
           02  F              PIC  N(004) VALUE "入　　金".
           02  F              PIC  X(020) VALUE "  ----><----------  ".
           02  F              PIC  N(007) VALUE "売　掛　残　高".
           02  F              PIC  X(012) VALUE "  --------->".
       01  HEAD3.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(009) VALUE "履　物　　　工　品".
           02  F              PIC  N(006) VALUE "　　　合　計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "材　料".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "合　計".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  P-M     REDEFINES P-DATE  PIC  N(004).
           02  P-HA           PIC ----,---,--9.
           02  P-KO           PIC ----,---,--9.
           02  P-TO           PIC ----,---,--9.
           02  P-ZA           PIC ---,---,--9.
           02  P-SH           PIC ---,---,--9.
           02  P-UN           PIC ----,---,--9.
           02  P-SN           PIC ---,---,--9.
           02  P-UZ           PIC --,---,---,--9.
           02  P-SZ           PIC ---,---,--9.
           02  P-ZT           PIC --,---,---,--9.
       01  W-DATA.
           02  W-UZ           PIC S9(010).
           02  W-SZ           PIC S9(008).
           02  W-MUZ          PIC S9(010).
           02  W-MSZ          PIC S9(008).
           02  W-MZG          PIC S9(010).
           02  W-ZT           PIC S9(010).
           02  W-DATE         PIC  9(006).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NG         PIC  9(004).
             03  W-PEY        PIC  9(002).
           02 WNA-D.
             03  WN-D     OCCURS  31.
               04  WN-HA      PIC S9(009).
               04  WN-KO      PIC S9(009).
               04  WN-TO      PIC S9(009).
               04  WN-ZA      PIC S9(008).
               04  WN-SH      PIC S9(008).
               04  WN-UN      PIC S9(009).
               04  WN-SN      PIC S9(008).
           02  WT-D.
             03  WT-HA        PIC S9(009).
             03  WT-KO        PIC S9(009).
             03  WT-TO        PIC S9(009).
             03  WT-ZA        PIC S9(008).
             03  WT-SH        PIC S9(008).
             03  WT-UN        PIC S9(009).
             03  WT-SN        PIC S9(008).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITTM.
           COPY LSPF.
           COPY LSNYUR.
      *FD  SNTR-F
       01  SNTR-F_HKG400.
           02  SNTR-F_PNAME1  PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTR-F_LNAME   PIC  X(013) VALUE "SNTR-F_HKG400".
           02  F              PIC  X(001).
           02  SNTR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SNTR-F_SORT    PIC  X(100) VALUE SPACE.
           02  SNTR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SNTR-F_RES     USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  F              PIC  X(006).
           02  SNTR-PEY       PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-D1.
             03  SNTR-HCD     PIC  9(006).
             03  F            PIC  X(031).
             03  SNTR-SU      PIC S9(005).
             03  F            PIC  X(005).
             03  SNTR-KIN     PIC S9(008).
             03  F            PIC  X(001).
             03  SNTR-DC      PIC  9(001).
             03  SNTR-FT      PIC  9(005).
             03  F            PIC  X(003).
             03  SNTR-BC      PIC  9(002).
             03  F            PIC  X(001).
             03  SNTR-NC      PIC  9(001).
             03  F            PIC  X(003).
             03  SNTR-TNC     PIC  9(002).
             03  F            PIC  X(034).
           02  SNTR-D2    REDEFINES SNTR-D1.
             03  F            PIC  X(084).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  X(017).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  URIR-F
       01  URIR-F_HKG400.
           02  URIR-F_PNAME1  PIC  X(005) VALUE "URIRF".
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_HKG400".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  UR-DC          PIC  9(001).
           02  F              PIC  X(006).
           02  UR-PEY         PIC  9(002).
           02  F              PIC  X(004).
           02  UR-HCD         PIC  X(005).
           02  F              PIC  X(016).
           02  UR-KIN         PIC S9(008).
           02  UR-YC          PIC  9(002).
           02  F              PIC  X(084).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　部門別　日計　集計表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-UZ.
             03  FILLER  PIC  N(013) VALUE
                  "［　マスターの売掛残高　］".
             03  FILLER  PIC  N(021) VALUE
                  "　　　　売　上　　　　消費税　　　　合　計".
             03  FILLER.
               04  FILLER  PIC --,---,---,--9.
               04  FILLER  PIC --,---,---,--9.
               04  FILLER  PIC --,---,---,--9.
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "110" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UZ" " " "0" "0" "110" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-UZ" "RN" "14" "15" "26" " " "D-UZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-UZ" "N" "16" "7" "42" "01D-UZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-UZ" " " "17" "0" "42" "02D-UZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-UZ" "--,---,---,--9" "17" "7" "14" " " "03D-UZ"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-UZ" BY REFERENCE W-MUZ "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-UZ" "--,---,---,--9" "17" "21" "14" "0103D-UZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-UZ" BY REFERENCE W-MSZ "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-UZ" "--,---,---,--9" "17" "35" "14" "0203D-UZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-UZ" BY REFERENCE W-MZG "10" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
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
           MOVE ZERO TO W-DATA.
      *
           PERFORM TTM-RTN THRU TTM-EX.
      *
           PERFORM SNF-RTN THRU SNF-EX.
      *
           PERFORM NYU-RTN THRU NYU-EX.
      *
           PERFORM URI-RTN THRU URI-EX.
      *-----------------------------------------------------------------
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE D-NHNG TO W-NG.
           CALL "PR_Open" RETURNING RESP.
      *
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           MOVE SPACE TO SP-R W-P.
           COMPUTE W-ZT = W-UZ + W-SZ.
           MOVE W-UZ TO P-UZ.
           MOVE W-SZ TO P-SZ.
           MOVE W-ZT TO P-ZT.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-20.
           ADD 1 TO W-PEY.
           IF  W-PEY > 31
               GO TO M-90
           END-IF
           IF  ZERO = WN-HA(W-PEY) AND WN-KO(W-PEY) AND WN-ZA(W-PEY)
                 AND WN-SH(W-PEY) AND WN-UN(W-PEY) AND WN-SN(W-PEY)
               GO TO M-20
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
           GO TO M-20.
       M-90.
           MOVE SPACE TO W-P.
           MOVE "合　　計" TO P-M.
           MOVE WT-HA TO P-HA.
           MOVE WT-KO TO P-KO.
           MOVE WT-TO TO P-TO.
           MOVE WT-ZA TO P-ZA.
           MOVE WT-SH TO P-SH.
           MOVE WT-UN TO P-UN.
           MOVE WT-SN TO P-SN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
           IF  W-UZ = W-MUZ
               IF  W-SZ = W-MSZ
                   IF  W-ZT = W-MZG
                       GO TO M-95
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-UZ" D-UZ "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       TTM-RTN.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
       TTM-010.
      *           READ TT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
               GO TO TTM-EX
           END-IF
           ADD TT-TZZ TO W-UZ.
           ADD TT-TZZZ TO W-SZ.
           ADD TT-TUZ TO W-MUZ W-MZG.
           ADD TT-TUZZ TO W-MSZ W-MZG.
           GO TO TTM-010.
       TTM-EX.
           EXIT.
       SNF-RTN.
           CALL "DB_F_Open" USING
            "INPUT" SNTR-F_PNAME1 " " BY REFERENCE SNTR-F_IDLST "0".
       SNF-010.
      *           READ SNTR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SNTR-F_PNAME1 BY REFERENCE SNTR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTR-F_IDLST SNTR-F_PNAME1
               GO TO SNF-EX
           END-IF
           IF  SNTR-GNO = 9
               GO TO SNF-020
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO SNF-010
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               SUBTRACT SNTR-KIN FROM WN-HA(SNTR-PEY) WN-TO(SNTR-PEY)
           ELSE
               ADD SNTR-KIN TO WN-HA(SNTR-PEY) WN-TO(SNTR-PEY)
           END-IF
           GO TO SNF-010.
       SNF-020.
           IF  SNTR-SHZ = ZERO
               GO TO SNF-010
           END-IF
           IF  SNTR-SNC = ZERO
               ADD SNTR-SHZ TO WN-SH(SNTR-PEY)
           ELSE
               SUBTRACT SNTR-SHZ FROM WN-SH(SNTR-PEY)
           END-IF
           GO TO SNF-010.
       SNF-EX.
           EXIT.
       NYU-RTN.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
       NYU-010.
      *           READ NYUR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               GO TO NYU-EX
           END-IF
           IF  NUR-KIN = ZERO
               GO TO NYU-010
           END-IF
           IF  NUR-NC2 > 7
               ADD NUR-KIN TO WN-SN(NUR-PEY)
           ELSE
               ADD NUR-KIN TO WN-UN(NUR-PEY)
           END-IF
           GO TO NYU-010.
       NYU-EX.
           EXIT.
       URI-RTN.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       URI-010.
      *           READ URIR-F AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE URIR-F_IDLST URIR-F_PNAME1
               GO TO URI-EX
           END-IF
           IF  UR-KIN = ZERO
               GO TO URI-010
           END-IF
           IF  UR-DC = 5
               ADD UR-KIN TO WN-SH(UR-PEY)
               GO TO URI-010
           END-IF
           IF  UR-DC = 9
               SUBTRACT UR-KIN FROM WN-SH(UR-PEY)
               GO TO URI-010
           END-IF
           IF  UR-DC > 7
               GO TO URI-020
           END-IF
           IF  UR-YC = 00
               ADD UR-KIN TO WN-ZA(UR-PEY)
           ELSE
               ADD UR-KIN TO WN-KO(UR-PEY) WN-TO(UR-PEY)
           END-IF
           GO TO URI-010.
       URI-020.
           IF  UR-YC = 00
               SUBTRACT UR-KIN FROM WN-ZA(UR-PEY)
           ELSE
               SUBTRACT UR-KIN FROM WN-KO(UR-PEY) WN-TO(UR-PEY)
           END-IF
           GO TO URI-010.
       URI-EX.
           EXIT.
       PRI-RTN.
           COMPUTE W-UZ = WN-TO(W-PEY) + WN-ZA(W-PEY) - WN-UN(W-PEY)
                                                      + W-UZ.
           COMPUTE W-SZ = WN-SH(W-PEY) - WN-SN(W-PEY) + W-SZ.
           COMPUTE W-ZT = W-UZ + W-SZ.
      *
           MOVE SPACE TO W-P.
           MOVE W-DATE TO P-DATE.
           MOVE WN-HA(W-PEY) TO P-HA.
           MOVE WN-KO(W-PEY) TO P-KO.
           MOVE WN-TO(W-PEY) TO P-TO.
           MOVE WN-ZA(W-PEY) TO P-ZA.
           MOVE WN-SH(W-PEY) TO P-SH.
           MOVE WN-UN(W-PEY) TO P-UN.
           MOVE WN-SN(W-PEY) TO P-SN.
           MOVE W-UZ TO P-UZ.
           MOVE W-SZ TO P-SZ.
           MOVE W-ZT TO P-ZT.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WN-HA(W-PEY) TO WT-HA.
           ADD WN-KO(W-PEY) TO WT-KO.
           ADD WN-TO(W-PEY) TO WT-TO.
           ADD WN-ZA(W-PEY) TO WT-ZA.
           ADD WN-SH(W-PEY) TO WT-SH.
           ADD WN-UN(W-PEY) TO WT-UN.
           ADD WN-SN(W-PEY) TO WT-SN.
       PRI-EX.
           EXIT.
