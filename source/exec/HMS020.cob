       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMS020.
       AUTHOR. Y-KII.
      *********************************************************
      *    PROGRAM         :  荷札・入日記　入力　　　　      *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  SCHS02                          *
      *        変更　　　  :  62/05/25                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE      SECTION.
       77  ERR-STAT         PIC X(2).
       01  WN-R.
           02  WN-NO        PIC 9(3).
           02  WN-CO        PIC 9(7).
           02  WN-UCO       PIC 9.
           02  WN-CSU       PIC 9(3).
           02  WN-MSU       PIC 9(3).
           02  WN-TY        PIC N(36).
           02  WN-DATE      PIC 9(6).
           02  F            PIC X(07).
       01  WI-R.
           02  WI-NO        PIC 9(3).
           02  WI-CO        PIC 9(6).
           02  WI-SIZ.
             03  WI-SIZD   OCCURS 18 PIC X(3).
             03  WI-SSD    OCCURS 18 PIC 9(2).
           02  WI-GOKEI     PIC 9(2).
       01  WI-AD.
           02  WI-D         OCCURS 6 PIC X(101).
       01  TEIGI-1.
           02  WCODE         PIC 9(7).
           02  WSIZE-1       PIC X(3).
           02  WSURYOW-1     PIC 9(2).
           02  KAKUNIN-1     PIC 9.
       01  TEIGI-2.
           02  TATE.
             03  TATE-1      PIC 9(2).
             03  TATE-2      PIC 9(2).
             03  TATE-3      PIC 9(2).
           02  TATE-A.
             03  TATE-1A     PIC 9(2).
             03  TATE-2A     PIC 9(2).
             03  TATE-3A     PIC 9(2).
           02  YOKO.
             03  YOKO-1      PIC 9(2).
             03  YOKO-2      PIC 9(2).
           02  KAZU.
             03  KAZU-1      PIC 9.
             03  KAZU-2      PIC 9.
           02  CNT.
             03  CNT-1       PIC 9(2).
             03  CNT-2       PIC 9(2).
           02  NAMBER        PIC 9(3).
           02  JUMBAN        PIC 9.
           02  W-DCHK        PIC 9.
           02  W-CODE        PIC 9(7).
           02  W-CSU1        PIC 9(3).
           02  W-CSU2        PIC 9(3).
           02  W-UNSOW1      PIC 9.
           02  W-UNSOW2      PIC 9.
           02  W-TY          PIC N(36).
       01  TEIGI-3.
           02  W-SCHK.
             03  W-SCHK1     PIC  X(30) VALUE
                  "003002001000201301401280290300".
             03  W-SCHK2     PIC  X(30) VALUE
                  "125130135140150160170180190200".
             03  W-SCHK3     PIC X(30)  VALUE
                  "210215220225230235240245250   ".
             03  W-SCHK4     PIC X(30)  VALUE
                  "240245250255260265270275      ".
           02  W-SCHKD.
             03  W-SCHK9  OCCURS 10  PIC X(3).
           02  W-SC.
             03  W-SCD    OCCURS 10  PIC 9.
           02  W-SCNT.
             03  W-SCNT1.
               04  F       PIC X(42) VALUE
                    "125130135140150160170180190200210215220225".
               04  F       PIC X(39) VALUE
                    "230235240245250255260265270275280290300".
             03  W-SCNT2.
               04  F       PIC X(42) VALUE
                    "003002001000201301401999999999999999999999".
               04  F       PIC X(39) VALUE
                    "999999999999999999999999999999999999999".
           02  W-SCNTD.
             03  W-SCNT9   OCCURS 27   PIC X(3).
           02  W-UND.
             03  W-UN       PIC N(03).
             03  F          PIC N(03).
       01  SUWZI            PIC 9(2).
       01  W-DATA.
           02  W-NGP        PIC 9(6).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN      PIC 9(2).
             03  W-GET      PIC 9(2).
             03  W-PEY      PIC 9(2).
           COPY LSTAT.
      *
           COPY L-JCON.
           COPY LITCM.
           COPY LIHIM2.
      *FD  SNF-F
       01  SNF-F_HMS020.
           02  SNF-F_PNAME1   PIC  X(004) VALUE "SNFF".
           02  F              PIC  X(001).
           02  SNF-F_LNAME    PIC  X(012) VALUE "SNF-F_HMS020".
           02  F              PIC  X(001).
           02  SNF-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SNF-F_SORT     PIC  X(100) VALUE SPACE.
           02  SNF-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SNF-F_RES      USAGE  POINTER.
       01  SNF-R.
           02  SNF-NO         PIC 9(3).
           02  SNF-CO         PIC 9(7).
           02  SNF-UCO        PIC 9.
           02  SNF-CSU        PIC 9(3).
           02  SNF-MSU        PIC 9(3).
           02  SNF-TY         PIC N(36).
           02  SNF-DATE       PIC 9(06).
           02  F              PIC X(07).
       77  F                  PIC X(01).
      *FD  SIN-F
       01  SIN-F_HMS020.
           02  SIN-F_PNAME1   PIC  X(004) VALUE "SINF".
           02  F              PIC  X(001).
           02  SIN-F_LNAME    PIC  X(012) VALUE "SIN-F_HMS020".
           02  F              PIC  X(001).
           02  SIN-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SIN-F_SORT     PIC  X(100) VALUE SPACE.
           02  SIN-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SIN-F_RES      USAGE  POINTER.
       01  SIN-R.
           02  SIN-NO         PIC X(3).
           02  SIN-CO         PIC 9(6).
           02  SIN-SU      OCCURS 27 PIC 9(2).
           02  SIN-GOKEI      PIC 9(2).
           02  F              PIC X(20).
       77  F                  PIC X(01).
      *
       77  ENDS               PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  HYOJI1.
           02  HYO    PIC  X(12) VALUE "CLEAR SCREEN".
       01  NYURYOKU.
           02  NYURYOKU-0.
             03  A-DATE     PIC 9(6).
           02  NYURYOKU-1.
             03 SCODE       PIC 9(7).
             03 SNAME       PIC N(24).
             03 SUNSO-1     PIC 9 .
             03 SUNSO-2     PIC N(3).
             03 SUNSO-3     PIC 9 .
             03 SUNSO-5     PIC N(3).
             03 SCSU        PIC 9(3).
           02  NYURYOKU-2.
             03 SHCODE      PIC 9(6).
             03 SHNAME      PIC N(24).
           02  NYURYOKU-3.
             03 SSIZE       PIC 9(3).
           02  NYURYOKU-4.
             03 SSURYOW     PIC 9(2).
           02  NYURYOKU-5.
             03 STY         PIC N(36).
           02  NYURYOKU-6.
             03 SMSU        PIC 9(3).
           02  KAKUNIN.
             03 SKAKUNIN    PIC 9 .
       01  HYOJI.
           02  HYOJI3.
             03  SCSU-1     PIC ZZ9 .
           02  HYOJI4.
             03  SSURYOW-1  PIC Z9 .
           02  HYOJI5.
             03  SGOKEI     PIC ZZ9 .
           02  HYOJI6.
             03  SMSU-1     PIC ZZ9 .
       01  C-CLEAR.
           02  C-CLEAR1.
             03  C-SHCODE   PIC X(6) VALUE "      ".
             03  C-SHNAME   PIC X(48) VALUE
                  "                                      ".
             03  C-SGOKEI   PIC X(3) VALUE "   ".
           02  C-CLEAR2.
             03  C-SSIZE1   PIC X(36) VALUE
                "                          ".
             03  C-SSIZE2   PIC X(35) VALUE
                "                         ".
             03  C-SSIZE    PIC X(3) VALUE "   ".
           02  C-CLEAR3.
             03  C-SSURYOW1 PIC X(36)    VALUE
                "                          ".
             03  C-SSURYOW2 PIC X(35)   VALUE
                "                         ".
             03  C-SSURYOW  PIC X(2) VALUE "  ".
       01  ERRORSHORI.
           02  ERR-1        PIC X(5) VALUE X"1B4A05".
           02  ERR-2        PIC X(19) VALUE
                            "***  ﾀﾞﾌﾞﾙｴﾗｰ  ***".
           02  ERR-31       PIC X(22) VALUE
                            "***  PROGRAM ｴﾗｰ 1 ***".
           02  ERR-32       PIC X(22) VALUE
                            "***  PROGRAM ｴﾗｰ 2 ***".
           02  ERR-4        PIC X(19) VALUE
                            "***  ｻｲｽﾞ 無し  ***".
           02  ERR-5        PIC X(19) VALUE
                            "***  HI-M 無し  ***".
           02  ERR-6        PIC X(19) VALUE
                            "***  TC-M 無し  ***".
           02  ERR-7        PIC X(18) VALUE
                            "***  ｺｰﾄﾞ ｴﾗｰ ***".
           02  C-ERR        PIC X(30) VALUE
                            "                              ".
       PROCEDURE          DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *HYOJI1
       CALL "SD_Init" USING
           "HYOJI1" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "HYO" "X" "1" "0" "12" " " "HYOJI1" RETURNING RESU.
      *NYURYOKU
       CALL "SD_Init" USING 
            "NYURYOKU" " " "0" "0" "213" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-0" " " "2" "0" "6" " " "NYURYOKU" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "2" "6" "6" " " "NYURYOKU-0"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-1" " " "3" "0" "72" "NYURYOKU-0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SCODE" "9" "3" "6" "7" " " "NYURYOKU-1"  RETURNING RESU.
       CALL "SD_Using" USING 
            "SCODE" BY REFERENCE WCODE "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SNAME" "N" "3" "13" "48" "SCODE" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "SNAME" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SUNSO-1" "9" "3" "66" "1" "SNAME" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "SUNSO-1" BY REFERENCE W-UNSOW1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SUNSO-2" "N" "3" "67" "6" "SUNSO-1" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "SUNSO-2" BY REFERENCE W-UN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SUNSO-3" "9" "3" "66" "1" "SUNSO-2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "SUNSO-3" BY REFERENCE W-UNSOW2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SUNSO-5" "N" "3" "67" "6" "SUNSO-3" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "SUNSO-5" BY REFERENCE W-UN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SCSU" "9" "3" "78" "3" "SUNSO-5" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "SCSU" BY REFERENCE W-CSU1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-2" " " "TATE-1" "0" "54" "NYURYOKU-1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SHCODE" "9" "TATE-1" "10" "6" " " "NYURYOKU-2"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "SHCODE" BY REFERENCE WI-CO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SHNAME" "N" "TATE-1" "22" "48" "SHCODE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "SHNAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-3" " " "TATE-2" "0" "3" "NYURYOKU-2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SSIZE" "9" "TATE-2" "YOKO-1" "3" " " "NYURYOKU-3"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "SSIZE" BY REFERENCE WSIZE-1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-4" " " "TATE-3" "0" "2" "NYURYOKU-3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SSURYOW" "9" "TATE-3" "YOKO-2" "2" " " "NYURYOKU-4"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "SSURYOW" BY REFERENCE WSURYOW-1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-5" " " "22" "0" "72" "NYURYOKU-4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "STY" "N" "22" "8" "72" " " "NYURYOKU-5"  RETURNING RESU.
       CALL "SD_Using" USING 
            "STY" BY REFERENCE WN-TY "72" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "NYURYOKU-6" " " "23" "0" "3" "NYURYOKU-5" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SMSU" "9" "23" "76" "3" " " "NYURYOKU-6"  RETURNING RESU.
       CALL "SD_Using" USING 
            "SMSU" BY REFERENCE WN-MSU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "KAKUNIN" " " "23" "0" "1" "NYURYOKU-6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SKAKUNIN" "9" "23" "50" "1" " " "KAKUNIN" RETURNING RESU.
       CALL "SD_Using" USING 
            "SKAKUNIN" BY REFERENCE KAKUNIN-1 "1" "0" RETURNING RESU.
      *HYOJI
       CALL "SD_Init" USING 
            "HYOJI" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "HYOJI3" " " "3" "0" "3" " " "HYOJI"  RETURNING RESU.
       CALL "SD_Init" USING 
            "SCSU-1" "ZZ9" "3" "78" "3" " " "HYOJI3"  RETURNING RESU.
       CALL "SD_From" USING 
            "SCSU-1" BY REFERENCE W-CSU1 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "HYOJI4" " " "TATE-3" "0" "2" "HYOJI3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SSURYOW-1" "Z9" "TATE-3" "YOKO-2" "2" " " "HYOJI4"
            RETURNING RESU.
       CALL "SD_From" USING 
            "SSURYOW-1" BY REFERENCE WSURYOW-1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "HYOJI5" " " "TATE-1" "0" "3" "HYOJI4" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "SGOKEI" "ZZ9" "TATE-1" "76" "3" " " "HYOJI5" RETURNING RESU.
       CALL "SD_From" USING 
            "SGOKEI" BY REFERENCE WI-GOKEI "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "HYOJI6" " " "23" "0" "3" "HYOJI5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "SMSU-1" "ZZ9" "23" "76" "3" " " "HYOJI6"  RETURNING RESU.
       CALL "SD_From" USING 
            "SMSU-1" BY REFERENCE WN-MSU "3" "0" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "204" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLEAR1" " " "TATE-1" "0" "57" " " "C-CLEAR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SHCODE" "X" "TATE-1" "10" "6" " " "C-CLEAR1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SHNAME" "X" "TATE-1" "22" "48" "C-SHCODE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SGOKEI" "X" "TATE-1" "76" "3" "C-SHNAME" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLEAR2" " " "TATE-2" "0" "74" "C-CLEAR1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SSIZE1" "X" "TATE-2" "8" "36" " " "C-CLEAR2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SSIZE2" "X" "TATE-2" "44" "35" "C-SSIZE1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SSIZE" "X" "TATE-2" "YOKO-1" "3" "C-SSIZE2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CLEAR3" " " "TATE-3" "0" "73" "C-CLEAR2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SSURYOW1" "X" "TATE-3" "8" "36" " " "C-CLEAR3"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SSURYOW2" "X" "TATE-3" "44" "35" "C-SSURYOW1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SSURYOW" "X" "TATE-3" "YOKO-2" "2" "C-SSURYOW2" " "
            RETURNING RESU.
      *ERRORSHORI
       CALL "SD_Init" USING 
            "ERRORSHORI" " " "24" "0" "173" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-1" "X" "24" "80" "5" " " "ERRORSHORI" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-2" "X" "24" "31" "19" "ERR-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-31" "X" "24" "31" "22" "ERR-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-32" "X" "24" "31" "22" "ERR-31" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-4" "X" "24" "31" "19" "ERR-32" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-5" "X" "24" "31" "19" "ERR-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-6" "X" "24" "31" "19" "ERR-5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-7" "X" "24" "31" "18" "ERR-6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-ERR" "X" "24" "30" "30" "ERR-7" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-000.
           CALL "DB_F_Open" USING
            "INPUT" SIN-F_PNAME1 " " BY REFERENCE SIN-F_IDLST "0".
           MOVE ZERO TO NAMBER.
       M-005.
      *           READ SIN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SIN-F_PNAME1 BY REFERENCE SIN-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SIN-F_IDLST SIN-F_PNAME1
               GO TO M-010
           END-IF
           MOVE SIN-NO TO NAMBER.
           GO TO M-005.
       M-010.
           CALL "SD_Output" USING "HYOJI1" HYOJI1 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" SNF-F_PNAME1 " " BY REFERENCE SNF-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" SIN-F_PNAME1 " " BY REFERENCE SIN-F_IDLST "0".
           MOVE SPACE TO W-TY.
           ACCEPT W-NGP FROM DATE.
           MOVE ZERO TO W-CODE.
       M-020.
           CALL "SD_Screen_Output" USING "SCHS02" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
       M-025.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ENDS RETURNING RESU.
           IF  ENDS = PF9 OR C2
               GO TO M-900
           END-IF
           IF  ENDS NOT = HTB AND SKP
               GO TO M-025
           END-IF
           IF  W-NGP = ZERO
               GO TO M-030
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-025
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-025
           END-IF.
       M-030.
           PERFORM S-05 THRU S-15.
           MOVE W-CODE TO WCODE.
           MOVE W-TY TO WN-TY.
           MOVE W-NGP TO WN-DATE.
       M-031.
           IF  WCODE NOT = ZERO
               CALL "SD_Output" USING "SCODE" SCODE "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE SCODE "SCODE" "9" "7"
            BY REFERENCE ENDS RETURNING RESU.
           CALL "SD_Output" USING "C-ERR" C-ERR "p" RETURNING RESU.
           IF  ENDS = C2 AND PF9
               GO TO M-900
           END-IF
           IF  ENDS = BTB
               GO TO M-025
           END-IF
           IF  ENDS NOT = HTB AND SKP
               GO TO M-030
           END-IF
           IF  WCODE = W-CODE
               GO TO M-040
           END-IF
           MOVE WCODE TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-6" ERR-6 "p" RETURNING RESU
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO TO M-030
           END-IF
           MOVE TC-UCD TO W-UNSOW1.
           MOVE ZERO TO W-CSU1.
           GO TO M-050.
       M-040.
           MOVE W-UNSOW2 TO W-UNSOW1.
           MOVE W-CSU2 TO W-CSU1.
       M-050.
           PERFORM S-75 THRU S-80.
           CALL "SD_Output" USING "SNAME" SNAME "p" RETURNING RESU.
           CALL "SD_Output" USING "SUNSO-1" SUNSO-1 "p" RETURNING RESU.
           CALL "SD_Output" USING "SUNSO-2" SUNSO-2 "p" RETURNING RESU.
           CALL "SD_Output" USING "SCSU-1" SCSU-1 "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE SUNSO-1 "SUNSO-1" "9" "1"
            BY REFERENCE ENDS RETURNING RESU.
           IF  ENDS = BTB
               GO TO M-031
           END-IF
           PERFORM S-75 THRU S-80.
           CALL "SD_Output" USING "SUNSO-2" SUNSO-2 "p" RETURNING RESU.
       M-070.
           CALL "SD_Accept" USING BY REFERENCE SCSU "SCSU" "9" "3"
            BY REFERENCE ENDS RETURNING RESU.
           CALL "SD_Output" USING "SCSU-1" SCSU-1 "p" RETURNING RESU.
           IF  ENDS = BTB
               GO TO M-050
           END-IF
           IF  W-CSU1 = ZERO
               GO TO M-070
           END-IF
           MOVE 1 TO WN-MSU.
       M-080.
           MOVE ZERO TO KAZU-1.
           MOVE 1 TO TATE-1.
           CALL "SD_Arg_Match_Line" USING
            "TATE-1" "2" TATE-1 RETURNING RESU.
           MOVE 2 TO TATE-2.
           CALL "SD_Arg_Match_Line" USING
            "TATE-2" "2" TATE-2 RETURNING RESU.
           MOVE 3 TO TATE-3.
           CALL "SD_Arg_Match_Line" USING
            "TATE-3" "2" TATE-3 RETURNING RESU.
       M-090.
           ADD 1 TO KAZU-1.
           IF  KAZU-1 > 6
               GO TO M-300
           END-IF
           ADD 3 TO TATE-1 TATE-2 TATE-3.
           CALL "SD_Arg_Match_Line" USING
            "TATE-1" "2" TATE-1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "TATE-2" "2" TATE-2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "TATE-3" "2" TATE-3 RETURNING RESU.
       M-100.
           MOVE WI-D(KAZU-1) TO WI-R.
           CALL "SD_Accept" USING BY REFERENCE SHCODE "SHCODE" "9" "6"
            BY REFERENCE ENDS RETURNING RESU.
           CALL "SD_Output" USING "C-ERR" C-ERR "p" RETURNING RESU.
           IF  ENDS = BTB
               GO TO M-270
           END-IF
           IF  ENDS = C1 OR ADV
               GO TO M-280
           END-IF
           MOVE WI-CO TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-5" ERR-5 "p" RETURNING RESU
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO TO M-100
           END-IF
           CALL "SD_Output" USING "SHNAME" SHNAME "p" RETURNING RESU.
           MOVE 4 TO YOKO-1.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-1" "2" YOKO-1 RETURNING RESU.
           MOVE 5 TO YOKO-2.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-2" "2" YOKO-2 RETURNING RESU.
           MOVE ZERO TO CNT-2.
       M-110.
           ADD 1 TO CNT-2.
           IF  CNT-2 > 18
               GO TO M-270
           END-IF
           ADD 4 TO YOKO-1 YOKO-2.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-1" "2" YOKO-1 RETURNING RESU.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-2" "2" YOKO-2 RETURNING RESU.
       M-120.
           MOVE WI-SIZD(CNT-2) TO WSIZE-1.
           CALL "SD_Accept" USING BY REFERENCE SSIZE "SSIZE" "9" "3"
            BY REFERENCE ENDS RETURNING RESU.
           CALL "SD_Output" USING "C-ERR" C-ERR "p" RETURNING RESU.
           IF  ENDS = ADV
               GO TO M-250
           END-IF
           IF  ENDS = BTB
               GO TO M-240
           END-IF
           MOVE W-SCHK1 TO W-SCHKD.
           MOVE HI-SS1 TO W-SC.
           MOVE 1 TO JUMBAN.
           MOVE ZERO TO CNT-1.
       M-130.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 10
               GO TO M-140
           END-IF
           IF  W-SCHK9(CNT-1) NOT = WSIZE-1
               GO TO M-130
           END-IF
           GO TO M-200.
       M-140.
           MOVE W-SCHK2 TO W-SCHKD.
           MOVE HI-SS2 TO W-SC.
           MOVE 2 TO JUMBAN.
           MOVE ZERO TO CNT-1.
       M-150.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 10
               GO TO M-160
           END-IF
           IF  W-SCHK9(CNT-1) NOT = WSIZE-1
               GO TO M-150
           END-IF
           GO TO M-200.
       M-160.
           MOVE W-SCHK3 TO W-SCHKD.
           MOVE HI-SS3 TO W-SC.
           MOVE 3 TO JUMBAN.
           MOVE ZERO TO CNT-1.
       M-170.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 10
               GO TO M-180
           END-IF
           IF  W-SCHK9(CNT-1) NOT = WSIZE-1
               GO TO M-170
           END-IF
           GO TO M-200.
       M-180.
           MOVE W-SCHK4 TO W-SCHKD.
           MOVE HI-SS4 TO W-SC.
           MOVE 4 TO JUMBAN.
           MOVE ZERO TO CNT-1.
       M-190.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 10
               GO TO M-210
           END-IF
           IF  W-SCHK9(CNT-1) NOT = WSIZE-1
               GO TO M-190
           END-IF.
       M-200.
           IF  W-SCD(CNT-1) NOT = 0
               GO TO M-220
           END-IF
           IF  JUMBAN = 3
               GO TO M-180
           END-IF.
       M-210.
           CALL "SD_Output" USING "ERR-4" ERR-4 "p" RETURNING RESU.
           CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU.
           GO TO M-120.
       M-220.
           PERFORM S-50 THRU S-60.
           IF  W-DCHK NOT = ZERO
               GO TO M-120
           END-IF
           MOVE WSIZE-1 TO WI-SIZD(CNT-2).
       M-230.
           MOVE WI-SSD(CNT-2) TO WSURYOW-1.
           CALL "SD_Accept" USING BY REFERENCE SSURYOW "SSURYOW" "9" "2"
            BY REFERENCE ENDS RETURNING RESU.
           CALL "SD_Output" USING
            "SSURYOW-1" SSURYOW-1 "p" RETURNING RESU.
           IF  ENDS = BTB
               GO TO M-120
           END-IF
           IF  WSURYOW-1 = ZERO
               GO TO M-230
           END-IF
           MOVE WSURYOW-1 TO WI-SSD(CNT-2).
           GO TO M-110.
       M-240.
           SUBTRACT 1 FROM CNT-2.
           IF  CNT-2 = ZERO
               GO TO M-100
           END-IF
           SUBTRACT 4 FROM YOKO-1 YOKO-2.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-1" "2" YOKO-1 RETURNING RESU.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-2" "2" YOKO-2 RETURNING RESU.
           GO TO M-120.
       M-250.
           IF  CNT-2 = 1
               GO TO M-120
           END-IF
           PERFORM S-40 THRU S-45.
       M-260.
           PERFORM S-20 THRU S-35.
           GO TO M-090.
       M-270.
           PERFORM S-20 THRU S-35.
           SUBTRACT 1 FROM KAZU-1.
           IF KAZU-1 = ZERO
               GO TO M-070
           END-IF
           SUBTRACT 3 FROM TATE-1 TATE-2 TATE-3.
           CALL "SD_Arg_Match_Line" USING
            "TATE-1" "2" TATE-1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "TATE-2" "2" TATE-2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "TATE-3" "2" TATE-3 RETURNING RESU.
           GO TO M-100.
       M-280.
           MOVE KAZU-1 TO KAZU-2.
           COMPUTE TATE-1A = TATE-1 - 3.
           COMPUTE TATE-2A = TATE-2 - 3.
           COMPUTE TATE-3A = TATE-3 - 3.
       M-290.
           MOVE ZERO TO WI-R.
           MOVE SPACE TO WI-SIZ.
           MOVE WI-R TO WI-D(KAZU-2).
           CALL "SD_Output" USING
            "C-SHCODE" C-SHCODE "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SHNAME" C-SHNAME "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SGOKEI" C-SGOKEI "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SSIZE1" C-SSIZE1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SSIZE2" C-SSIZE2 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SSURYOW1" C-SSURYOW1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SSURYOW2" C-SSURYOW2 "p" RETURNING RESU.
           ADD 1 TO KAZU-2.
           IF  KAZU-2 = 7
               MOVE TATE-1A TO TATE-1
               CALL "SD_Arg_Match_Line" USING
                "TATE-1" "2" TATE-1 RETURNING RESU
               MOVE TATE-2A TO TATE-2
               CALL "SD_Arg_Match_Line" USING
                "TATE-2" "2" TATE-2 RETURNING RESU
               MOVE TATE-3A TO TATE-3
               CALL "SD_Arg_Match_Line" USING
                "TATE-3" "2" TATE-3 RETURNING RESU
               GO TO M-300
           END-IF
           ADD 3 TO TATE-1 TATE-2 TATE-3.
           CALL "SD_Arg_Match_Line" USING
            "TATE-1" "2" TATE-1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "TATE-2" "2" TATE-2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "TATE-3" "2" TATE-3 RETURNING RESU.
           GO TO M-290.
       M-300.
           CALL "SD_Output" USING "STY" STY "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE STY "STY" "N" "72"
            BY REFERENCE ENDS RETURNING RESU.
           IF  ENDS = HTB OR SKP
               GO TO M-305
           END-IF
           IF  ENDS NOT = BTB
               GO TO M-300
           END-IF
           SUBTRACT 1 FROM KAZU-1.
           IF  KAZU-1 = ZERO
               GO TO M-070
           END-IF
           GO TO M-100.
       M-305.
           CALL "SD_Output" USING "SMSU-1" SMSU-1 "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE SMSU "SMSU" "9" "3"
            BY REFERENCE ENDS RETURNING RESU.
           IF  ENDS = BTB
               GO TO M-300
           END-IF
           IF  WN-MSU = ZERO
               GO TO M-305
           END-IF
           CALL "SD_Output" USING "SMSU-1" SMSU-1 "p" RETURNING RESU.
       M-310.
           CALL "SD_Accept" USING BY REFERENCE SKAKUNIN "SKAKUNIN"
            "9" "1" BY REFERENCE ENDS RETURNING RESU.
           IF  ENDS = BTB
               GO TO M-305
           END-IF
           IF  ENDS NOT = HTB AND SKP
               GO TO M-310
           END-IF
           IF  KAKUNIN-1 = 9
               GO TO M-020
           END-IF
           IF  KAKUNIN-1 NOT = 1
               GO TO M-310
           END-IF
           MOVE WN-TY TO W-TY.
           ADD 1 TO NAMBER.
           MOVE NAMBER TO WN-NO.
           MOVE ZERO TO SNF-R.
           MOVE WCODE TO WN-CO.
           MOVE W-CSU1 TO WN-CSU.
           MOVE W-UNSOW1 TO WN-UCO.
           MOVE WN-R TO SNF-R.
      *           WRITE SNF-R.
      *//////////////
           CALL "DB_Insert" USING
            SNF-F_PNAME1 SNF-F_LNAME SNF-R RETURNING RET.
           MOVE WCODE TO W-CODE.
           MOVE W-CSU1 TO W-CSU2.
           MOVE W-UNSOW1 TO W-UNSOW2.
           MOVE ZERO TO KAZU-2.
       M-320.
           ADD 1 TO KAZU-2.
           IF  KAZU-2 = 7
               GO TO M-030
           END-IF
           MOVE WI-D(KAZU-2) TO WI-R.
           IF  WI-CO = ZERO
               GO TO M-030
           END-IF
           MOVE NAMBER TO WI-NO.
           MOVE ZERO TO SIN-R.
           MOVE WI-NO TO SIN-NO.
           MOVE WI-CO TO SIN-CO.
           MOVE WI-GOKEI TO SIN-GOKEI.
           MOVE ZERO TO CNT-2.
       M-330.
           ADD 1 TO CNT-2.
           IF  CNT-2 = 19
      *               WRITE SIN-R
      *//////////////
               CALL "DB_Insert" USING
                SIN-F_PNAME1 SIN-F_LNAME SIN-R RETURNING RET
               GO TO M-320
           END-IF
           MOVE WI-SIZD(CNT-2) TO WSIZE-1.
           IF  WSIZE-1 = SPACE
               GO TO M-330
           END-IF
           MOVE W-SCNT1 TO W-SCNTD.
           MOVE ZERO TO CNT-1.
       M-340.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 27
               GO TO M-350
           END-IF
           IF  W-SCNT9(CNT-1) NOT = WSIZE-1
               GO TO M-340
           END-IF
           GO TO M-370.
       M-350.
           MOVE W-SCNT2 TO W-SCNTD.
           MOVE ZERO TO CNT-1.
       M-360.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 7
               CALL "SD_Output" USING "ERR-31" ERR-31 "p" RETURNING RESU
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO TO M-900
           END-IF
           IF  W-SCNT9(CNT-1) NOT = WSIZE-1
               GO TO M-360
           END-IF.
       M-370.
           IF  SIN-SU(CNT-1) NOT = ZERO AND SPACE
               CALL "SD_Output" USING "ERR-32" ERR-32 "p" RETURNING RESU
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO TO M-900
           END-IF
           MOVE WI-SSD(CNT-2) TO SIN-SU(CNT-1).
           GO TO M-330.
       M-900.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNF-F_IDLST SNF-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SIN-F_IDLST SIN-F_PNAME1.
           CALL "SD_Output" USING "HYOJI1" HYOJI1 "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "SD_Screen_Output" USING "SCHS02" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           MOVE ZERO TO WN-R WI-R.
           MOVE SPACE TO WN-TY WI-SIZ.
           MOVE ALL "　" TO WN-TY.
           MOVE ZERO TO CNT-1.
       S-10.
           ADD 1 TO CNT-1.
           IF  CNT-1 = 7
               GO TO S-15
           END-IF
           MOVE SPACE TO WI-D(CNT-1).
           GO TO S-10.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO WI-GOKEI CNT-1.
       S-25.
           ADD 1 TO CNT-1.
           IF  CNT-1 = 19
               GO TO S-30
           END-IF
           ADD WI-SSD(CNT-1) TO WI-GOKEI.
           GO TO S-25.
       S-30.
           CALL "SD_Output" USING "SGOKEI" SGOKEI "p" RETURNING RESU.
           MOVE WI-R TO WI-D(KAZU-1).
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO WSIZE-1.
           MOVE ZERO TO WSURYOW-1.
           CALL "SD_Output" USING "C-SSIZE" C-SSIZE "p" RETURNING RESU.
           CALL "SD_Output" USING
            "C-SSURYOW" C-SSURYOW "p" RETURNING RESU.
           MOVE WSIZE-1 TO WI-SIZD(CNT-2).
           MOVE WSURYOW-1 TO WI-SSD(CNT-2).
           ADD 1 TO CNT-2.
           IF  CNT-2 = 19
               GO TO S-45
           END-IF
           ADD 4 TO YOKO-1 YOKO-2.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-1" "2" YOKO-1 RETURNING RESU.
           CALL "SD_Arg_Match_Col" USING
            "YOKO-2" "2" YOKO-2 RETURNING RESU.
           GO TO S-40.
       S-45.
           EXIT.
       S-50.
           MOVE ZERO TO W-DCHK CNT-1.
       S-55.
           ADD 1 TO CNT-1.
           IF  CNT-1 > 18
               GO TO S-60
           END-IF
           IF  WI-SIZD(CNT-1) = SPACE
               GO TO S-60
           END-IF
           IF  WI-SIZD(CNT-1) NOT = WSIZE-1
               GO TO S-55
           END-IF
           IF  CNT-1 = CNT-2
               GO TO S-55
           END-IF
           CALL "SD_Output" USING "ERR-2" ERR-2 "p" RETURNING RESU.
           CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU.
           MOVE 5 TO W-DCHK.
       S-60.
           EXIT.
       S-65.
           CALL "SD_Output" USING "C-ERR" C-ERR "p" RETURNING RESU.
           IF  ENDS = C2
               GO TO M-900
           END-IF
           IF  ENDS NOT = HTB AND SKP
               GO TO M-030
           END-IF
           MOVE W-CODE TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-6" ERR-6 "p" RETURNING RESU
               CALL "SD_Output" USING "ERR-1" ERR-1 "p" RETURNING RESU
               GO TO M-030
           END-IF
           CALL "SD_Output" USING "SNAME" SNAME "p" RETURNING RESU.
       S-70.
           EXIT.
       S-75.
           MOVE 2 TO JCON1-01.
           MOVE W-UNSOW1 TO JCON1-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON2-03
           END-IF
           MOVE JCON2-03 TO W-UND.
       S-80.
           EXIT.
