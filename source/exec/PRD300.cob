       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRD300.
      **********************************************
      *    êUë÷ì`ï[ç‡ñ±ä∑(SIWAKE-IWÅ®SIWAKE-I)     *
      **********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-SNGP.
             03  W-SNEN       PIC  9(004).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-SU           PIC  9(003).
           02  W-DNO          PIC  9(006).
      *
           COPY LSTAT.
           COPY SIWAID.
           COPY SIWAIW.
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "êUë÷ì`ï[ÅiéËå`ÅEóÃé˚èëóvÅEîÑä|ÅEîÉä|ÅjÅ@ç‡ñ±ïœä∑".
           02  FILLER  PIC  X(022) VALUE
                "ämîF  OK=1 NO=9   ÿ¿∞›".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NGP   .
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  N(001) VALUE "îN".
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  N(001) VALUE "åé".
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  N(001) VALUE "ì˙".
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  N(001) VALUE "îN".
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  N(001) VALUE "åé".
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  N(001) VALUE "ì˙".
           02  D-SU.
             03  FILLER  PIC  Z(003).
             03  FILLER  PIC  N(008) VALUE "ñáÅ@ïœä∑ÇµÇ‹ÇµÇΩ".
             03  FILLER  PIC  X(006) VALUE "OK=ESC".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(028) VALUE
                  "***  SIWAKE-I WRITE ¥◊∞  ***".
             03  E-ME2     PIC  X(031) VALUE
                  "***  SIWAKE-IW REWRITE ¥◊∞  ***".
             03  E-ME3     PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME9     PIC  X(026) VALUE
                  "***  ñ¢çÏï\ÉfÅ[É^óLÇË  ***".
           COPY LSSEM.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "70" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "RN" "1" "15" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "X" "20" "40" "22" "01C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "55" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NGP" " " "12" "0" "30" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NGP" "9" "12" "23" "4" " " "D-NGP" RETURNING RESU.
       CALL "SD_From" USING
            "01D-NGP" BY REFERENCE W-SNEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NGP" "N" "12" "27" "2" "01D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NGP" "Z" "12" "29" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "03D-NGP" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NGP" "N" "12" "31" "2" "03D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05D-NGP" "Z" "12" "33" "2" "04D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "05D-NGP" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06D-NGP" "N" "12" "35" "2" "05D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07D-NGP" "N" "12" "38" "2" "06D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08D-NGP" "9" "12" "41" "4" "07D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "08D-NGP" BY REFERENCE W-ENEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "09D-NGP" "N" "12" "45" "2" "08D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10D-NGP" "Z" "12" "47" "2" "09D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "10D-NGP" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "11D-NGP" "N" "12" "49" "2" "10D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "12D-NGP" "Z" "12" "51" "2" "11D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING
            "12D-NGP" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "13D-NGP" "N" "12" "53" "2" "12D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SU" " " "15" "0" "25" "D-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-SU" "Z" "15" "29" "3" " " "D-SU" RETURNING RESU.
       CALL "SD_From" USING
            "01D-SU" BY REFERENCE W-SU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-SU" "N" "15" "32" "16" "01D-SU" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03D-SU" "X" "15" "54" "6" "02D-SU" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "102" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "28" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "31" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME9" "X" "24" "15" "26" "E-ME3" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p"
                                         RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" SDW_PNAME1 " " BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
       M-10.
      *           READ SDW NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF.
           IF  SDWZHC NOT = 0
               GO TO M-10
           END-IF.
           IF  ZERO = W-SNGP
               MOVE SDWYMD TO W-SNGP W-ENGP
           END-IF.
           IF  SDWYMD < W-SNGP
               MOVE SDWYMD TO W-SNGP
           END-IF.
           IF  SDWYMD > W-ENGP
               MOVE SDWYMD TO W-ENGP
           END-IF.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           IF  ZERO = W-SNGP
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-95
           END-IF.
           CALL "SD_Output" USING "D-NGP" D-NGP "p"
                                         RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING
                 BY REFERENCE A-DMM "A-DMM" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-DMM = 9
               GO TO M-95
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF.
      *
           CALL "DB_F_Open" USING
            "INPUT" SDW_PNAME1 " " BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDI_PNAME1 "SHARED" BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
       M-25.
      *           READ SDW NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  SDWZHC NOT = 0
               GO TO M-25
           END-IF.
           IF  SDWSIN NOT = 1
               CALL "SD_Output" USING "E-ME9" E-ME9 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU
               GO TO M-25
           END-IF.
       M-30.
           IF  SDWJNO NOT = W-DNO
               ADD 1 TO W-SU
               MOVE SDWJNO TO W-DNO
           END-IF.
       M-35.
           INITIALIZE SDI-REC.
           MOVE SDWYMD TO SDIYMD.
           MOVE SDWJNO TO SDIJNO.
           MOVE SDWLNO TO SDILNO.
           MOVE SDWKARI TO SDIKARI.
           MOVE SDWKASI TO SDIKASI.
           MOVE SDWCUST TO SDICUST.
           MOVE SDWTEKICD TO SDITEKICD.
           MOVE SDWTEKI TO SDITEKI.
           MOVE SDWNAMEN TO SDINAMEN.
           MOVE SDWETAX TO SDIETAX.
           MOVE 1 TO SDISIN.
      *           WRITE SDI-REC INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            SDI_PNAME1 SDI_LNAME SDI-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME1" E-ME1 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-40
           END-IF.
      *
           MOVE 1 TO SDWZHC.
      *           REWRITE SDW-REC INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SDW_PNAME1 SDW_LNAME SDW-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME3" E-ME3 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-90
           END-IF.
           GO TO M-25.
       M-40.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING "E-ME78" E-ME78 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU
               GO TO M-90
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           MOVE "SIWAKE-I     " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p"
                                         RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" SDI_PNAME1 "SHARED" BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
           GO TO M-35.
       M-90.
           CALL "SD_Output" USING "D-SU" D-SU "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p"
                                         RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
