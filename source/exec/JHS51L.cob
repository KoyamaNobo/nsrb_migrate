       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS51L.
      *********************************************************
      *    PROGRAM         :  統一伝票発行（ワークマン）      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-20K              PIC  X(005) VALUE X"1A24212474".
       01  W-P01.
           02  P-20K          PIC  X(005).
           02  F              PIC  X(017).
           02  P-UBCD         PIC  X(015).
           02  F              PIC  X(060).
           02  P-HCCD         PIC  X(010).
           02  F              PIC  X(007).
       01  W-P02.
           02  F              PIC  X(007).
           02  P-SNA          PIC  X(020).
           02  F              PIC  X(045).
           02  P-NR           PIC  N(005).
           02  F              PIC  X(027).
       01  W-P03.
           02  F              PIC  X(007).
           02  P-TNA          PIC  X(020).
           02  F              PIC  X(004).
           02  P-SCD          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(003).
           02  P-BCD          PIC  9(002).
           02  P-V            PIC  X(001).
           02  P-SHC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DPC          PIC  X(002).
           02  F              PIC  X(005).
           02  P-DNOD         PIC  9(007).
           02  F              PIC  X(001).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(026).
           02  P-HNEN         PIC  9(002).
           02  P-HGET         PIC Z9.
           02  P-HPEY         PIC Z9.
           02  P-NNEN         PIC  9(002).
           02  P-NGET         PIC Z9.
           02  P-NPEY         PIC Z9.
           02  F              PIC  X(001).
           02  P-HSP          PIC  X(001).
           02  F              PIC  X(001).
       01  W-P04.
           02  F              PIC  X(005).
           02  P-GCN          PIC  9(006).
           02  F              PIC  X(005).
           02  P-NON          PIC  X(003).
           02  P-TSH          PIC  9(005).
           02  F              PIC  X(001).
           02  P-TKCD         PIC  X(005).
           02  F              PIC  X(079).
       01  W-P05.
           02  F              PIC  X(005).
           02  P-SHN          PIC  X(025).
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(004).
           02  P-ISU          PIC  Z(003).
           02  F              PIC  X(001).
           02  P-KSU          PIC  Z(004).
           02  F              PIC  X(001).
           02  P-HTC          PIC  X(002).
           02  P-SU           PIC  Z(005).
           02  F              PIC  X(014).
           02  P-GTN          PIC  Z(006).
           02  F              PIC  X(002).
           02  P-GKIN         PIC  Z(009).
           02  P-UTN          PIC  Z(006).
           02  P-UKIN         PIC  Z(009).
       01  W-DATA.
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-D.
             03  W-POC        PIC  9(001).
             03  W-FOC        PIC  9(001).
             03  W-TPC        PIC  9(001).
             03  W-HKC        PIC  9(001).
             03  W-SED.
               04  W-SSTC.
                 05  W-SSCD   PIC  9(002).
                 05  W-STCD   PIC  9(004).
               04  W-ESTC.
                 05  W-ESCD   PIC  9(002).
                 05  W-ETCD   PIC  9(004).
               04  W-SDNO     PIC  9(007).
               04  W-EDNO     PIC  9(007).
             03  W-DMM        PIC  9(001).
             03  W-RSTC.
               04  W-RSCD     PIC  9(002).
               04  W-RTCD     PIC  9(004).
             03  W-STC        PIC  9(009).
             03  W-DNO        PIC  9(009).
             03  W-HCC        PIC  9(001).
             03  W-LC         PIC  9(002).
           COPY LSTAT.
      *
           COPY LITDNW.
      *FD  TDNWRF
       01  TDNWRF_JHS51L.
           02  TDNWRF_PNAME1   PIC  X(006) VALUE "TDNWRF".
           02  F               PIC  X(001).
           02  TDNWRF_LNAME    PIC  X(013) VALUE "TDNWRF_JHS51L".
           02  F               PIC  X(001).
           02  TDNWRF_KEY1     PIC  X(100) VALUE SPACE.
           02  TDNWRF_SORT     PIC  X(100) VALUE SPACE.
           02  TDNWRF_IDLST    PIC  X(100) VALUE SPACE.
           02  TDNWRF_RES      USAGE  POINTER.
       01  TDNWR-R.
           02  TDNWR-R1.
               03  TDNWR1-KEY.
                 04  TDNWR1-STC.
                   05  F           PIC  9(002).
                   05  TDNWR1-SCD  PIC  9(002).
                   05  F           PIC  9(001).
                   05  TDNWR1-TCD  PIC  9(004).
                 04  TDNWR1-DNO.
                   05  F           PIC  9(002).
                   05  TDNWR1-DNOD PIC  9(007).
                 04  TDNWR1-DGN    PIC  9(002).
               03  TDNWR1-BC.
                 04  TDNWR1-BCD    PIC  9(002).
                 04  F             PIC  9(001).
               03  TDNWR1-SHC      PIC  9(001).
               03  TDNWR1-DPC      PIC  X(002).
               03  TDNWR1-HNGP     PIC  9(006).
               03  TDNWR1-HNGPD REDEFINES TDNWR1-HNGP.
                 04  TDNWR1-HNEN   PIC  9(002).
                 04  TDNWR1-HGET   PIC  9(002).
                 04  TDNWR1-HPEY   PIC  9(002).
               03  TDNWR1-NNGP     PIC  9(006).
               03  TDNWR1-NNGPD REDEFINES TDNWR1-NNGP.
                 04  TDNWR1-NNEN   PIC  9(002).
                 04  TDNWR1-NGET   PIC  9(002).
                 04  TDNWR1-NPEY   PIC  9(002).
               03  TDNWR1-THC      PIC  9(006).
               03  TDNWR1-MHC      PIC  X(001).
               03  F               PIC  X(001).
               03  TDNWR1-SNA      PIC  X(020).
               03  TDNWR1-TNA      PIC  X(020).
               03  TDNWR1-HCC      PIC  9(001).
               03  TDNWR1-HSP      PIC  X(001).
               03  TDNWR1-DHC      PIC  X(001).
               03  TDNWR1-KHC      PIC  X(001).
               03  TDNWR1-KCC      PIC  X(001).
               03  TDNWR1-UBC      PIC  X(001).
               03  TDNWR1-NCC      PIC  9(001).
               03  TDNWR1-EDI      PIC  9(001).
               03  TDNWR1-NKC      PIC  9(001).
               03  TDNWR1-ZAC      PIC  9(001).
               03  F               PIC  X(150).
               03  TDNWR1-HC       PIC  9(001).
               03  F               PIC  X(008).
               03  TDNWR1-PC       PIC  9(001).
           02  TDNWR-R2     REDEFINES TDNWR-R1.
               03  TDNWR2-KEY.
                 04  TDNWR2-STC.
                   05  F           PIC  9(002).
                   05  TDNWR2-SCD  PIC  9(002).
                   05  F           PIC  9(001).
                   05  TDNWR2-TCD  PIC  9(004).
                 04  TDNWR2-DNO.
                   05  F           PIC  9(002).
                   05  TDNWR2-DNOD PIC  9(007).
                 04  TDNWR2-DGN    PIC  9(002).
               03  TDNWR2-HCD      PIC  X(013).
               03  TDNWR2-ISU      PIC  9(003)V9(01).
               03  TDNWR2-KSU      PIC  9(004).
               03  TDNWR2-HTC      PIC  X(002).
               03  TDNWR2-SU       PIC  9(005)V9(01).
               03  TDNWR2-GTN      PIC  9(007)V9(02).
               03  TDNWR2-UTN      PIC  9(007).
               03  TDNWR2-GKIN     PIC  9(010).
               03  TDNWR2-UKIN     PIC  9(010).
               03  TDNWR2-GCN      PIC  9(006).
               03  TDNWR2-CCD      PIC  X(003).
               03  TDNWR2-SHN      PIC  X(025).
               03  TDNWR2-JAN      PIC  X(013).
               03  F               PIC  X(004).
               03  TDNWR2-TSH      PIC  9(005).
               03  TDNWR2-TKC      PIC  X(001).
               03  F               PIC  X(001).
               03  F               PIC  X(103).
               03  TDNWR2-HC       PIC  9(001).
               03  F               PIC  X(008).
               03  TDNWR2-PC       PIC  9(001).
       77  F                   PIC  X(001).
      *FD  SP-F
       77  SP-R                PIC  X(170).
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
           02  C-CL    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(014) VALUE
                "チェーンストア統一伝票　発行".
           02  FILLER  PIC  N(013) VALUE
                "（ターンアラウンド用２型）".
           02  FILLER  PIC  N(007) VALUE
                "［ワークマン］".
           02  FILLER  PIC  X(035) VALUE
                "テストプリント印字 (YES=1,NO=2) [ ]".
           02  FILLER  PIC  X(010) VALUE
                "１．発　行".
           02  FILLER  PIC  X(018) VALUE
                "２．再発行（当日）".
           02  FILLER  PIC  X(026) VALUE
                "３．再発行（以前）     [ ]".
           02  FILLER  PIC  X(028) VALUE
                "確認 (OK=1,NO=9) --->   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-SHM.
             03  FILLER  PIC  X(018) VALUE
                  "社店ｺｰﾄﾞ  伝票番号".
             03  FILLER  PIC  N(004) VALUE
                  "ＦＲＯＭ".
             03  FILLER  PIC  N(002) VALUE
                  "ＴＯ".
           02  D-SHMC.
             03  FILLER  PIC  X(018) VALUE
                  "                  ".
             03  FILLER  PIC  X(028) VALUE
                  "                            ".
             03  FILLER  PIC  X(028) VALUE
                  "                            ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-HKC   PIC  9(001).
           02  A-DMM   PIC  9(001).
           02  FILLER.
             03  A-SSTC  PIC  9(006).
             03  A-SDNO  PIC  9(007).
           02  FILLER.
             03  A-ESTC  PIC  9(006).
             03  A-EDNO  PIC  9(007).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  TDNWF DATA ｴﾗｰ  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNWF REWRITE ｴﾗｰ  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "185" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "20" "28" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "2" "20" "26" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "4" "10" "14" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "7" "22" "35" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "10" "22" "10" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "11" "22" "18" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "12" "22" "26" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "23" "40" "28" "07C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "104" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHM" " " "0" "0" "30" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHM" "X" "14" "33" "18" " " "D-SHM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHM" "N" "15" "22" "8" "01D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHM" "N" "16" "22" "4" "02D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHMC" " " "0" "0" "74" "D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHMC" "X" "14" "33" "18" " " "D-SHMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHMC" "X" "15" "22" "28" "01D-SHMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHMC" "X" "16" "22" "28" "02D-SHMC" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "7" "55" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HKC" "9" "12" "46" "1" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HKC" BY REFERENCE W-HKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "62" "1" "A-HKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "15" "0" "13" "A-DMM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSTC" "9" "15" "34" "6" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSTC" BY REFERENCE W-SSTC "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDNO" "9" "15" "44" "7" "A-SSTC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDNO" BY REFERENCE W-SDNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "16" "0" "13" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESTC" "9" "16" "34" "6" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESTC" BY REFERENCE W-ESTC "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EDNO" "9" "16" "44" "7" "A-ESTC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EDNO" BY REFERENCE W-EDNO "7" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "89" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "89" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "27" "E-ME10" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           MOVE ZERO TO W-D.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TPC = 2
               GO TO M-15
           END-IF
           IF  W-TPC NOT = 1
               GO TO M-10
           END-IF
           PERFORM TST-RTN THRU TST-EX.
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-HKC "A-HKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-HKC = 1
               CALL "SD_Output" USING "D-SHMC" D-SHMC "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  W-HKC NOT = 2 AND 3
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SSTC "A-SSTC" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ESTC "A-ESTC" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-SSTC > W-ESTC
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-HKC = 1
                   GO TO M-15
               ELSE
                   GO TO M-35
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-40
           END-IF
      *
           IF  W-HKC = 1
               CALL "DB_F_Open" USING
                "I-O" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST "1"
                "TDNW1-KEY" BY REFERENCE TDNW1-KEY
           ELSE
               IF  W-HKC = 2
                   CALL "DB_F_Open" USING
                    "INPUT" TDNWF_PNAME1 "SHARED" BY REFERENCE
                    TDNWF_IDLST "1" "TDNW1-KEY" BY REFERENCE TDNW1-KEY
               ELSE
                   CALL "DB_F_Open" USING
                    "INPUT" TDNWRF_PNAME1 " " BY REFERENCE
                    TDNWRF_IDLST "0"
               END-IF
           END-IF
           MOVE 1 TO W-FOC.
           IF  W-HKC = 3
               GO TO M-46
           END-IF.
       M-45.
      *           READ TDNWF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TDNW1-PC = 0
               GO TO M-45
           END-IF
           IF  W-HKC = 1
               IF  TDNW1-PC = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNW1-PC NOT = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               MOVE TDNW1-SCD TO W-RSCD
               MOVE TDNW1-TCD TO W-RTCD
               IF  W-RSTC < W-SSTC OR > W-ESTC
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNW1-DNOD < W-SDNO OR > W-EDNO
                   GO TO M-45
               END-IF
           END-IF
           IF  TDNW1-DGN NOT = 0
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               GO TO M-45
           END-IF
           GO TO M-50.
       M-46.
      *           READ TDNWRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNWRF_PNAME1 BY REFERENCE TDNWR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE TDNWR1-SCD TO W-RSCD.
           MOVE TDNWR1-TCD TO W-RTCD.
           IF  W-RSTC < W-SSTC OR > W-ESTC
               GO TO M-46
           END-IF
           IF  TDNWR1-DNOD < W-SDNO OR > W-EDNO
               GO TO M-46
           END-IF
           IF  TDNWR1-DGN NOT = 0
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               GO TO M-46
           END-IF.
       M-50.
           IF  W-HKC = 1 OR 2
               MOVE TDNW1-STC TO W-STC
               MOVE TDNW1-DNO TO W-DNO
               MOVE TDNW1-HCC TO W-HCC
           ELSE
               MOVE TDNWR1-STC TO W-STC
               MOVE TDNWR1-DNO TO W-DNO
               MOVE TDNWR1-HCC TO W-HCC
           END-IF
           MOVE ZERO TO WT-D.
           MOVE SPACE TO W-P01 W-P02 W-P03.
      *
           MOVE W-20K TO P-20K.
           IF  W-HKC = 1 OR 2
               PERFORM HED1-RTN THRU HED1-EX
           ELSE
               PERFORM HED2-RTN THRU HED2-EX
           END-IF
      *
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P01 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 20 TO W-LC.
           IF  W-HKC = 3
               GO TO M-56
           END-IF.
       M-55.
      *           READ TDNWF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TDNW1-PC = 0
               GO TO M-55
           END-IF
           IF  W-HKC = 1
               IF  TDNW1-PC = 9
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNW1-PC NOT = 9
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC = 2
               MOVE TDNW1-SCD TO W-RSCD
               MOVE TDNW1-TCD TO W-RTCD
               IF  W-RSTC < W-SSTC OR > W-ESTC
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNW1-DNOD < W-SDNO OR > W-EDNO
                   GO TO M-55
               END-IF
           END-IF
           IF (TDNW1-STC NOT = W-STC) OR (TDNW1-DNO NOT = W-DNO)
               GO TO M-60
           END-IF
           IF  TDNW1-DGN = ZERO
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               GO TO M-50
           END-IF
           GO TO M-57.
       M-56.
      *           READ TDNWRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNWRF_PNAME1 BY REFERENCE TDNWR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           MOVE TDNWR1-SCD TO W-RSCD.
           MOVE TDNWR1-TCD TO W-RTCD.
           IF  W-RSTC < W-SSTC OR > W-ESTC
               GO TO M-56
           END-IF
           IF  TDNWR1-DNOD < W-SDNO OR > W-EDNO
               GO TO M-56
           END-IF
           IF (TDNWR1-STC NOT = W-STC) OR (TDNWR1-DNO NOT = W-DNO)
               GO TO M-60
           END-IF
           IF  TDNWR1-DGN = ZERO
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               GO TO M-50
           END-IF.
       M-57.
           IF  W-HKC = 1 OR 2
               PERFORM MEI1-RTN THRU MEI1-EX
               GO TO M-55
           ELSE
               PERFORM MEI2-RTN THRU MEI2-EX
               GO TO M-56
           END-IF.
       M-60.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-HKC = 1
               PERFORM REW-RTN THRU REW-EX
           END-IF
           GO TO M-50.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-HKC = 1
               PERFORM REW-RTN THRU REW-EX
           END-IF.
       M-90.
           IF  W-FOC NOT = 0
               IF  W-HKC = 1 OR 2
                   CALL "DB_F_Close" USING
                    BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
               ELSE
                   CALL "DB_F_Close" USING
                    BY REFERENCE TDNWRF_IDLST TDNWRF_PNAME1
               END-IF
           END-IF
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       HED1-RTN.
           IF  TDNW1-UBC = "1"
               MOVE "ﾃｲﾊﾞﾝｺﾞﾝﾄﾞﾗ" TO P-UBCD
           END-IF
           IF  TDNW1-UBC = "2"
               MOVE "ｷﾝｲﾂ" TO P-UBCD
           END-IF
           IF  TDNW1-UBC = "3"
               MOVE "ｱｸｼｮﾝｴﾝﾄﾞ" TO P-UBCD
           END-IF
           IF  TDNW1-HCC = 1
               MOVE "*ｶﾞｲﾁｭｳ*" TO P-HCCD
           END-IF
           IF  TDNW1-HCC = 2
               MOVE "*ﾅｲﾁｭｳ*" TO P-HCCD
           END-IF
      *
           MOVE TDNW1-SNA TO P-SNA.
           MOVE "日進ゴム㈱" TO P-NR.
      *
           MOVE TDNW1-TNA TO P-TNA.
           MOVE TDNW1-SCD TO P-SCD.
           MOVE TDNW1-TCD TO P-TCD.
           MOVE TDNW1-BCD TO P-BCD.
           MOVE "-" TO P-V.
           MOVE TDNW1-SHC TO P-SHC.
           MOVE TDNW1-DPC TO P-DPC.
           MOVE TDNW1-DNOD TO P-DNOD.
           MOVE TDNW1-THC TO P-THC.
           MOVE TDNW1-HNEN TO P-HNEN.
           MOVE TDNW1-HGET TO P-HGET.
           MOVE TDNW1-HPEY TO P-HPEY.
           MOVE TDNW1-NNEN TO P-NNEN.
           MOVE TDNW1-NGET TO P-NGET.
           MOVE TDNW1-NPEY TO P-NPEY.
           MOVE TDNW1-HSP TO P-HSP.
       HED1-EX.
           EXIT.
       HED2-RTN.
           IF  TDNWR1-UBC = "1"
               MOVE "ﾃｲﾊﾞﾝｺﾞﾝﾄﾞﾗ" TO P-UBCD
           END-IF
           IF  TDNWR1-UBC = "2"
               MOVE "ｷﾝｲﾂ" TO P-UBCD
           END-IF
           IF  TDNWR1-UBC = "3"
               MOVE "ｱｸｼｮﾝｴﾝﾄﾞ" TO P-UBCD
           END-IF
           IF  TDNWR1-HCC = 1
               MOVE "*ｶﾞｲﾁｭｳ*" TO P-HCCD
           END-IF
           IF  TDNWR1-HCC = 2
               MOVE "*ﾅｲﾁｭｳ*" TO P-HCCD
           END-IF
      *
           MOVE TDNWR1-SNA TO P-SNA.
           MOVE "日進ゴム㈱" TO P-NR.
      *
           MOVE TDNWR1-TNA TO P-TNA.
           MOVE TDNWR1-SCD TO P-SCD.
           MOVE TDNWR1-TCD TO P-TCD.
           MOVE TDNWR1-BCD TO P-BCD.
           MOVE "-" TO P-V.
           MOVE TDNWR1-SHC TO P-SHC.
           MOVE TDNWR1-DPC TO P-DPC.
           MOVE TDNWR1-DNOD TO P-DNOD.
           MOVE TDNWR1-THC TO P-THC.
           MOVE TDNWR1-HNEN TO P-HNEN.
           MOVE TDNWR1-HGET TO P-HGET.
           MOVE TDNWR1-HPEY TO P-HPEY.
           MOVE TDNWR1-NNEN TO P-NNEN.
           MOVE TDNWR1-NGET TO P-NGET.
           MOVE TDNWR1-NPEY TO P-NPEY.
           MOVE TDNWR1-HSP TO P-HSP.
       HED2-EX.
           EXIT.
       MEI1-RTN.
           MOVE SPACE TO W-P04 W-P05.
      *
           IF  W-HCC = 1
               IF  TDNW2-GCN NOT = ZERO
                   MOVE TDNW2-GCN TO P-GCN
               END-IF
           END-IF
           IF  TDNW2-TSH NOT = ZERO
               MOVE "NO." TO P-NON
               MOVE TDNW2-TSH TO P-TSH
           END-IF
           IF  TDNW2-TKC = ZERO
               MOVE "ﾂｲｶﾌｶ" TO P-TKCD
           END-IF
      *
           MOVE TDNW2-SHN TO P-SHN.
           IF  TDNW2-JAN NOT = SPACE AND ZERO
               MOVE TDNW2-JAN TO P-JAN
           ELSE
               MOVE TDNW2-HCD TO P-JAN
           END-IF
           MOVE TDNW2-ISU TO P-ISU.
           MOVE TDNW2-KSU TO P-KSU.
           MOVE TDNW2-HTC TO P-HTC.
           MOVE TDNW2-SU TO P-SU.
           MOVE TDNW2-GTN TO P-GTN.
           MOVE TDNW2-GKIN TO P-GKIN.
           MOVE TDNW2-UTN TO P-UTN.
           MOVE TDNW2-UKIN TO P-UKIN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDNW2-SU TO WT-SU.
           ADD TDNW2-GKIN TO WT-GKIN.
           ADD TDNW2-UKIN TO WT-UKIN.
       MEI1-EX.
           EXIT.
       MEI2-RTN.
           MOVE SPACE TO W-P04 W-P05.
      *
           IF  W-HCC = 1
               IF  TDNWR2-GCN NOT = ZERO
                   MOVE TDNWR2-GCN TO P-GCN
               END-IF
           END-IF
           IF  TDNWR2-TSH NOT = ZERO
               MOVE "NO." TO P-NON
               MOVE TDNWR2-TSH TO P-TSH
           END-IF
           IF  TDNWR2-TKC = ZERO
               MOVE "ﾂｲｶﾌｶ" TO P-TKCD
           END-IF
      *
           MOVE TDNWR2-SHN TO P-SHN.
           IF  TDNWR2-JAN NOT = SPACE AND ZERO
               MOVE TDNWR2-JAN TO P-JAN
           ELSE
               MOVE TDNWR2-HCD TO P-JAN
           END-IF
           MOVE TDNWR2-ISU TO P-ISU.
           MOVE TDNWR2-KSU TO P-KSU.
           MOVE TDNWR2-HTC TO P-HTC.
           MOVE TDNWR2-SU TO P-SU.
           MOVE TDNWR2-GTN TO P-GTN.
           MOVE TDNWR2-GKIN TO P-GKIN.
           MOVE TDNWR2-UTN TO P-UTN.
           MOVE TDNWR2-UKIN TO P-UKIN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDNWR2-SU TO WT-SU.
           ADD TDNWR2-GKIN TO WT-GKIN.
           ADD TDNWR2-UKIN TO WT-UKIN.
       MEI2-EX.
           EXIT.
       KEI-RTN.
           IF  W-LC > 2
               MOVE SPACE TO W-P05
               MOVE "*ﾖﾊｸﾆ ﾊ ｷﾆｭｳ ｼﾅｲﾃﾞ ｸﾀﾞｻｲ*" TO P-SHN
               MOVE SPACE TO SP-R
               MOVE W-P05 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               SUBTRACT 2 FROM W-LC
           END-IF
           MOVE SPACE TO W-P05.
           MOVE WT-SU TO P-SU.
           MOVE WT-GKIN TO P-GKIN.
           MOVE WT-UKIN TO P-UKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_LineFeed" USING W-LC RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
       REW-RTN.
           MOVE SPACE TO TDNW1-KEY.
           MOVE W-STC TO TDNW1-STC.
           MOVE W-DNO TO TDNW1-DNO.
      *           START TDNWF KEY NOT < TDNW1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNWF_PNAME1 "TDNW1-KEY" " NOT < " TDNW1-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO REW-EX
           END-IF.
       REW-05.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO REW-EX
           END-IF
           IF  TDNW1-PC = 0 OR 9
               GO TO REW-05
           END-IF
           IF (TDNW1-STC NOT = W-STC) OR (TDNW1-DNO NOT = W-DNO)
               GO TO REW-EX
           END-IF
           MOVE 9 TO TDNW1-PC.
      *           REWRITE TDNW-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO REW-EX
           END-IF
           GO TO REW-05.
       REW-EX.
           EXIT.
       TST-RTN.
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
               MOVE SPACE TO W-P01 W-P02 W-P03 W-P04 W-P05
               MOVE W-20K TO P-20K
               MOVE "日進ゴム㈱" TO P-NR
               MOVE "-" TO P-V
               MOVE ALL "X" TO P-UBCD P-HCCD P-TNA P-DPC P-NON P-TKCD
                               P-SHN P-HTC
               MOVE 9 TO P-SHC
               MOVE 99 TO P-SCD P-BCD P-HNEN P-HGET P-HPEY
                          P-NNEN P-NGET P-NPEY
               MOVE 999 TO P-ISU
               MOVE 9999 TO P-TCD P-KSU
               MOVE 99999 TO P-TSH P-SU
               MOVE 999999 TO P-THC P-GCN P-GTN P-UTN
               MOVE 9999999 TO P-DNOD
               MOVE 999999999 TO P-GKIN P-UKIN
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P01 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-LC.
       TST-05.
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-LC.
           IF  W-LC < 9
               GO TO TST-05
           END-IF.
       TST-EX.
           EXIT.
