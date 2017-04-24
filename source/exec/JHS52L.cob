       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS52L.
      *********************************************************
      *    PROGRAM         :  統一伝票発行（ナフコ）          *
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
       77  W-20K              PIC  X(005) VALUE X"1A24212078".
       01  W-P01.
           02  P-20K          PIC  X(005).
           02  F              PIC  X(006).
           02  P-AR           PIC  X(007).
           02  F              PIC  X(005).
           02  P-DUR          PIC  X(026).
           02  F              PIC  X(049).
           02  P-ER           PIC  X(005).
           02  F              PIC  X(011).
       01  W-P02.
           02  F              PIC  X(018).
           02  P-DSHR         PIC  X(014).
           02  F              PIC  X(007).
           02  P-DSMR         PIC  X(007).
           02  F              PIC  X(028).
           02  P-TSN          PIC  X(015).
           02  F              PIC  X(020).
       01  W-P03.
           02  F              PIC  X(009).
           02  P-SNA          PIC  X(015).
           02  F              PIC  X(085).
       01  W-P04.
           02  F              PIC  X(009).
           02  P-TNA          PIC  X(015).
           02  F              PIC  X(009).
           02  P-SCD          PIC  9(002).
           02  P-TCD          PIC  9(003).
           02  F              PIC  X(005).
           02  P-BCD          PIC  9(002).
           02  F              PIC  X(002).
           02  P-DPC          PIC  9(002).
           02  F              PIC  X(005).
           02  P-DNOD         PIC  9(007).
           02  F              PIC  X(001).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(006).
           02  P-TST          PIC  X(012).
           02  F              PIC  X(008).
           02  P-HNEN         PIC  9(002).
           02  P-HGET         PIC Z9.
           02  P-HPEY         PIC Z9.
           02  P-NNEN         PIC  9(002).
           02  P-NGET         PIC Z9.
           02  P-NPEY         PIC Z9.
           02  F              PIC  X(003).
       01  W-P05.
           02  F              PIC  X(005).
           02  P-SHN          PIC  X(025).
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(001).
           02  P-COR          PIC  X(006).
           02  P-SIZ          PIC  X(005).
           02  F              PIC  X(027).
           02  P-GTN1         PIC  Z(001).
           02  F              PIC  X(016).
           02  P-UTN1         PIC  Z(001).
           02  F              PIC  X(009).
       01  W-P06.
           02  F              PIC  X(005).
           02  P-KKK          PIC  X(025).
           02  P-HSC          PIC  X(008).
           02  F              PIC  X(006).
           02  P-GAR          PIC  X(006).
           02  F              PIC  X(005).
           02  P-TNI          PIC  X(003).
           02  P-SU           PIC  Z(005).
           02  F              PIC  X(014).
           02  P-GTN2         PIC  Z(006).
           02  F              PIC  X(002).
           02  P-GKIN         PIC  Z(009).
           02  P-UTN2         PIC  Z(006).
           02  P-UKIN         PIC  Z(009).
       01  W-P07.
           02  F              PIC  X(006).
           02  P-FUR          PIC  X(007).
           02  F              PIC  X(096).
       01  W-P08.
           02  F              PIC  X(006).
           02  P-FSR          PIC  X(015).
           02  F              PIC  X(014).
           02  P-LUR          PIC  X(020).
           02  F              PIC  X(003).
           02  P-SUT          PIC  Z(005).
           02  F              PIC  X(022).
           02  P-GKINT        PIC  Z(009).
           02  F              PIC  X(006).
           02  P-UKINT        PIC  Z(009).
       01  W-P09.
           02  F              PIC  X(035).
           02  P-LCR          PIC  X(016).
           02  F              PIC  X(058).
       01  W-P10.
           02  F              PIC  X(037).
           02  P-LSR          PIC  X(007).
           02  F              PIC  X(065).
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
                 05  W-STCD   PIC  9(003).
               04  W-ESTC.
                 05  W-ESCD   PIC  9(002).
                 05  W-ETCD   PIC  9(003).
               04  W-SDNO     PIC  9(007).
               04  W-EDNO     PIC  9(007).
             03  W-DMM        PIC  9(001).
             03  W-RSTC.
               04  W-RSCD     PIC  9(002).
               04  W-RTCD     PIC  9(003).
             03  W-STC        PIC  9(009).
             03  W-DNO        PIC  9(009).
             03  W-LC         PIC  9(002).
           COPY LSTAT.
      *
           COPY LITDNN.
      *FD  TDNNRF
       01  TDNNRF_JHS52L.
           02  TDNNRF_PNAME1  PIC  X(006) VALUE "TDNNRF".
           02  F              PIC  X(001).
           02  TDNNRF_LNAME   PIC  X(013) VALUE "TDNNRF_JHS52L".
           02  F              PIC  X(001).
           02  TDNNRF_KEY1    PIC  X(100) VALUE SPACE.
           02  TDNNRF_SORT    PIC  X(100) VALUE SPACE.
           02  TDNNRF_IDLST   PIC  X(100) VALUE SPACE.
           02  TDNNRF_RES     USAGE  POINTER.
       01  TDNNR-R.
           02  TDNNR-R1.
               03  TDNNR1-KEY.
                 04  TDNNR1-STC.
                   05  TDNNR1-SCD PIC  9(002).
                   05  TDNNR1-TCD PIC  9(003).
                   05  F          PIC  X(004).
                 04  TDNNR1-DNO.
                   05  F          PIC  X(002).
                   05  TDNNR1-DNOD PIC  9(007).
                 04  TDNNR1-DGN   PIC  9(002).
               03  F              PIC  X(002).
               03  TDNNR1-BCD     PIC  9(002).
               03  TDNNR1-DPC     PIC  9(002).
               03  TDNNR1-HNGP    PIC  9(006).
               03  TDNNR1-HNGPD REDEFINES TDNNR1-HNGP.
                 04  TDNNR1-HNEN  PIC  9(002).
                 04  TDNNR1-HGET  PIC  9(002).
                 04  TDNNR1-HPEY  PIC  9(002).
               03  TDNNR1-NNGP    PIC  9(006).
               03  TDNNR1-NNGPD REDEFINES TDNNR1-NNGP.
                 04  TDNNR1-NNEN  PIC  9(002).
                 04  TDNNR1-NGET  PIC  9(002).
                 04  TDNNR1-NPEY  PIC  9(002).
               03  TDNNR1-THC     PIC  9(006).
               03  TDNNR1-STA     PIC  X(002).
               03  TDNNR1-SNA     PIC  X(015).
               03  TDNNR1-TNA     PIC  X(015).
               03  TDNNR1-TSN     PIC  X(015).
               03  TDNNR1-TST     PIC  X(012).
               03  TDNNR1-HCC     PIC  9(001).
               03  F              PIC  X(024).
               03  TDNNR1-AR      PIC  X(007).
               03  TDNNR1-DUR     PIC  X(026).
               03  TDNNR1-DSHR    PIC  X(014).
               03  TDNNR1-DSMR    PIC  X(007).
               03  TDNNR1-ER      PIC  X(005).
               03  TDNNR1-FSR     PIC  X(015).
               03  TDNNR1-FUR     PIC  X(007).
               03  TDNNR1-LCR     PIC  X(016).
               03  TDNNR1-LUR     PIC  X(020).
               03  TDNNR1-LSR     PIC  X(007).
               03  F              PIC  X(003).
               03  TDNNR1-PC      PIC  9(001).
               03  F              PIC  X(085).
           02  TDNNR-R2  REDEFINES TDNNR-R1.
               03  TDNNR2-KEY.
                 04  TDNNR2-STC.
                   05  TDNNR2-SCD PIC  9(002).
                   05  TDNNR2-TCD PIC  9(003).
                   05  F          PIC  X(004).
                 04  TDNNR2-DNO.
                   05  F          PIC  X(002).
                   05  TDNNR2-DNOD PIC  9(007).
                 04  TDNNR2-DGN   PIC  9(002).
               03  TDNNR2-JAN     PIC  X(013).
               03  TDNNR2-GAR     PIC  X(006).
               03  F              PIC  X(001).
               03  TDNNR2-TNI     PIC  X(003).
               03  TDNNR2-SU      PIC  9(005).
               03  F              PIC  X(001).
               03  TDNNR2-GTN     PIC  9(007).
               03  TDNNR2-GTND  REDEFINES TDNNR2-GTN.
                 04  TDNNR2-GTN1  PIC  9(001).
                 04  TDNNR2-GTN2  PIC  9(006).
               03  F              PIC  X(002).
               03  TDNNR2-UTN     PIC  9(007).
               03  TDNNR2-UTND  REDEFINES TDNNR2-UTN.
                 04  TDNNR2-UTN1  PIC  9(001).
                 04  TDNNR2-UTN2  PIC  9(006).
               03  TDNNR2-GKIN    PIC  9(010).
               03  TDNNR2-UKIN    PIC  9(010).
               03  F              PIC  X(009).
               03  TDNNR2-SHN     PIC  X(025).
               03  TDNNR2-HSC     PIC  X(008).
               03  TDNNR2-COR     PIC  X(006).
               03  TDNNR2-SIZ     PIC  X(005).
               03  F              PIC  X(004).
               03  TDNNR2-KKK     PIC  X(025).
               03  TDNNR2-PCH     PIC  X(001).
               03  TDNNR2-PSI     PIC  X(001).
               03  TDNNR2-PBM     PIC  9(002).
               03  TDNNR2-PJAN    PIC  X(013).
               03  TDNNR2-PSHN    PIC  X(020).
               03  TDNNR2-PKKK    PIC  X(020).
               03  TDNNR2-PUTN    PIC  9(007).
               03  TDNNR2-PMS     PIC  9(005).
               03  F              PIC  X(019).
               03  TDNNR2-PC      PIC  9(001).
               03  F              PIC  X(085).
       77  F                  PIC  X(001).
      *FD  SP-F
       77  SP-R               PIC  X(170).
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
                "［ナ　フ　コ］".
           02  FILLER  PIC  X(035) VALUE
                "テストプリント印字 (YES=1,NO=2) [ ]".
           02  FILLER  PIC  X(010) VALUE
                "１．発　行".
           02  FILLER  PIC  X(018) VALUE
                "２．再発行（当日）".
           02  FILLER  PIC  X(026) VALUE
                "３．　〃　（以前）     [ ]".
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
             03  A-SSTC  PIC  9(005).
             03  A-SDNO  PIC  9(007).
           02  FILLER.
             03  A-ESTC  PIC  9(005).
             03  A-EDNO  PIC  9(007).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  TDNNF DATA ｴﾗｰ  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNNF REWRITE ｴﾗｰ  ***".
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
            "C-ACP" " " "0" "0" "27" " " " " RETURNING RESU.
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
            "04C-ACP" " " "15" "0" "12" "A-DMM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSTC" "9" "15" "34" "5" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSTC" BY REFERENCE W-SSTC "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDNO" "9" "15" "44" "7" "A-SSTC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDNO" BY REFERENCE W-SDNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "16" "0" "12" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESTC" "9" "16" "34" "5" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESTC" BY REFERENCE W-ESTC "5" "0" RETURNING RESU.
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
           CALL "SD_Accept" USING BY REFERENCE A-SSTC "A-SSTC" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ESTC "A-ESTC" "9" "5"
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
                "I-O" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
                "TDNN1-KEY" BY REFERENCE TDNN1-KEY
           ELSE
               IF  W-HKC = 2
                   CALL "DB_F_Open" USING
                    "INPUT" TDNNF_PNAME1 "SHARED" BY REFERENCE
                    TDNNF_IDLST "1" "TDNN1-KEY" BY REFERENCE TDNN1-KEY
               ELSE
                   CALL "DB_F_Open" USING
                    "INPUT" TDNNRF_PNAME1 " " BY REFERENCE
                    TDNNRF_IDLST "0"
               END-IF
           END-IF
           MOVE 1 TO W-FOC.
           IF  W-HKC = 3
               GO TO M-46
           END-IF.
       M-45.
      *           READ TDNNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TDNN1-PC = 0
               GO TO M-45
           END-IF
           IF  W-HKC = 1
               IF TDNN1-PC = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNN1-PC NOT = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               MOVE TDNN1-SCD TO W-RSCD
               MOVE TDNN1-TCD TO W-RTCD
               IF  W-RSTC < W-SSTC OR > W-ESTC
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNN1-DNOD < W-SDNO OR > W-EDNO
                   GO TO M-45
               END-IF
           END-IF
           IF  TDNN1-DGN NOT = 0
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
      *           READ TDNNRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNNRF_PNAME1 BY REFERENCE TDNNR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE TDNNR1-SCD TO W-RSCD.
           MOVE TDNNR1-TCD TO W-RTCD.
           IF  W-RSTC < W-SSTC OR > W-ESTC
               GO TO M-46
           END-IF
           IF  TDNNR1-DNOD < W-SDNO OR > W-EDNO
               GO TO M-46
           END-IF
           IF  TDNNR1-DGN NOT = 0
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
               MOVE TDNN1-STC TO W-STC
               MOVE TDNN1-DNO TO W-DNO
           ELSE
               MOVE TDNNR1-STC TO W-STC
               MOVE TDNNR1-DNO TO W-DNO
           END-IF
           MOVE ZERO TO WT-D.
           MOVE SPACE TO W-P01 W-P02 W-P03 W-P04
                         W-P07 W-P08 W-P09 W-P10.
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
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 20 TO W-LC.
           IF  W-HKC = 3
               GO TO M-56
           END-IF.
       M-55.
      *           READ TDNNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TDNN1-PC = 0
               GO TO M-55
           END-IF
           IF  W-HKC = 1
               IF  TDNN1-PC = 9
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNN1-PC NOT = 9
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC = 2
               MOVE TDNN1-SCD TO W-RSCD
               MOVE TDNN1-TCD TO W-RTCD
               IF  W-RSTC < W-SSTC OR > W-ESTC
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNN1-DNOD < W-SDNO OR > W-EDNO
                   GO TO M-55
               END-IF
           END-IF
           IF (TDNN1-STC NOT = W-STC) OR (TDNN1-DNO NOT = W-DNO)
               GO TO M-60
           END-IF
           IF  TDNN1-DGN = ZERO
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
      *           READ TDNNRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNNRF_PNAME1 BY REFERENCE TDNNR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           MOVE TDNNR1-SCD TO W-RSCD.
           MOVE TDNNR1-TCD TO W-RTCD.
           IF  W-RSTC < W-SSTC OR > W-ESTC
               GO TO M-56
           END-IF
           IF  TDNNR1-DNOD < W-SDNO OR > W-EDNO
               GO TO M-56
           END-IF
           IF (TDNNR1-STC NOT = W-STC) OR (TDNNR1-DNO NOT = W-DNO)
               GO TO M-60
           END-IF
           IF  TDNNR1-DGN = ZERO
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
           ELSE
               PERFORM MEI2-RTN THRU MEI2-EX
           END-IF
           IF  W-HKC = 1 OR 2
               GO TO M-55
           ELSE
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
                    BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
               ELSE
                   CALL "DB_F_Close" USING
                    BY REFERENCE TDNNRF_IDLST TDNNRF_PNAME1
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
           MOVE TDNN1-AR TO P-AR.
           MOVE TDNN1-DUR TO P-DUR.
           MOVE TDNN1-ER TO P-ER.
      *
           MOVE TDNN1-DSHR TO P-DSHR.
           MOVE TDNN1-DSMR TO P-DSMR.
           MOVE TDNN1-TSN TO P-TSN.
      *
           MOVE TDNN1-SNA TO P-SNA.
      *
           MOVE TDNN1-TNA TO P-TNA.
           MOVE TDNN1-SCD TO P-SCD.
           MOVE TDNN1-TCD TO P-TCD.
           MOVE TDNN1-BCD TO P-BCD.
           MOVE TDNN1-DPC TO P-DPC.
           MOVE TDNN1-DNOD TO P-DNOD.
           MOVE TDNN1-THC TO P-THC.
           MOVE TDNN1-TST TO P-TST.
           MOVE TDNN1-HNEN TO P-HNEN.
           MOVE TDNN1-HGET TO P-HGET.
           MOVE TDNN1-HPEY TO P-HPEY.
           MOVE TDNN1-NNEN TO P-NNEN.
           MOVE TDNN1-NGET TO P-NGET.
           MOVE TDNN1-NPEY TO P-NPEY.
      *
           MOVE TDNN1-FUR TO P-FUR.
      *
           MOVE TDNN1-FSR TO P-FSR.
           MOVE TDNN1-LUR TO P-LUR.
      *
           MOVE TDNN1-LCR TO P-LCR.
      *
           MOVE TDNN1-LSR TO P-LSR.
       HED1-EX.
           EXIT.
       HED2-RTN.
           MOVE TDNNR1-AR TO P-AR.
           MOVE TDNNR1-DUR TO P-DUR.
           MOVE TDNNR1-ER TO P-ER.
      *
           MOVE TDNNR1-DSHR TO P-DSHR.
           MOVE TDNNR1-DSMR TO P-DSMR.
           MOVE TDNNR1-TSN TO P-TSN.
      *
           MOVE TDNNR1-SNA TO P-SNA.
      *
           MOVE TDNNR1-TNA TO P-TNA.
           MOVE TDNNR1-SCD TO P-SCD.
           MOVE TDNNR1-TCD TO P-TCD.
           MOVE TDNNR1-BCD TO P-BCD.
           MOVE TDNNR1-DPC TO P-DPC.
           MOVE TDNNR1-DNOD TO P-DNOD.
           MOVE TDNNR1-THC TO P-THC.
           MOVE TDNNR1-TST TO P-TST.
           MOVE TDNNR1-HNEN TO P-HNEN.
           MOVE TDNNR1-HGET TO P-HGET.
           MOVE TDNNR1-HPEY TO P-HPEY.
           MOVE TDNNR1-NNEN TO P-NNEN.
           MOVE TDNNR1-NGET TO P-NGET.
           MOVE TDNNR1-NPEY TO P-NPEY.
      *
           MOVE TDNNR1-FUR TO P-FUR.
      *
           MOVE TDNNR1-FSR TO P-FSR.
           MOVE TDNNR1-LUR TO P-LUR.
      *
           MOVE TDNNR1-LCR TO P-LCR.
      *
           MOVE TDNNR1-LSR TO P-LSR.
       HED2-EX.
           EXIT.
       MEI1-RTN.
           MOVE SPACE TO W-P05 W-P06.
      *
           MOVE TDNN2-SHN TO P-SHN.
           MOVE TDNN2-JAN TO P-JAN.
           MOVE TDNN2-COR TO P-COR.
           MOVE TDNN2-SIZ TO P-SIZ.
           MOVE TDNN2-GTN1 TO P-GTN1.
           MOVE TDNN2-UTN1 TO P-UTN1.
      *
           MOVE TDNN2-KKK TO P-KKK.
           MOVE TDNN2-HSC TO P-HSC.
           MOVE TDNN2-GAR TO P-GAR.
           MOVE TDNN2-TNI TO P-TNI.
           MOVE TDNN2-SU TO P-SU.
           MOVE TDNN2-GTN2 TO P-GTN2.
           MOVE TDNN2-GKIN TO P-GKIN.
           MOVE TDNN2-UTN2 TO P-UTN2.
           MOVE TDNN2-UKIN TO P-UKIN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P06 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDNN2-SU TO WT-SU.
           ADD TDNN2-GKIN TO WT-GKIN.
           ADD TDNN2-UKIN TO WT-UKIN.
       MEI1-EX.
           EXIT.
       MEI2-RTN.
           MOVE SPACE TO W-P05 W-P06.
      *
           MOVE TDNNR2-SHN TO P-SHN.
           MOVE TDNNR2-JAN TO P-JAN.
           MOVE TDNNR2-COR TO P-COR.
           MOVE TDNNR2-SIZ TO P-SIZ.
           MOVE TDNNR2-GTN1 TO P-GTN1.
           MOVE TDNNR2-UTN1 TO P-UTN1.
      *
           MOVE TDNNR2-KKK TO P-KKK.
           MOVE TDNNR2-HSC TO P-HSC.
           MOVE TDNNR2-GAR TO P-GAR.
           MOVE TDNNR2-TNI TO P-TNI.
           MOVE TDNNR2-SU TO P-SU.
           MOVE TDNNR2-GTN2 TO P-GTN2.
           MOVE TDNNR2-GKIN TO P-GKIN.
           MOVE TDNNR2-UTN2 TO P-UTN2.
           MOVE TDNNR2-UKIN TO P-UKIN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P06 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDNNR2-SU TO WT-SU.
           ADD TDNNR2-GKIN TO WT-GKIN.
           ADD TDNNR2-UKIN TO WT-UKIN.
       MEI2-EX.
           EXIT.
       KEI-RTN.
           MOVE WT-SU TO P-SUT.
           MOVE WT-GKIN TO P-GKINT.
           MOVE WT-UKIN TO P-UKINT.
           SUBTRACT 1 FROM W-LC.
      *
           MOVE SPACE TO SP-R.
           MOVE W-P07 TO SP-R.
           CALL "PR_LineFeed" USING W-LC RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P08 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P09 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P10 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
       REW-RTN.
           MOVE SPACE TO TDNN1-KEY.
           MOVE W-STC TO TDNN1-STC.
           MOVE W-DNO TO TDNN1-DNO.
      *           START TDNNF KEY NOT < TDNN1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNNF_PNAME1 "TDNN1-KEY" " NOT < " TDNN1-KEY RETURNING RET.
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
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-EX
           END-IF
           IF  TDNN1-PC = 0 OR 9
               GO TO REW-05
           END-IF
           IF (TDNN1-STC NOT = W-STC) OR (TDNN1-DNO NOT = W-DNO)
               GO TO REW-EX
           END-IF
           MOVE 9 TO TDNN1-PC.
      *           REWRITE TDNN-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
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
                             W-P06 W-P07 W-P08 W-P09 W-P10
               MOVE W-20K TO P-20K
               MOVE ALL "X" TO P-AR P-DUR P-ER P-DSHR P-DSMR P-TSN
                               P-SNA P-TNA P-TST P-SHN P-JAN P-COR
                               P-SIZ P-KKK P-HSC P-GAR P-TNI P-FUR
                               P-FSR P-LUR P-LCR P-LSR
               MOVE 99 TO P-SCD P-BCD P-DPC P-HNEN P-HGET P-HPEY
                          P-NNEN P-NGET P-NPEY
               MOVE 999 TO P-TCD
               MOVE 99999 TO P-SU P-SUT
               MOVE 999999 TO P-THC P-GTN2 P-UTN2
               MOVE 9999999 TO P-DNOD
               MOVE 999999999 TO P-GKIN P-UKIN P-GKINT P-UKINT
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
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-LC.
       TST-05.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P06 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-LC.
           IF  W-LC < 9
               GO TO TST-05
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P07 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P08 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P09 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P10 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TST-EX.
           EXIT.
