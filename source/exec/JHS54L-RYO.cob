       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS54L.
      *********************************************************
      *    PROGRAM         :  ìùàÍì`ï[î≠çsÅiê‘ÇøÇ·ÇÒñ{ï‹Åj    *
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
       77  W-INV              PIC  9(001).
       77  W-20K              PIC  X(005) VALUE X"1A24212474".
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  W-P02.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(019).
           02  P-BI           PIC  X(010).
           02  F              PIC  X(057).
           02  P-NNGP         PIC  9(006).
           02  F              PIC  X(018).
       01  W-P03.
           02  F              PIC  X(073).
           02  P-NRN          PIC  N(006).
           02  F              PIC  X(028).
       01  W-P04.
           02  F              PIC  X(007).
           02  P-SNA          PIC  N(008).
           02  F              PIC  X(053).
           02  P-NRA          PIC  N(014).
           02  F              PIC  X(028).
       01  W-P05.
           02  F              PIC  X(007).
           02  P-TNA          PIC  N(014).
           02  F              PIC  X(004).
           02  P-STC          PIC  9(007).
           02  F              PIC  X(008).
           02  P-DC           PIC  9(002).
           02  F              PIC  X(004).
           02  P-DNO          PIC  9(007).
           02  F              PIC  X(002).
           02  P-THC          PIC  9(006).
           02  F              PIC  X(006).
           02  P-NRT          PIC  X(017).
           02  F              PIC  X(003).
           02  P-HNGP         PIC  9(006).
           02  P-NGPS         PIC  9(006).
           02  F              PIC  X(004).
       01  W-PMO.
           02  F              PIC  X(006).
           02  P-DPM          PIC  X(002).
           02  P-CLS          PIC  X(003).
           02  F              PIC  X(002).
           02  P-MKH          PIC  X(010).
           02  F              PIC  X(035).
           02  P-NSU          PIC ZZZZ9.
           02  F              PIC  X(022).
           02  P-GKINS        PIC ZZZZZZZZ9.
           02  F              PIC  X(006).
           02  P-MSB          PIC  X(010).
           02  P-MSBD  REDEFINES  P-MSB.
             03  P-UKINS      PIC ZZZZZZZZ9B.
       01  W-PMU.
           02  F              PIC  X(006).
           02  P-SHM          PIC  X(013).
           02  F              PIC  X(011).
           02  P-JAN          PIC  X(013).
           02  F              PIC  X(001).
           02  P-COL          PIC  N(004).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  X(004).
           02  F              PIC  X(001).
           02  P-TY           PIC  X(002).
           02  P-SU           PIC  Z(005).
           02  P-SUV   REDEFINES P-SU    PIC  X(005).
           02  F              PIC  X(014).
           02  P-GTN          PIC  Z(006).
           02  F              PIC  X(002).
           02  P-GKIN         PIC  Z(009).
           02  P-GKINV REDEFINES P-GKIN  PIC  X(009).
           02  P-UTN          PIC  Z(006).
           02  P-UKIN         PIC  Z(009).
           02  P-UKINV REDEFINES P-UKIN  PIC  X(009).
           02  F              PIC  X(001).
       01  W-P11.
           02  F              PIC  X(008).
           02  P-HNO          PIC  9(009).
           02  F              PIC  X(008).
           02  P-HNA          PIC  X(006).
           02  F              PIC  X(027).
           02  P-NSUT         PIC  Z(005).
           02  F              PIC  X(047).
       01  W-P12.
           02  F              PIC  X(058).
           02  P-SUT          PIC  Z(005).
           02  P-SUTV   REDEFINES P-SUT    PIC  X(005).
           02  F              PIC  X(022).
           02  P-GKINT        PIC  Z(009).
           02  P-GKINTV REDEFINES P-GKINT  PIC  X(009).
           02  F              PIC  X(006).
           02  P-UKINT        PIC  Z(009).
           02  P-UKINTV REDEFINES P-UKINT  PIC  X(009).
           02  F              PIC  X(001).
       01  W-P13.
           02  F              PIC  X(007).
           02  P-ZONM         PIC  N(006).
           02  P-ZON          PIC  X(004).
           02  F              PIC  X(090).
       01  W-P15.
           02  P-20K          PIC  X(005).
           02  F              PIC  X(073).
           02  P-TGKIN        PIC  N(009).
           02  P-TUKIN        PIC  N(009).
           02  F              PIC  X(001).
       01  W-DATA.
           02  WT-D.
             03  W-TSC        PIC  9(001).
             03  WT-NSU       PIC  9(006).
             03  WT-GKINS     PIC  9(009).
             03  WT-UKINS     PIC  9(009).
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
             03  WT-TGKIN     PIC  9(009).
             03  WT-TUKIN     PIC  9(009).
           02  W-D.
             03  W-POC        PIC  9(001).
             03  W-TPC        PIC  9(001).
             03  W-FOC        PIC  9(001).
             03  W-HKC        PIC  9(001).
             03  W-SED.
               04  W-SSTC     PIC  9(007).
               04  W-SDNO     PIC  9(007).
               04  W-ESTC     PIC  9(007).
               04  W-EDNO     PIC  9(007).
             03  W-DMM        PIC  9(001).
             03  W-RSTC.
               04  W-RSCD     PIC  9(002).
               04  W-RTCD     PIC  9(003).
             03  W-STC        PIC  9(007).
             03  W-DNO        PIC  9(007).
             03  W-HCC        PIC  9(001).
             03  W-LC         PIC  9(002).
           02  W-GKINS        PIC  9(009).
           02  W-UKINS        PIC  9(009).
           02  W-TGKINZ       PIC  Z(009).
           02  W-TUKINZ       PIC  Z(009).
           02  CNT            PIC  9(002).
           02  CNTD           PIC  9(002).
           COPY LSTAT.
      *
           COPY LITDNA-RYO.
      *FD  TDNARF
       01  TDNARF_JHS54L.
           02  TDNARF_PNAME1  PIC  X(006) VALUE "TDNARF".
           02  F              PIC  X(001).
           02  TDNARF_LNAME   PIC  X(013) VALUE "TDNARF_JHS54L".
           02  F              PIC  X(001).
           02  TDNARF_KEY1    PIC  X(100) VALUE SPACE.
           02  TDNARF_SORT    PIC  X(100) VALUE SPACE.
           02  TDNARF_IDLST   PIC  X(100) VALUE SPACE.
           02  TDNARF_RES     USAGE  POINTER.
       01  TDNAR-R.
           02  TDNAR-KEY.
             03  TDNAR-STC    PIC  9(007).
             03  TDNAR-DNO    PIC  9(007).
             03  TDNAR-DGN    PIC  9(002).
           02  TDNAR-JAN      PIC  X(013).
           02  TDNAR-SU       PIC  9(006).
           02  TDNAR-GTN      PIC  9(007).
           02  TDNAR-UTN      PIC  9(007).
           02  TDNAR-GKIN     PIC  9(010).
           02  TDNAR-UKIN     PIC  9(010).
           02  TDNAR-DPM      PIC  X(002).
           02  TDNAR-CLS      PIC  X(003).
           02  TDNAR-SHM      PIC  X(013).
           02  TDNAR-MKH      PIC  X(010).
           02  TDNAR-MSB      PIC  X(010).
           02  TDNAR-TY       PIC  X(002).
           02  TDNAR-HCD      PIC  9(006).
           02  TDNAR-COR      PIC  N(004).
           02  TDNAR-SIZ      PIC  X(004).
           02  TDNAR-NSU      PIC  9(006).
           02  TDNAR-TSC      PIC  9(001).
           02  F              PIC  X(008).
           02  TDNAR-CCD      PIC  9(003).
           02  TDNAR-TNA      PIC  N(014).
           02  TDNAR-KEY2.
             03  TDNAR-HNO    PIC  9(009).
             03  TDNAR-HNGP   PIC  9(006).
           02  TDNAR-NNGP     PIC  9(006).
           02  TDNAR-THC      PIC  9(006).
           02  TDNAR-BI       PIC  X(010).
           02  TDNAR-SNGP     PIC  9(008).
           02  TDNAR-SNGPD REDEFINES TDNAR-SNGP.
             03  F            PIC  9(002).
             03  TDNAR-NGPS   PIC  9(006).
           02  TDNAR-HNA      PIC  X(006).
           02  TDNAR-ZON      PIC  X(004).
           02  TDNAR-DC       PIC  9(002).
           02  F              PIC  X(015).
           02  TDNAR-NRC      PIC  9(001).
           02  TDNAR-RNGP     PIC  9(008).
           02  TDNAR-PC       PIC  9(001).
           02  TDNAR-RC       PIC  9(001).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(014) VALUE
                "É`ÉFÅ[ÉìÉXÉgÉAìùàÍì`ï[Å@î≠çs".
           02  FILLER  PIC  N(013) VALUE
                "ÅiÉ^Å[ÉìÉAÉâÉEÉìÉhópÇQå^Åj".
           02  FILLER  PIC  N(008) VALUE
                "Åmê‘ÇøÇ·ÇÒñ{ï‹Ån".
           02  FILLER  PIC  X(035) VALUE
                "ÉeÉXÉgÉvÉäÉìÉgàÛéö (YES=1,NO=2) [ ]".
           02  FILLER  PIC  X(010) VALUE
                "ÇPÅDî≠Å@çs".
           02  FILLER  PIC  X(018) VALUE
                "ÇQÅDçƒî≠çsÅiìñì˙Åj".
           02  FILLER  PIC  X(026) VALUE
                "ÇRÅDçƒî≠çsÅià»ëOÅj     [ ]".
           02  FILLER  PIC  X(028) VALUE
                "ämîF (OK=1,NO=9) --->   ÿ¿∞›".
       01  C-DSP.
           02  D-SHM.
             03  FILLER  PIC  X(020) VALUE
                  " é–ìX∫∞ƒﬁ   ì`ï[î‘çÜ".
             03  FILLER  PIC  N(004) VALUE
                  "ÇeÇqÇnÇl".
             03  FILLER  PIC  N(002) VALUE
                  "ÇsÇn".
           02  D-SHMC.
             03  FILLER  PIC  X(020) VALUE
                  "                  ".
             03  FILLER  PIC  X(031) VALUE
                  "                               ".
             03  FILLER  PIC  X(031) VALUE
                  "                              ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-HKC   PIC  9(001).
           02  A-DMM   PIC  9(001).
           02  FILLER.
             03  A-SSTC  PIC  9(007).
             03  A-SDNO  PIC  9(007).
           02  FILLER.
             03  A-ESTC  PIC  9(007).
             03  A-EDNO  PIC  9(007).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
             03  E-ME10  PIC  X(027) VALUE
                  "***  TDNAF DATA ¥◊∞  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNAF REWRITE ¥◊∞  ***".
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
            "C-MID" " " "0" "0" "187" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "20" "28" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "2" "20" "26" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "4" "10" "16" "02C-MID" " " RETURNING RESU.
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
            "C-DSP" " " "0" "0" "114" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHM" " " "0" "0" "32" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHM" "X" "14" "33" "20" " " "D-SHM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHM" "N" "15" "22" "8" "01D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHM" "N" "16" "22" "4" "02D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHMC" " " "0" "0" "82" "D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHMC" "X" "14" "33" "20" " " "D-SHMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SHMC" "X" "15" "22" "31" "01D-SHMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SHMC" "X" "16" "22" "31" "02D-SHMC" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "31" " " " " RETURNING RESU.
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
            "04C-ACP" " " "15" "0" "14" "A-DMM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSTC" "9" "15" "34" "7" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSTC" BY REFERENCE W-SSTC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDNO" "9" "15" "45" "7" "A-SSTC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDNO" BY REFERENCE W-SDNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "16" "0" "14" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESTC" "9" "16" "34" "7" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESTC" BY REFERENCE W-ESTC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EDNO" "9" "16" "45" "7" "A-ESTC" " " RETURNING RESU.
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
           CALL "SD_Accept" USING BY REFERENCE A-SSTC "A-SSTC" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ESTC "A-ESTC" "9" "7"
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
                "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
                "TDNA-KEY" BY REFERENCE TDNA-KEY
           ELSE
               IF  W-HKC = 2
                   CALL "DB_F_Open" USING
                    "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE
                    TDNAF_IDLST "1" "TDNA-KEY" BY REFERENCE TDNA-KEY
               ELSE
                   CALL "DB_F_Open" USING
                    "INPUT" TDNARF_PNAME1 " " BY REFERENCE
                    TDNARF_IDLST "0"
               END-IF
           END-IF
           MOVE 1 TO W-FOC.
           IF  W-HKC = 3
               GO TO M-50
           END-IF.
       M-45.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TDNA-RC NOT = 0
               GO TO M-45
           END-IF
           IF  TDNA-PC = 0
               GO TO M-45
           END-IF
           IF  W-HKC = 1
               IF  TDNA-PC = 9
                   GO TO M-45
               END-IF
           END-IF
           IF W-HKC = 2
               IF  TDNA-PC NOT = 9
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNA-STC < W-SSTC OR > W-ESTC
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNA-DNO < W-SDNO OR > W-EDNO
                   GO TO M-45
               END-IF
           END-IF
           IF  TDNA-DGN NOT = 1
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
           GO TO M-55.
       M-50.
      *           READ TDNARF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNARF_PNAME1 BY REFERENCE TDNAR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TDNAR-PC = 0
               GO TO M-50
           END-IF
           IF  W-HKC = 1
               IF  TDNAR-PC = 9
                   GO TO M-50
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNAR-PC NOT = 9
                   GO TO M-50
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNAR-STC < W-SSTC OR > W-ESTC
                   GO TO M-50
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNAR-DNO < W-SDNO OR > W-EDNO
                   GO TO M-50
               END-IF
           END-IF
           IF  TDNAR-DGN NOT = 1
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
       M-55.
           IF  W-HKC = 1 OR 2
               MOVE TDNA-STC TO W-STC
               MOVE TDNA-DNO TO W-DNO
           ELSE
               MOVE TDNAR-STC TO W-STC
               MOVE TDNAR-DNO TO W-DNO
           END-IF
           MOVE ZERO TO WT-D.
           MOVE SPACE TO W-P02 W-P03 W-P04 W-P05
                         W-P11 W-P12 W-P13 W-P15.
           MOVE SPACE TO P-TGKIN P-TUKIN.
      *
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "áäê‘ÇøÇ·ÇÒñ{ï‹Å@" TO P-SNA.
           MOVE "ì˙êiÉSÉÄáäÅ@" TO P-NRN.
           MOVE "â™éRésñkãÊç°î™íöñ⁄ÇPÇUÅ|ÇPÇV" TO P-NRA.
           MOVE "TEL (086)243-2456" TO P-NRT.
           MOVE "î[ïiÉ]Å[Éì" TO P-ZONM.
           IF  W-HKC = 1 OR 2
               PERFORM HED1-RTN THRU HED1-EX
           ELSE
               PERFORM HED2-RTN THRU HED2-EX
           END-IF
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
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
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 20 TO W-LC.
       M-60.
           IF  W-HKC = 1 OR 2
               PERFORM MEI1-RTN THRU MEI1-EX
               GO TO M-65
           ELSE
               PERFORM MEI2-RTN THRU MEI2-EX
               GO TO M-70
           END-IF.
       M-65.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TDNA-RC NOT = 0
               GO TO M-65
           END-IF
           IF  TDNA-PC = 0
               GO TO M-65
           END-IF
           IF  W-HKC = 1
               IF  TDNA-PC = 9
                   GO TO M-65
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNA-PC NOT = 9
                   GO TO M-65
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNA-STC < W-SSTC OR > W-ESTC
                   GO TO M-65
               END-IF
           END-IF
           IF  W-HKC = 2
               IF  TDNA-DNO < W-SDNO OR > W-EDNO
                   GO TO M-65
               END-IF
           END-IF
           IF (TDNA-STC NOT = W-STC) OR (TDNA-DNO NOT = W-DNO)
               GO TO M-75
           END-IF
           IF  TDNA-DGN = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               GO TO M-55
           END-IF
           GO TO M-60.
       M-70.
      *           READ TDNARF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNARF_PNAME1 BY REFERENCE TDNAR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TDNAR-STC < W-SSTC OR > W-ESTC
               GO TO M-70
           END-IF
           IF  TDNAR-DNO < W-SDNO OR > W-EDNO
               GO TO M-70
           END-IF
           IF (TDNAR-STC NOT = W-STC) OR (TDNAR-DNO NOT = W-DNO)
               GO TO M-75
           END-IF
           IF  TDNAR-DGN = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               GO TO M-55
           END-IF
           GO TO M-60.
       M-75.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-HKC = 1
               PERFORM REW-RTN THRU REW-EX
           END-IF
           IF  W-HKC = 1 OR 2
               IF  TDNA-DGN NOT = 1
                   CALL "SD_Output" USING
                    "E-ME78" E-ME78 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME2" E-ME2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME78" E-ME78 "p" RETURNING RESU
                   GO TO M-55
               END-IF
           END-IF
           IF  W-HKC NOT = 1 AND 2
               IF  TDNAR-DGN NOT = 1
                   CALL "SD_Output" USING
                    "E-ME78" E-ME78 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME2" E-ME2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME78" E-ME78 "p" RETURNING RESU
                   GO TO M-55
               END-IF
           END-IF
           GO TO M-55.
       M-85.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-HKC = 1
               PERFORM REW-RTN THRU REW-EX
           END-IF.
       M-90.
           IF  W-FOC NOT = 0
               IF  W-HKC = 1 OR 2
                   CALL "DB_F_Close" USING
                    BY REFERENCE TDNAF_IDLST TDNAF_PNAME1
               ELSE
                   CALL "DB_F_Close" USING
                    BY REFERENCE TDNARF_IDLST TDNARF_PNAME1
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
      *
           MOVE TDNA-BI TO P-BI.
           MOVE TDNA-NNGP TO P-NNGP.
           MOVE TDNA-TNA TO P-TNA.
           MOVE TDNA-STC TO P-STC.
           MOVE TDNA-DC TO P-DC.
           MOVE TDNA-DNO TO P-DNO.
           MOVE TDNA-THC TO P-THC.
           MOVE TDNA-HNGP TO P-HNGP.
           MOVE TDNA-NGPS TO P-NGPS.
           MOVE TDNA-HNO TO P-HNO.
           MOVE TDNA-HNA TO P-HNA.
           MOVE TDNA-ZON TO P-ZON.
       HED1-EX.
           EXIT.
       HED2-RTN.
           MOVE TDNAR-BI TO P-BI.
           MOVE TDNAR-NNGP TO P-NNGP.
           MOVE TDNAR-TNA TO P-TNA.
           MOVE TDNAR-STC TO P-STC.
           MOVE TDNAR-DC TO P-DC.
           MOVE TDNAR-DNO TO P-DNO.
           MOVE TDNAR-THC TO P-THC.
           MOVE TDNAR-HNGP TO P-HNGP.
           MOVE TDNAR-NGPS TO P-NGPS.
           MOVE TDNAR-HNO TO P-HNO.
           MOVE TDNAR-HNA TO P-HNA.
           MOVE TDNAR-ZON TO P-ZON.
       HED2-EX.
           EXIT.
       MEI1-RTN.
           IF  TDNA-TSC NOT = 0
               MOVE 1 TO W-TSC
               COMPUTE W-GKINS = TDNA-NSU * TDNA-GTN
               COMPUTE W-UKINS = TDNA-NSU * TDNA-UTN
           END-IF
      *
           MOVE SPACE TO W-PMO W-PMU.
           MOVE TDNA-DPM TO P-DPM.
           MOVE TDNA-CLS TO P-CLS.
           MOVE TDNA-MKH TO P-MKH.
           IF  TDNA-TSC NOT = 0
               MOVE TDNA-NSU TO P-NSU
               MOVE W-GKINS TO P-GKINS
               MOVE W-UKINS TO P-UKINS
           ELSE
               MOVE TDNA-MSB TO P-MSB
           END-IF
           MOVE TDNA-SHM TO P-SHM.
           MOVE TDNA-JAN TO P-JAN.
           MOVE TDNA-COR TO P-COL.
           MOVE TDNA-SIZ TO P-SIZ.
           MOVE TDNA-TY TO P-TY.
           MOVE TDNA-SU TO P-SU.
           MOVE TDNA-GTN TO P-GTN.
           MOVE TDNA-GKIN TO P-GKIN.
           MOVE TDNA-UTN TO P-UTN.
           MOVE TDNA-UKIN TO P-UKIN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-PMO TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-PMU TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDNA-SU TO WT-SU.
           ADD TDNA-GKIN TO WT-GKIN.
           ADD TDNA-UKIN TO WT-UKIN.
           IF  TDNA-TSC = 0
               ADD TDNA-SU TO WT-NSU
               ADD TDNA-GKIN TO WT-TGKIN
               ADD TDNA-UKIN TO WT-TUKIN
               GO TO MEI1-EX
           END-IF
           ADD TDNA-NSU TO WT-NSU.
           ADD W-GKINS TO WT-TGKIN.
           ADD W-UKINS TO WT-TUKIN.
      *
           MOVE SPACE TO W-PMU.
           MOVE SPACE TO P-COL.
           IF  TDNA-SU < 10
               MOVE "    =" TO P-SUV
           ELSE
               IF  TDNA-SU < 100
                   MOVE "   ==" TO P-SUV
               ELSE
                   IF  TDNA-SU < 1000
                       MOVE "  ===" TO P-SUV
                   ELSE
                       IF  TDNA-SU < 10000
                           MOVE " ====" TO P-SUV
                       ELSE
                           MOVE "=====" TO P-SUV
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDNA-GKIN < 1000
               MOVE "      ===" TO P-GKINV
           ELSE
               IF  TDNA-GKIN < 10000
                   MOVE "     ====" TO P-GKINV
               ELSE
                   IF  TDNA-GKIN < 100000
                       MOVE "    =====" TO P-GKINV
                   ELSE
                       IF  TDNA-GKIN < 1000000
                           MOVE "   ======" TO P-GKINV
                       ELSE
                           IF  TDNA-GKIN < 10000000
                               MOVE "  =======" TO P-GKINV
                           ELSE
                               MOVE " ========" TO P-GKINV
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDNA-UKIN < 1000
               MOVE "      ===" TO P-UKINV
           ELSE
               IF  TDNA-UKIN < 10000
                   MOVE "     ====" TO P-UKINV
               ELSE
                   IF  TDNA-UKIN < 100000
                       MOVE "    =====" TO P-UKINV
                   ELSE
                       IF  TDNA-UKIN < 1000000
                           MOVE "   ======" TO P-UKINV
                       ELSE
                           IF  TDNA-UKIN < 10000000
                               MOVE "  =======" TO P-UKINV
                           ELSE
                               MOVE " ========" TO P-UKINV
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PMU TO SP-R.
           CALL "PR_LineFeed" USING "0" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI1-EX.
           EXIT.
       MEI2-RTN.
           IF  TDNAR-TSC NOT = 0
               MOVE 1 TO W-TSC
               COMPUTE W-GKINS = TDNAR-NSU * TDNAR-GTN
               COMPUTE W-UKINS = TDNAR-NSU * TDNAR-UTN
           END-IF
      *
           MOVE SPACE TO W-PMO W-PMU.
           MOVE TDNAR-DPM TO P-DPM.
           MOVE TDNAR-CLS TO P-CLS.
           MOVE TDNAR-MKH TO P-MKH.
           IF  TDNAR-TSC NOT = 0
               MOVE TDNAR-NSU TO P-NSU
               MOVE W-GKINS TO P-GKINS
               MOVE W-UKINS TO P-UKINS
           ELSE
               MOVE TDNAR-MSB TO P-MSB
           END-IF
           MOVE TDNAR-SHM TO P-SHM.
           MOVE TDNAR-JAN TO P-JAN.
           MOVE TDNAR-COR TO P-COL.
           MOVE TDNAR-SIZ TO P-SIZ.
           MOVE TDNAR-TY TO P-TY.
           MOVE TDNAR-SU TO P-SU.
           MOVE TDNAR-GTN TO P-GTN.
           MOVE TDNAR-GKIN TO P-GKIN.
           MOVE TDNAR-UTN TO P-UTN.
           MOVE TDNAR-UKIN TO P-UKIN.
      *
           MOVE SPACE TO SP-R.
           MOVE W-PMO TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-PMU TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           SUBTRACT 2 FROM W-LC.
           ADD TDNAR-SU TO WT-SU.
           ADD TDNAR-GKIN TO WT-GKIN.
           ADD TDNAR-UKIN TO WT-UKIN.
           IF  TDNAR-TSC = 0
               ADD TDNAR-SU TO WT-NSU
               ADD TDNAR-GKIN TO WT-TGKIN
               ADD TDNAR-UKIN TO WT-TUKIN
               GO TO MEI2-EX
           END-IF
           ADD TDNAR-NSU TO WT-NSU.
           ADD W-GKINS TO WT-TGKIN.
           ADD W-UKINS TO WT-TUKIN.
      *
           MOVE SPACE TO W-PMU.
           MOVE SPACE TO P-COL.
           IF  TDNAR-SU < 10
               MOVE "    =" TO P-SUV
           ELSE
               IF  TDNAR-SU < 100
                   MOVE "   ==" TO P-SUV
               ELSE
                   IF  TDNAR-SU < 1000
                       MOVE "  ===" TO P-SUV
                   ELSE
                       IF  TDNAR-SU < 10000
                           MOVE " ====" TO P-SUV
                       ELSE
                           MOVE "=====" TO P-SUV
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDNAR-GKIN < 1000
               MOVE "      ===" TO P-GKINV
           ELSE
               IF  TDNAR-GKIN < 10000
                   MOVE "     ====" TO P-GKINV
               ELSE
                   IF  TDNAR-GKIN < 100000
                       MOVE "    =====" TO P-GKINV
                   ELSE
                       IF  TDNAR-GKIN < 1000000
                           MOVE "   ======" TO P-GKINV
                       ELSE
                           IF  TDNAR-GKIN < 10000000
                               MOVE "  =======" TO P-GKINV
                           ELSE
                               MOVE " ========" TO P-GKINV
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDNAR-UKIN < 1000
               MOVE "      ===" TO P-UKINV
           ELSE
               IF  TDNAR-UKIN < 10000
                   MOVE "     ====" TO P-UKINV
               ELSE
                   IF  TDNAR-UKIN < 100000
                       MOVE "    =====" TO P-UKINV
                   ELSE
                       IF  TDNAR-UKIN < 1000000
                           MOVE "   ======" TO P-UKINV
                       ELSE
                           IF  TDNAR-UKIN < 10000000
                               MOVE "  =======" TO P-UKINV
                           ELSE
                               MOVE " ========" TO P-UKINV
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PMU TO SP-R.
           CALL "PR_LineFeed" USING "0" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI2-EX.
           EXIT.
       KEI-RTN.
           IF  W-TSC = 1
               MOVE WT-NSU TO P-NSUT
           END-IF
           SUBTRACT 1 FROM W-LC.
           MOVE SPACE TO SP-R.
           MOVE W-P11 TO SP-R.
           CALL "PR_LineFeed" USING W-LC RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE WT-SU TO P-SUT.
           MOVE WT-GKIN TO P-GKINT.
           MOVE WT-UKIN TO P-UKINT.
           MOVE SPACE TO SP-R.
           MOVE W-P12 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-TSC NOT = 1
               GO TO KEI-010
           END-IF
           MOVE SPACE TO W-P12.
           IF  WT-SU < 10
               MOVE "    =" TO P-SUTV
           ELSE
               IF  WT-SU < 100
                   MOVE "   ==" TO P-SUTV
               ELSE
                   IF  WT-SU < 1000
                       MOVE "  ===" TO P-SUTV
                   ELSE
                       IF  WT-SU < 10000
                           MOVE " ====" TO P-SUTV
                       ELSE
                           MOVE "=====" TO P-SUTV
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  WT-GKIN < 1000
               MOVE "      ===" TO P-GKINTV
           ELSE
               IF  WT-GKIN < 10000
                   MOVE "     ====" TO P-GKINTV
               ELSE
                   IF  WT-GKIN < 100000
                       MOVE "    =====" TO P-GKINTV
                   ELSE
                       IF  WT-GKIN < 1000000
                           MOVE "   ======" TO P-GKINTV
                       ELSE
                           IF  WT-GKIN < 10000000
                               MOVE "  =======" TO P-GKINTV
                           ELSE
                               MOVE " ========" TO P-GKINTV
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  WT-UKIN < 1000
               MOVE "      ===" TO P-UKINTV
           ELSE
               IF  WT-UKIN < 10000
                   MOVE "     ====" TO P-UKINTV
               ELSE
                   IF  WT-UKIN < 100000
                       MOVE "    =====" TO P-UKINTV
                   ELSE
                       IF  WT-UKIN < 1000000
                           MOVE "   ======" TO P-UKINTV
                       ELSE
                           IF  WT-UKIN < 10000000
                               MOVE "  =======" TO P-UKINTV
                           ELSE
                               MOVE " ========" TO P-UKINTV
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P12 TO SP-R.
           CALL "PR_LineFeed" USING "0" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-010.
           MOVE SPACE TO SP-R.
           MOVE W-P13 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-TSC = 1
               MOVE WT-TGKIN TO W-TGKINZ
               MOVE WT-TUKIN TO W-TUKINZ
               MOVE W-TGKINZ TO P-TGKIN
               MOVE W-TUKINZ TO P-TUKIN
           END-IF
           MOVE W-P15 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
       REW-RTN.
           MOVE SPACE TO TDNA-KEY.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
      *           START TDNAF KEY NOT < TDNA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNAF_PNAME1 "TDNA-KEY" " NOT < " TDNA-KEY RETURNING RET.
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
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO REW-EX
           END-IF
           IF (TDNA-STC NOT = W-STC) OR (TDNA-DNO NOT = W-DNO)
               GO TO REW-EX
           END-IF
           MOVE 9 TO TDNA-PC.
      *           REWRITE TDNA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
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
               MOVE SPACE TO W-P02 W-P03 W-P04 W-P05 W-PMO W-PMU
               MOVE SPACE TO W-P11 W-P12 W-P13 W-P15
               MOVE W-15K TO P-15K
               MOVE W-20K TO P-20K
               MOVE "áäê‘ÇøÇ·ÇÒñ{ï‹Å@" TO P-SNA
               MOVE "ÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÅ@" TO P-TNA
               MOVE "ì˙êiÉSÉÄáäÅ@" TO P-NRN
               MOVE "â™éRésñkãÊç°î™íöñ⁄ÇPÇUÅ|ÇPÇV" TO P-NRA
               MOVE "TEL (086)243-2456" TO P-NRT
               MOVE "ÇmÇmÇmÇm" TO P-COL
               MOVE "î[ïiÉ]Å[ÉìÅ@" TO P-ZONM
               MOVE "Å@ÇXÇXÇXÇXÇXÇXÇXÇX" TO P-TGKIN P-TUKIN
               MOVE ALL "X" TO P-BI P-DPM P-CLS P-MKH P-SHM P-JAN
                               P-SIZ P-HNA P-ZON
               MOVE 99 TO P-DC
               MOVE 99999 TO P-NSU P-SU P-GTN P-UTN
               MOVE 999999 TO P-NNGP P-THC P-HNGP P-NGPS P-SUT P-NSUT
               MOVE 9999999 TO P-STC P-DNO
               MOVE 999999999 TO P-GKINS P-UKINS P-GKIN P-UKIN P-HNO
                                 P-GKINT P-UKINT
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
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
           MOVE W-P05 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-LC.
       TST-05.
           MOVE SPACE TO SP-R.
           MOVE W-PMO TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-PMU TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-LC.
           IF  W-LC < 9
               GO TO TST-05
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P11 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P12 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P13 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P15 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TST-EX.
           EXIT.
