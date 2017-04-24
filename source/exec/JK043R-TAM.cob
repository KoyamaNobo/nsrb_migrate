       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JK043R.
      *********************************************************
      *    PROGRAM         :  ìùàÍì`ï[Å@ñ‚çáÇπ                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SJK043                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-NAME             PIC  N(007).
       01  W-DATA.
           02  W-SEN          PIC  9(001).
           02  W-SED.
             03  W-SSTC.
               04  W-SSCD     PIC  9(002).
               04  W-STCD     PIC  9(004).
             03  W-ESTC.
               04  W-ESCD     PIC  9(002).
               04  W-ETCD     PIC  9(004).
             03  W-SDNO       PIC  9(007).
             03  W-EDNO       PIC  9(007).
             03  W-SNHD       PIC  9(006).
             03  W-ENHD       PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-D.
             03  W-RSTC.
               04  W-RSCD     PIC  9(002).
               04  W-RTCD     PIC  9(004).
             03  W-DC         PIC  9(001).
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
           02  W-DSPD.
             04  WD-SCD       PIC  9(002).
             04  WD-TCD       PIC  X(004).
             04  WD-TCDD  REDEFINES WD-TCD.
               05  WD-TCD1    PIC  X(001).
               05  WD-TCD2    PIC  X(003).
             04  WD-DNOD      PIC  9(007).
             02  WD-HNGP.
               03  WD-HNEN    PIC  9(002).
               03  WD-HGET    PIC  9(002).
               03  WD-HPEY    PIC  9(002).
             02  WD-NNGP.
               03  WD-NNEN    PIC  9(002).
               03  WD-NGET    PIC  9(002).
               03  WD-NPEY    PIC  9(002).
             02  WD-MHCN      PIC  N(006).
             02  WD-SNA       PIC  X(020).
             02  WD-TNA       PIC  X(020).
             02  WD-PCN       PIC  N(001).
           COPY LSTAT.
      *
           COPY LITDNW-TAM.
           COPY LITDNN-TAM.
           COPY L-JCON.
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
           02  FILLER  PIC  N(009) VALUE
                "ìùàÍì`ï[áÇÅ@ñ‚çáÇπ".
           02  FILLER  PIC  X(036) VALUE
                "ÉèÅ[ÉNÉ}Éì = 0  ,  ÉiÉtÉR = 1    [ ]".
           02  FILLER  PIC  X(027) VALUE
                "é–ìX∫∞ƒﬁ  ì`ï[áÇ     î[ïiì˙".
           02  FILLER  PIC  N(004) VALUE
                "ÇeÇqÇnÇl".
           02  FILLER  PIC  X(001) VALUE "-".
           02  FILLER  PIC  N(004) VALUE
                "ÇsÇnÅ@Å@".
           02  FILLER  PIC  X(001) VALUE "-".
           02  FILLER  PIC  X(028) VALUE
                "ämîF (OK=1,NO=9) --->   ÿ¿∞›".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-SSCD  PIC  9(002).
             03  A-STCD  PIC  9(004).
             03  A-SDNO  PIC  9(007).
             03  A-SNHD  PIC  9(006).
           02  FILLER.
             03  A-ESCD  PIC  9(002).
             03  A-ETCD  PIC  9(004).
             03  A-EDNO  PIC  9(007).
             03  A-ENHD  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(007).
           02  D-DATA.
             03  FILLER.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  9(007).
               04  FILLER  PIC  X(020).
             03  FILLER.
               04  FILLER  PIC  X(004).
               04  FILLER  PIC  X(020).
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  X(001) VALUE "/".
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  X(001) VALUE "/".
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  9(002).
               04  FILLER  PIC  X(001) VALUE "/".
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  X(001) VALUE "/".
               04  FILLER  PIC Z9 .
               04  FILLER  PIC  N(006).
               04  FILLER  PIC  N(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
           COPY LSSEM.
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "127" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "32" "18" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "9" "21" "36" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "12" "33" "27" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "14" "21" "8" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "14" "35" "1" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "16" "21" "8" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "X" "16" "35" "1" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "23" "43" "28" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "9" "55" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "14" "0" "19" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSCD" "9" "14" "33" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSCD" BY REFERENCE W-SSCD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "14" "36" "4" "A-SSCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDNO" "9" "14" "43" "7" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDNO" BY REFERENCE W-SDNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNHD" "9" "14" "54" "6" "A-SDNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNHD" BY REFERENCE W-SNHD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "16" "0" "19" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ESCD" "9" "16" "33" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ESCD" BY REFERENCE W-ESCD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "16" "36" "4" "A-ESCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EDNO" "9" "16" "43" "7" "A-ETCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EDNO" BY REFERENCE W-EDNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ENHD" "9" "16" "54" "6" "A-EDNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ENHD" BY REFERENCE W-ENHD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "97" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "2" "1" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE W-NAME "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "0" "0" "83" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" " " "W-L1" "1" "29" " " "D-DATA" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-DATA" "9" "W-L1" "3" "2" " " "01D-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-DATA" BY REFERENCE WD-SCD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-DATA" "9" "W-L1" "11" "7" "0101D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-DATA" BY REFERENCE WD-DNOD "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-DATA" "X" "W-L1" "19" "20" "0201D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-DATA" BY REFERENCE WD-SNA "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02D-DATA" " " "W-L2" "1" "54" "01D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-DATA" "X" "W-L2" "3" "4" " " "02D-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-DATA" BY REFERENCE WD-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-DATA" "X" "W-L2" "23" "20" "0102D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-DATA" BY REFERENCE WD-TNA "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-DATA" "9" "W-L2" "44" "2" "0202D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-DATA" BY REFERENCE WD-HNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-DATA" "X" "W-L2" "46" "1" "0302D-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-DATA" "Z9" "W-L2" "47" "2" "0402D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502D-DATA" BY REFERENCE WD-HGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0602D-DATA" "X" "W-L2" "49" "1" "0502D-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0702D-DATA" "Z9" "W-L2" "50" "2" "0602D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702D-DATA" BY REFERENCE WD-HPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0802D-DATA" "9" "W-L2" "53" "2" "0702D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802D-DATA" BY REFERENCE WD-NNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0902D-DATA" "X" "W-L2" "55" "1" "0802D-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002D-DATA" "Z9" "W-L2" "56" "2" "0902D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002D-DATA" BY REFERENCE WD-NGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1102D-DATA" "X" "W-L2" "58" "1" "1002D-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1202D-DATA" "Z9" "W-L2" "59" "2" "1102D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1202D-DATA" BY REFERENCE WD-NPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1302D-DATA" "N" "W-L2" "62" "12" "1202D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1302D-DATA" BY REFERENCE WD-MHCN "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "1402D-DATA" "N" "W-L2" "76" "2" "1302D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1402D-DATA" BY REFERENCE WD-PCN "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN > 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SSCD "A-SSCD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-17.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-17
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-ESCD "A-ESCD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-17
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-22.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-22
           END-IF
      *
           IF  W-SSTC > W-ESTC
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-22
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO M-30
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-SNHD "A-SNHD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-ENHD "A-ENHD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-SNHD > W-ENHD
               GO TO M-35
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           IF  W-DMM = 9
               GO TO M-15
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-45
           END-IF
      *
           IF  W-SEN = 1
               MOVE "ÅyÉiÉtÉRÅzÅ@Å@" TO W-NAME
           ELSE
               MOVE "ÅyÉèÅ[ÉNÉ}ÉìÅz" TO W-NAME
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJK043" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE 0 TO W-DC.
           MOVE 3 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 4 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           IF  W-SEN = 1
               CALL "DB_F_Open" USING
                "INPUT" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST
                "1" "TDNN1-KEY" BY REFERENCE TDNN1-KEY
               GO TO M-55
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST
                "1" "TDNW1-KEY" BY REFERENCE TDNW1-KEY
           END-IF
      *
           MOVE ZERO TO TDNW1-KEY.
           MOVE W-SSCD TO TDNW1-SCD.
           MOVE W-STCD TO TDNW1-TCD.
           MOVE W-SDNO TO TDNW1-DNOD.
      *           START TDNWF KEY NOT < TDNW1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNWF_PNAME1 "TDNW1-KEY" " NOT < " TDNW1-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-50.
      *           READ TDNWF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF
           MOVE ZERO TO W-RSTC.
           MOVE TDNW1-SCD TO W-RSCD.
           MOVE TDNW1-TCD TO W-RTCD.
           IF  W-RSTC > W-ESTC
               GO TO M-75
           END-IF
           IF  TDNW1-DNOD < W-SDNO OR > W-EDNO
               GO TO M-50
           END-IF
           IF  TDNW1-DGN NOT = ZERO
               GO TO M-50
           END-IF
           IF  TDNW1-NNGP < W-SNHD OR > W-ENHD
               GO TO M-50
           END-IF
      *
           MOVE 3 TO JCON1-01.
           MOVE 6 TO JCON1-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
           END-IF
           INITIALIZE W-DSPD.
           MOVE TDNW1-SCD TO WD-SCD.
           MOVE TDNW1-TCD TO WD-TCD.
           MOVE TDNW1-DNOD TO WD-DNOD.
           MOVE TDNW1-HNGP TO WD-HNGP.
           MOVE TDNW1-NNGP TO WD-NNGP.
           MOVE JCON3-03 TO WD-MHCN.
           MOVE TDNW1-SNA TO WD-SNA.
           MOVE TDNW1-TNA TO WD-TNA.
           IF  TDNW1-PC = 0
               MOVE "Å@" TO WD-PCN
           ELSE
               IF  TDNW1-PC = 1
                   MOVE "ñ¢" TO WD-PCN
               ELSE
                   IF  TDNW1-PC = 9
                       MOVE "çœ" TO WD-PCN
                   END-IF
               END-IF
           END-IF
           GO TO M-65.
       M-55.
           MOVE ZERO TO TDNN1-KEY.
           MOVE W-SSCD TO TDNN1-SCD.
           MOVE W-STCD TO TDNN1-TCD.
           MOVE W-SDNO TO TDNN1-DNOD.
      *           START TDNNF KEY NOT < TDNN1-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNNF_PNAME1 "TDNN1-KEY" " NOT < " TDNN1-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-60.
      *           READ TDNNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF
           MOVE ZERO TO W-RSTC.
           MOVE TDNN1-SCD TO W-RSCD.
           MOVE TDNN1-TCD TO W-RTCD.
           IF  W-RSTC > W-ESTC
               GO TO M-75
           END-IF
           IF  TDNN1-DNOD < W-SDNO OR > W-EDNO
               GO TO M-60
           END-IF
           IF  TDNN1-DGN NOT = ZERO
               GO TO M-60
           END-IF
           IF  TDNN1-NNGP < W-SNHD OR > W-ENHD
               GO TO M-60
           END-IF
      *
           MOVE 3 TO JCON1-01.
           MOVE 6 TO JCON1-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
           END-IF
           INITIALIZE W-DSPD.
           MOVE TDNN1-SCD TO WD-SCD.
           MOVE TDNN1-TCD TO WD-TCD2.
           MOVE TDNN1-DNOD TO WD-DNOD.
           MOVE TDNN1-HNGP TO WD-HNGP.
           MOVE TDNN1-NNGP TO WD-NNGP.
           MOVE JCON3-03 TO WD-MHCN.
           MOVE TDNN1-SNA TO WD-SNA.
           MOVE TDNN1-TNA TO WD-TNA.
           IF  TDNN1-PC = 0
               MOVE "Å@" TO WD-PCN
           ELSE
               IF  TDNN1-PC = 1
                   MOVE "ñ¢" TO WD-PCN
               ELSE
                   IF  TDNN1-PC = 9
                       MOVE "çœ" TO WD-PCN
                   END-IF
               END-IF
           END-IF.
       M-65.
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF.
       M-70.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 > 21
               GO TO M-80
           END-IF
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
           IF  W-SEN = 1
               GO TO M-60
           ELSE
               GO TO M-50
           END-IF.
       M-75.
           IF W-DC = 0
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE 9 TO W-DC.
       M-80.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-80
           END-IF
           IF  W-DMM = 9
               GO TO M-90
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-80
           END-IF
      *
           IF W-DC NOT = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SJK043" RETURNING RESU
               CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU
               MOVE 3 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               MOVE 4 TO W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               GO TO M-70
           END-IF.
       M-90.
           IF W-SEN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
