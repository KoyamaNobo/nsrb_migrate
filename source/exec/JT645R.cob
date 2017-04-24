       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT645R.
      ******************************************************************
      *    <<REMARKS>>                                                 *
      *    FUNCTION.......  入出庫明細問合せ                           *
      *    AUTHOR.........  I.N                                        *
      *    COMPILE MODE...  NORMAL                                     *
      *    SCREEN.........  SJ645R                                     *
      *    RELEASE DATE...  90/10/22         (REV.001)                 *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT             PIC  X(02).
       77  JT-W170K1ID          PIC  X(12).
       77  JT-W170K3ID          PIC  X(12).
       01  OKC                  PIC  9(01).
       01  NXT-SW               PIC 9(01).
       01  W-DC                 PIC 9(01).
       01  STN-NO.
           02  STN-NO-01        PIC X(03).
           02  STN-NO-02        PIC X(03).
       01  WF1-ID.
           02  WORK1-ID-01      PIC X(09) VALUE "JT-W170K1".
           02  WORK1-ID-02      PIC X(03).
       01  WF2-ID.
           02  WORK2-ID-01      PIC X(09) VALUE "JT-W170K3".
           02  WORK2-ID-02      PIC X(03).
       01  NXT-KEY.
           03  NXT-01           PIC 9(06)  COMP-3.
           03  NXT-02           PIC 9(08)  COMP-3.
           03  NXT-03           PIC 9(02).
           03  NXT-04           PIC 9(06)  COMP-3.
           03  NXT-05           PIC 9(01).
       01  WG-AREA.
           02  WG-HINCD         PIC  9(06).
           02  WG-HINNM         PIC  N(20).
           02  WG-FHIZ          PIC  9(08).
           02  WG-FHIZD  REDEFINES WG-FHIZ.
               03  WG-FNEN      PIC  9(04).
               03  WG-FNENL  REDEFINES  WG-FNEN.
                   04  WG-FNEN1 PIC  9(02).
                   04  WG-FNEN2 PIC  9(02).
               03  WG-FGET      PIC  9(02).
               03  WG-FPEY      PIC  9(02).
           02  WG-FHIZL  REDEFINES WG-FHIZ.
               03  F            PIC  9(02).
               03  WG-FHIZS     PIC  9(06).
           02  WG-THIZ          PIC  9(08).
           02  WG-THIZD  REDEFINES WG-THIZ.
               03  WG-TNEN      PIC  9(04).
               03  WG-TNENL  REDEFINES  WG-TNEN.
                   04  WG-TNEN1 PIC  9(02).
                   04  WG-TNEN2 PIC  9(02).
               03  WG-TGET      PIC  9(02).
               03  WG-TPEY      PIC  9(02).
           02  WG-THIZL  REDEFINES WG-THIZ.
               03  F            PIC  9(02).
               03  WG-THIZS     PIC  9(06).
           02  WG-TBL.
               03  WG-TBL12          OCCURS  12.
                   05  WG-YYMM.
                       07  WG-MM     PIC  9(02).
                       07  WG-DD     PIC  9(02).
                   05  WG-KBN        PIC  9(02).
                   05  WG-KBNNM      PIC  N(02).
                   05  WG-TCMCD      PIC  9(03).
                   05  WG-TCMNM      PIC  N(16).
                   05  WG-SOKSU      PIC S9(06).
                   05  WG-HAITA      PIC  N(05).
                   05  WG-TEKIYO     PIC  N(07).
                   05  WG-TBL4       OCCURS  4.
                       07  WG-SUR    PIC S9(06)      OCCURS  10.
                   05  WG-GOK        PIC S9(07).
                   05  WG-TOKCD      PIC  9(04).
                   05  WG-JYUNO      PIC  9(06).
                   05  WG-DENNO      PIC  9(06).
                   05  WG-OKRNO      PIC  9(06).
                   05  WG-KURA       PIC  9(01).
                   05  WG-UNSO       PIC  9(01).
                   05  WG-SW         PIC  9(01).
               03  WG-TOKNM          PIC  N(18).
      *
           02  WG-ACT2          PIC  9(02).
           02  WG-DC            PIC  9(01).
           02  WG-DAI           PIC  N(04).
           02  WG-ZAI-TBL.
               03  WG-TBL44      OCCURS  4.
                   04  WG-ZAI    PIC S9(06)      OCCURS  10.
               03  WG-ZGOK   PIC S9(07).
       01  WORK-AREA.
           02  LIN              PIC  9(02).
           02  COLU             PIC  9(02).
           02  I                PIC  9(02).
           02  N                PIC  9(02).
           02  S                PIC  9(02).
           02  WK-HIZUKE        PIC  9(08).
           02  WK-HIZUKER   REDEFINES     WK-HIZUKE.
               03  F            PIC  9(02).
               03  WK-YY        PIC  9(02).
               03  WK-MM        PIC  9(02).
               03  WK-DD        PIC  9(02).
           02  OLD-KEY.
               03  OLD-02       PIC  9(08).
               03  OLD-03       PIC  9(02).
               03  OLD-04       PIC  9(06).
               03  OLD-05       PIC  9(01).
           02  WK-HINCD         PIC  9(06).
           02  WK-CODE          PIC  9(07).
           02  WK-CODER  REDEFINES   WK-CODE.
               03  F            PIC  X(01).
               03  WK-CD1       PIC  9(06).
           02  CNT1             PIC  9(02).
           02  CNT2             PIC  9(01).
           02  W-TEKIYO         PIC  N(07).
           02  W-TEKIYOD REDEFINES W-TEKIYO.
               03  W-TEKID  OCCURS   7.
                   04  W-TEKI   PIC  N(01).
           02  W-TEWD           PIC  N(12).
           02  W-TEAD    REDEFINES W-TEWD.
               03  W-TEA    OCCURS  12.
                   04  W-TE     PIC  N(01).
           COPY  LWMSG.
      *
           COPY  LIBFDD.
           COPY  L-JNSR.
           COPY  LJT170.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LNJZAI.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  ACT-CLEAR.
           02  CLR-GMN  PIC  X(12) VALUE "CLEAR SCREEN".
       01  HIM-ERR.
           02  FILLER   PIC  X(20) VALUE  "品名マスター　未登録".
       01  JNSR-ERR.
           02  FILLER   PIC  X(16) VALUE  "明細データ　無し".
       01  JZA2-ERR.
           02  FILLER   PIC  X(16) VALUE  "在庫データ　無し".
       01  NXT-ERR.
           02  FILLER   PIC  X(20) VALUE  "ＮＥＸＴデータ　無し".
       01  CLR-ERR.
           02  FILLER   PIC  X(40)  VALUE  "  ".
       01  ACP-AREA.
           02  ACP-HINCD   PIC  9(06).
           02  ACP-FHIZ    PIC  9(06).
           02  ACP-THIZ    PIC  9(06).
           02  ACP-ACT2    PIC  9(02).
           02  ACP-OKC     PIC  9(01).
       01  DSP-AREA1.
           02  DSP-HINNM   PIC  N(20).
           02  DSP-TBL12.
               03  DSP-MM     PIC  Z(02).
               03  DSP-S      PIC  X(01)  VALUE  "/".
               03  DSP-DD     PIC  Z(02).
               03  DSP-KBNNM  PIC  N(02).
               03  DSP-TCMNM  PIC  N(16).
               03  DSP-SOKSU  PIC -----9 .
               03  DSP-HAITA  PIC  N(05).
               03  DSP-TEKIYO PIC  N(07).
       01  DSP-AREA2.
           02  DSP-TBL4.
               03  DSP-SUR  PIC ------ .
               03  DSP-ZAI  PIC ------ .
           02  DSP-GOK     PIC ------- .
           02  DSP-ZGOK    PIC ------- .
           02  DSP-DAI     PIC  N(04).
           02  DSP-TOKNM   PIC  N(18).
           02  DSP-JYUNO   PIC  9(06).
           02  DSP-DENNO   PIC  9(06).
           02  DSP-OKRNO   PIC  9(06).
           02  DSP-KURA.
               03  FILLER  PIC  X(06)
                   VALUE  "(倉庫 ".
               03  FILLER  PIC  9(01).
               03  FILLER  PIC  X(01)
                   VALUE  ")".
           02  DSP-UNSO.
               03  FILLER  PIC  X(06)
                   VALUE  "(運送 ".
               03  FILLER  PIC  9(01).
               03  FILLER  PIC  X(01)
                   VALUE  ")".
       01  CLR-AREA1.
           02  CLR-01  PIC X(06)  VALUE  " ".
           02  CLR-01A PIC X(07)  VALUE  " ".
           02  CLR-02  PIC X(40)  VALUE  " ".
           02  CLR-03  PIC X(06)  VALUE  " ".
           02  CLR-04  PIC X(06)  VALUE  " ".
           02  CLR-TBL12.
               03  FILLER  PIC  X(05)  VALUE  " ".
               03  FILLER  PIC  X(04)  VALUE  " ".
               03  FILLER  PIC  X(40)  VALUE  " ".
               03  FILLER  PIC  X(06)  VALUE  " ".
               03  FILLER  PIC  X(10)  VALUE  " ".
               03  FILLER  PIC  X(14)  VALUE  " ".
       01  CLR-AREA2.
           02  CLR-TBL4.
               03  FILLER  PIC  X(67)  VALUE  " ".
           02  CLR-05  PIC X(02)  VALUE  " ".
           02  CLR-11  PIC X(08)  VALUE  " ".
           02  CLR-12  PIC X(08)  VALUE  " ".
           02  CLR-06  PIC X(08)  VALUE  " ".
           02  CLR-07  PIC X(36)  VALUE  " ".
           02  CLR-13  PIC X(06)  VALUE  " ".
           02  CLR-08  PIC X(06)  VALUE  " ".
           02  CLR-09  PIC X(06)  VALUE  " ".
           02  CLR-10  PIC X(01)  VALUE  " ".
      ************
           COPY  LSMSG.
           COPY  LIBSCR.
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACT-CLEAR
       CALL "SD_Init" USING
           "ACT-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-GMN" "X" "1" "0" "12" " " "ACT-CLEAR" RETURNING RESU.
      *HIM-ERR
       CALL "SD_Init" USING 
            "HIM-ERR" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01HIM-ERR" "X" "24" "1" "20" " " "HIM-ERR" RETURNING RESU.
      *JNSR-ERR
       CALL "SD_Init" USING 
            "JNSR-ERR" " " "0" "0" "16" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01JNSR-ERR" "X" "24" "1" "16" " " "JNSR-ERR" RETURNING RESU.
      *JZA2-ERR
       CALL "SD_Init" USING 
            "JZA2-ERR" " " "0" "0" "16" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01JZA2-ERR" "X" "24" "1" "16" " " "JZA2-ERR" RETURNING RESU.
      *NXT-ERR
       CALL "SD_Init" USING 
            "NXT-ERR" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01NXT-ERR" "X" "24" "1" "20" " " "NXT-ERR" RETURNING RESU.
      *CLR-ERR
       CALL "SD_Init" USING 
            "CLR-ERR" " " "0" "0" "40" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-ERR" "X" "24" "1" "40" " " "CLR-ERR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "21" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-HINCD" "9" "1" "14" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-HINCD" BY REFERENCE WG-HINCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FHIZ" "9" "1" "68" "6" "ACP-HINCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FHIZ" BY REFERENCE WG-FHIZS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-THIZ" "9" "1" "75" "6" "ACP-FHIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-THIZ" BY REFERENCE WG-THIZS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT2" "9" "19" "2" "2" "ACP-THIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT2" BY REFERENCE WG-ACT2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "73" "1" "ACP-ACT2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "111" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-HINNM" "N" "1" "21" "40" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HINNM" BY REFERENCE WG-HINNM "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "DSP-TBL12" " " "LIN" "0" "71" "DSP-HINNM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MM" "Z" "LIN" "4" "2" " " "DSP-TBL12" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MM" BY REFERENCE WG-MM(1) "2" "1" BY REFERENCE I 347
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-S" "X" "LIN" "6" "1" "DSP-MM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DD" "Z" "LIN" "7" "2" "DSP-S" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DD" BY REFERENCE WG-DD(1) "2" "1" BY REFERENCE I 347 
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KBNNM" "N" "LIN" "10" "4" "DSP-DD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KBNNM" BY REFERENCE WG-KBNNM(1) "4" "1" BY REFERENCE
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TCMNM" "N" "LIN" "15" "32" "DSP-KBNNM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TCMNM" BY REFERENCE WG-TCMNM(1) "32" "1" BY REFERENCE
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SOKSU" "-----9" "LIN" "49" "6" "DSP-TCMNM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SOKSU" BY REFERENCE WG-SOKSU(1) "6" "1" BY REFERENCE
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HAITA" "N" "LIN" "56" "10" "DSP-SOKSU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HAITA" BY REFERENCE WG-HAITA(1) "10" "1" BY REFERENCE
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TEKIYO" "N" "LIN" "67" "14" "DSP-HAITA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TEKIYO" BY REFERENCE WG-TEKIYO(1) "14" "1" BY REFERENCE
            I 347 RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING 
            "DSP-AREA2" " " "0" "0" "104" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-TBL4" " " "LIN" "0" "12" " " "DSP-AREA2" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SUR" "------" "LIN" "COLU" "6" " " "DSP-TBL4"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SUR" BY REFERENCE WG-SUR(1,1,1) "6" "3" BY REFERENCE 
            I 347 BY REFERENCE N 60 BY REFERENCE S 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ZAI" "------" "LIN" "COLU" "6" "DSP-SUR" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ZAI" BY REFERENCE WG-ZAI(1,1) "6" "2" BY REFERENCE N 60
            BY REFERENCE S 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-GOK" "-------" "22" "74" "7" "DSP-TBL4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-GOK" BY REFERENCE WG-GOK(1) "7" "1" BY REFERENCE I 347
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ZGOK" "-------" "22" "74" "7" "DSP-GOK" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ZGOK" BY REFERENCE WG-ZGOK "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DAI" "N" "23" "2" "8" "DSP-ZGOK" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DAI" BY REFERENCE WG-DAI "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TOKNM" "N" "23" "14" "36" "DSP-DAI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TOKNM" BY REFERENCE WG-TOKNM "36" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-JYUNO" "9" "23" "54" "6" "DSP-TOKNM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-JYUNO" BY REFERENCE WG-JYUNO(1) "6" "1" BY REFERENCE 
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-DENNO" "9" "23" "64" "6" "DSP-JYUNO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-DENNO" BY REFERENCE WG-DENNO(1) "6" "1" BY REFERENCE 
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-OKRNO" "9" "23" "74" "6" "DSP-DENNO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-OKRNO" BY REFERENCE WG-OKRNO(1) "6" "1" BY REFERENCE 
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KURA" " " "21" "0" "8" "DSP-OKRNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-KURA" "X" "21" "1" "6" " " "DSP-KURA" RETURNING RESU.
       CALL "SD_Init" USING 
          "02DSP-KURA" "9" "21" "7" "1" "01DSP-KURA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-KURA" BY REFERENCE WG-KURA(1) "1" "1" BY REFERENCE 
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
          "03DSP-KURA" "X" "21" "8" "1" "02DSP-KURA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-UNSO" " " "22" "0" "8" "DSP-KURA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-UNSO" "X" "22" "1" "6" " " "DSP-UNSO" RETURNING RESU.
       CALL "SD_Init" USING 
          "02DSP-UNSO" "9" "22" "7" "1" "01DSP-UNSO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-UNSO" BY REFERENCE WG-UNSO(1) "1" "1" BY REFERENCE 
            I 347 RETURNING RESU.
       CALL "SD_Init" USING 
          "03DSP-UNSO" "X" "22" "8" "1" "02DSP-UNSO" " " RETURNING RESU.
      *CLR-AREA1
       CALL "SD_Init" USING 
            "CLR-AREA1" " " "0" "0" "144" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-01" "X" "1" "14" "6" " " "CLR-AREA1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-01A" "X" "1" "14" "7" "CLR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-02" "X" "1" "21" "40" "CLR-01A" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-03" "X" "1" "68" "6" "CLR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-04" "X" "1" "75" "6" "CLR-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TBL12" " " "LIN" "0" "79" "CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-TBL12" "X" "LIN" "4" "5" " " "CLR-TBL12"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-TBL12" "X" "LIN" "10" "4" "01CLR-TBL12" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-TBL12" "X" "LIN" "15" "40" "02CLR-TBL12" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-TBL12" "X" "LIN" "49" "6" "03CLR-TBL12" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-TBL12" "X" "LIN" "56" "10" "04CLR-TBL12" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLR-TBL12" "X" "LIN" "67" "14" "05CLR-TBL12" " "
            RETURNING RESU.
      *CLR-AREA2
       CALL "SD_Init" USING 
            "CLR-AREA2" " " "0" "0" "148" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "CLR-TBL4" " " "LIN" "0" "67" " " "CLR-AREA2" RETURNING RESU.
       CALL "SD_Init" USING 
         "01CLR-TBL4" "X" "LIN" "14" "67" " " "CLR-TBL4" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-05" "X" "19" "2" "2" "CLR-TBL4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-11" "X" "21" "1" "8" "CLR-05" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-12" "X" "22" "1" "8" "CLR-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-06" "X" "23" "2" "8" "CLR-12" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-07" "X" "23" "14" "36" "CLR-06" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-13" "X" "23" "54" "6" "CLR-07" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-08" "X" "23" "64" "6" "CLR-13" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-09" "X" "23" "74" "6" "CLR-08" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-10" "X" "24" "73" "1" "CLR-09" " "  RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ******************************************************************
      *                    ＭＡＩＮ－ＲＴＮ                            *
      ******************************************************************
       MAIN-RTN.
           PERFORM   OPEN-RTN     THRU         OPEN-EX.
           PERFORM   INP-RTN      THRU         INP-EX.
           PERFORM   END-RTN      THRU         END-EX.
           CALL "DB_Close".
           STOP      RUN.
      ******************************************************************
      *                    ＯＰＥＮ－ＲＴＮ                            *
      ******************************************************************
       OPEN-RTN.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ645R" RETURNING RESU.
           CALL      "CBLSTNNO"   USING   STN-NO USER_ID.
           MOVE      STN-NO-02    TO      WORK1-ID-02  WORK2-ID-02.
           MOVE      WF1-ID       TO      JT-W170K1ID.
           MOVE      JT-W170K1ID  TO      JT-W170_PNAME1.
           MOVE      WF2-ID       TO      JT-W170K3ID.
           MOVE      JT-W170K3ID  TO      JT-W170_PNAME2.
           CALL "DB_F_Open" USING
            "INPUT" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JT-W170_PNAME1 "SHARED" BY REFERENCE JT-W170_IDLST
            "2" "JTW-KEY1" BY REFERENCE JTW-KEY1 "JTW-KEY3" BY REFERENCE
            JTW-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           INITIALIZE      WG-AREA  NXT-KEY.
           COPY  LIBCPR.
       OPEN-EX.
           EXIT.
      ******************************************************************
      *    INP-RTN         画面入力処理　　　                          *
      ******************************************************************
       INP-RTN.
      *<<  ＭＡＴ  >>*
           MOVE      WG-HINCD    TO    WK-HINCD.
       INP-010.
      *<<  品名コード  >>*
           CALL "SD_Output" USING "CLR-01A" CLR-01A "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACP-HINCD "ACP-HINCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =   "P9" 
               GO  TO  INP-EX
           END-IF
           IF  ESTAT  NOT =   "01"  AND  "06"
               GO  TO  INP-010
           END-IF.
       INP-020.
           MOVE      WG-HINCD  TO   HI-MHCD HI-HCD.
      *           READ      HI2-M     UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "HIM-ERR" HIM-ERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  INP-RTN
           END-IF
           MOVE      HI-NAME   TO   WG-HINNM.
           CALL "SD_Output" USING
            "DSP-HINNM" DSP-HINNM "p" RETURNING RESU.
           IF  WG-HINCD   >    999899
               GO  TO  INP-010
           END-IF.
       INP-030.
      *<<  日付ＦＲＯＭ  >>*
           CALL "SD_Accept" USING BY REFERENCE ACP-FHIZ "ACP-FHIZ"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =   "09"
               GO  TO  INP-RTN
           END-IF
           IF  ESTAT  NOT =  "01" AND "04" AND "06"
               GO  TO  INP-030
           END-IF
           IF  ESTAT      =   "04"
               ACCEPT  WG-FHIZS FROM DATE
               CALL "SD_Output" USING
                "ACP-FHIZ" ACP-FHIZ "p" RETURNING RESU
           END-IF
           IF  WG-FHIZS   =   ZERO
               CALL "SD_Output" USING
                "ACP-FHIZ" ACP-FHIZ "p" RETURNING RESU
               MOVE  ZERO       TO  WG-FNEN1
               GO  TO  INP-040
           END-IF
           IF (WG-FGET  <  1  OR  >  12)  OR
              (WG-FPEY  <  1  OR  >  31)
               GO  TO  INP-030
           END-IF
           MOVE  ZERO       TO  WG-FNEN1.
           IF  WG-FNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO WG-FNEN
           END-IF
           IF  WG-FNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO WG-FNEN
           END-IF.
       INP-040.
      *<<  日付ＴＯ  >>*
           CALL "SD_Accept" USING BY REFERENCE ACP-THIZ "ACP-THIZ"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      =   "09"
               GO  TO  INP-030
           END-IF
           IF  ESTAT  NOT =  "01" AND "04" AND "06"
               GO  TO  INP-040
           END-IF
           IF  ESTAT      =   "04"
               ACCEPT  WG-THIZS FROM DATE
               CALL "SD_Output" USING
                "ACP-THIZ" ACP-THIZ "p" RETURNING RESU
           END-IF
           IF  WG-THIZS   =   ZERO  OR  999999
               MOVE  99999999 TO  WG-THIZ
               CALL "SD_Output" USING
                "ACP-THIZ" ACP-THIZ "p" RETURNING RESU
               GO  TO  INP-050
           END-IF
           IF (WG-TGET  <  1  OR  >  12)  OR
              (WG-TPEY  <  1  OR  >  31)
               GO  TO  INP-040
           END-IF
           MOVE  ZERO       TO  WG-TNEN1.
           IF  WG-TNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO WG-TNEN
           END-IF
           IF  WG-TNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO WG-TNEN
           END-IF
           IF  WG-FHIZ    >   WG-THIZ
               GO  TO  INP-040
           END-IF
           PERFORM   CLR-010    THRU     CLR-EX.
       INP-050.
           MOVE      0          TO  I  N  S.
           MOVE      0          TO  W-DC.
           MOVE      WG-HINCD   TO  JNSR-01.
           MOVE      WG-FHIZ    TO  JNSR-02.
           MOVE      ZERO       TO  JNSR-03  JNSR-04  JNSR-05.
       INP-055.
      *           START     JNSR       KEY     NOT <      JNSR-KEY1 INVALID
      *///////////////
           CALL "DB_Start" USING
            JNSR_PNAME1 "JNSR-KEY1" " NOT < " JNSR-KEY1 RETURNING RET.
           IF  RET = 1
               GO  TO  INP-062
           END-IF.
       INP-060.
      *           READ      JNSR       NEXT  UNLOCK  AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNSR_PNAME1 BY REFERENCE JNSR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  INP-062
           END-IF
           IF  JNSR-10        = 2
               GO  TO  INP-060
           END-IF
           IF  JNSR-01        > 999899
               GO  TO  INP-060
           END-IF
           IF  JNSR-01     NOT = WG-HINCD
               GO  TO  INP-062
           END-IF
           IF  JNSR-02     >     WG-THIZ
               GO  TO  INP-062
           END-IF
           IF  I          =   0
               MOVE  0          TO  NXT-SW
               MOVE  JNSR-02    TO  OLD-02
               MOVE  JNSR-03    TO  OLD-03
               MOVE  JNSR-04    TO  OLD-04
               MOVE  JNSR-05    TO  OLD-05
           END-IF
           PERFORM   MOV1-RTN    THRU  MOV1-EX.
           IF  I         >   12
               MOVE  1          TO  NXT-SW
               MOVE  JNSR-KEY1  TO  NXT-KEY
               GO  TO  INP-070
           END-IF
           MOVE      JNSR-02    TO    OLD-02.
           MOVE      JNSR-03    TO    OLD-03.
           MOVE      JNSR-04    TO    OLD-04.
           MOVE      JNSR-05    TO    OLD-05.
           GO  TO  INP-060.
       INP-062.
           MOVE      1          TO  W-DC.
           MOVE      WG-HINCD   TO  JTW-01.
           MOVE      WG-FHIZ    TO  JTW-02.
           MOVE      ZERO       TO  JTW-03  JTW-04  JTW-05.
       INP-064.
      *           START     JT-W170    KEY     NOT <      JTW-KEY1 INVALID
      *///////////////
           CALL "DB_Start" USING
            JT-W170_PNAME1 "JTW-KEY1" " NOT < " JTW-KEY1 RETURNING RET.
           IF  RET = 1
               GO  TO  INP-070
           END-IF.
       INP-067.
      *           READ      JT-W170    NEXT  UNLOCK  AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-W170_PNAME1 BY REFERENCE JTW-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  INP-070
           END-IF
           IF  JTW-10         = 2
               GO  TO  INP-067
           END-IF
           IF  JTW-01     NOT = WG-HINCD
               GO  TO  INP-070
           END-IF
           IF  JTW-02     >     WG-THIZ
               GO  TO  INP-070
           END-IF
           IF  I          =   0
               MOVE  0          TO  NXT-SW
               MOVE  JTW-02     TO  OLD-02
               MOVE  JTW-03     TO  OLD-03
               MOVE  JTW-04     TO  OLD-04
               MOVE  JTW-05     TO  OLD-05
           END-IF
           PERFORM   MOV2-RTN    THRU  MOV2-EX.
           IF  I         >   12
               MOVE  1          TO  NXT-SW
               MOVE  JTW-KEY1   TO  NXT-KEY
               GO  TO  INP-070
           END-IF
           MOVE      JTW-02     TO    OLD-02.
           MOVE      JTW-03     TO    OLD-03.
           MOVE      JTW-04     TO    OLD-04.
           MOVE      JTW-05     TO    OLD-05.
           GO  TO  INP-067.
       INP-070.
           PERFORM   DSP1-RTN   THRU  DSP1-EX.
       INP-999.
      *<<  確認  >>*
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "CLR-ERR" CLR-ERR "p" RETURNING RESU.
           IF  ESTAT      =   "09"
               PERFORM   SHOSAI-RTN    THRU  SHOSAI-EX
           ELSE
               GO  TO    INP-110
           END-IF
           IF  ESTAT      =   "09"
               GO  TO  INP-999
           END-IF.
       INP-110.
           IF  ESTAT  NOT =   "01" AND "06"
               GO  TO  INP-999
           END-IF
           IF  OKC        =   9
               PERFORM  CLR-RTN  THRU  CLR-EX
               GO  TO  INP-RTN
           END-IF
           IF  OKC        =   1
               IF  NXT-SW       NOT =   1
                   CALL "SD_Output" USING
                    "NXT-ERR" NXT-ERR "p" RETURNING RESU
                   GO  TO  INP-999
               ELSE
                   PERFORM  CLR-010  THRU  CLR-EX
                   MOVE  NXT-KEY  TO  JNSR-KEY1
                   MOVE  0      TO  I N S
                   IF  W-DC           =   0
                       GO  TO  INP-055
                   ELSE
                       GO  TO  INP-064
                   END-IF
               END-IF
           END-IF
           GO  TO  INP-999.
       INP-EX.
           EXIT.
      ******************************************************************
      *    SHOSAI-RTN    詳細表示                                      *
      ******************************************************************
       SHOSAI-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT2 "ACP-ACT2"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "CLR-ERR" CLR-ERR "p" RETURNING RESU.
           IF  ESTAT      =   "09"
               GO  TO  SHOSAI-EX
           END-IF
           IF  ESTAT  NOT =   "01" AND "06"
               GO  TO  SHOSAI-RTN
           END-IF
           IF    ( WG-ACT2    NOT <   1   AND  NOT >  12 )
               OR   ( WG-ACT2      =  91  OR  92  OR  99 )
               CONTINUE
           ELSE
               GO  TO  SHOSAI-RTN
           END-IF
           MOVE      WG-ACT2        TO  I.
           IF  WG-ACT2         <  91
               IF  WG-SW(I)    =  0
                   GO  TO  SHOSAI-RTN
               END-IF
           END-IF
           IF  WG-ACT2     =  99
               PERFORM  ZAI-RTN    THRU  ZAI-EX
           ELSE
               IF  WG-ACT2     >  90
                   PERFORM  NSK-RTN    THRU  NSK-EX
               ELSE
                   PERFORM  DSP2-RTN   THRU  DSP2-EX
               END-IF
           END-IF
           GO  TO  SHOSAI-RTN.
       SHOSAI-EX.
           EXIT.
      ******************************************************************
      *    MOV1-RTN   １～１２行　項目セット処理    （入出庫累積Ｆ）
      ******************************************************************
       MOV1-RTN.
           IF  I         =   0
               MOVE  1          TO  I
               GO  TO  MOV1-010
           END-IF
           IF  JNSR-02    NOT =  OLD-02    OR
                 JNSR-03     NOT =  OLD-03   OR
                   JNSR-04     NOT =  OLD-04
               ADD  1       TO  I
           ELSE
               GO  TO  MOV1-020
           END-IF.
       MOV1-010.
           IF  I         >   12
               GO  TO  MOV1-EX
           END-IF
           MOVE  1             TO  WG-SW(I).
           MOVE  JNSR-02        TO  WK-HIZUKE.
           MOVE  WK-MM         TO  WG-MM(I).
           MOVE  WK-DD         TO  WG-DD(I).
           MOVE  JNSR-03        TO  WG-KBN(I).
           IF  JNSR-03  =  10 OR 11
               MOVE  "入庫"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  12
               MOVE  "格外"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  20
               MOVE  "出庫"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  21
               MOVE  "移動"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  22
               MOVE  "出荷"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  23
               MOVE  "返品"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  24
               MOVE  "不良"  TO  WG-KBNNM(I)
           END-IF
           IF  JNSR-03  =  25
               MOVE  "訂正"  TO  WG-KBNNM(I)
           END-IF
           MOVE  JNSR-111       TO  TC-TCD  WG-TOKCD(I).
           MOVE  JNSR-112       TO  TC-CCD  WG-TCMCD(I).
      *           READ  TC-M     UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE   TO  TC-NAME
           END-IF
           MOVE  TC-NAME        TO  WG-TCMNM(I).
           MOVE  JNSR-23        TO  WG-HAITA(I).
           MOVE  SPACE          TO  W-TEKIYO  W-TEWD.
           MOVE  JNSR-24        TO  W-TEWD.
           PERFORM  MOV9-RTN    THRU  MOV9-EX.
           MOVE  W-TEKIYO       TO  WG-TEKIYO(I).
           MOVE  JNSR-151       TO  WG-JYUNO(I).
           MOVE  JNSR-04        TO  WG-DENNO(I).
           MOVE  JNSR-12        TO  WG-OKRNO(I).
           MOVE  JNSR-06        TO  WG-KURA(I).
           MOVE  JNSR-14        TO  WG-UNSO(I).
       MOV1-020.
           MOVE  JNSR-07        TO  N.
           PERFORM  SUR-SET-RTN    THRU  SUR-SET-EX.
       MOV1-EX.
           EXIT.
      ******************************************************************
      *    MOV2-RTN   １～１２行　項目セット処理    （入出庫ワークＦ）
      ******************************************************************
       MOV2-RTN.
           IF  I         =   0
               MOVE  1          TO  I
               GO  TO  MOV2-010
           END-IF
           IF  JTW-02    NOT =  OLD-02    OR
                 JTW-03     NOT =  OLD-03   OR
                   JTW-04     NOT =  OLD-04
               ADD  1       TO  I
           ELSE
               GO  TO  MOV2-020
           END-IF.
       MOV2-010.
           IF  I         >   12
               GO  TO  MOV2-EX
           END-IF
           MOVE  1             TO  WG-SW(I).
           MOVE  ZERO          TO  WK-HIZUKE.
           MOVE  WK-MM         TO  WG-MM(I).
           MOVE  WK-DD         TO  WG-DD(I).
           MOVE  JTW-03        TO  WG-KBN(I).
           IF  JTW-03   =  10
               MOVE  "入庫"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  12
               MOVE  "格外"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  11
               MOVE  "移入"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  20
               MOVE  "出庫"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  21
               MOVE  "移出"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  22
               MOVE  "出荷"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  23
               MOVE  "返品"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  24
               MOVE  "不良"  TO  WG-KBNNM(I)
           END-IF
           IF  JTW-03   =  25
               MOVE  "訂正"  TO  WG-KBNNM(I)
           END-IF
           MOVE  JTW-111       TO  TC-TCD  WG-TOKCD(I).
           MOVE  JTW-112       TO  TC-CCD  WG-TCMCD(I).
      *           READ  TC-M     UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE   TO  TC-NAME
           END-IF
           MOVE  TC-NAME       TO  WG-TCMNM(I).
           MOVE  JTW-23        TO  WG-HAITA(I).
           MOVE  SPACE         TO  W-TEKIYO  W-TEWD.
           MOVE  JTW-24        TO  W-TEWD.
           PERFORM  MOV9-RTN   THRU  MOV9-EX.
           MOVE  W-TEKIYO      TO  WG-TEKIYO(I).
           MOVE  JTW-151       TO  WG-JYUNO(I).
           MOVE  JTW-04        TO  WG-DENNO(I).
           MOVE  JTW-12        TO  WG-OKRNO(I).
           MOVE  JTW-06        TO  WG-KURA(I).
           MOVE  JTW-14        TO  WG-UNSO(I).
       MOV2-020.
           MOVE  JTW-07        TO  N.
           PERFORM  SUR-SET-RTN    THRU  SUR-SET-EX.
       MOV2-EX.
           EXIT.
      ******************************************************************
      *    MOV9-RTN   １～１２行　摘要項目セット処理（入出庫ワークＦ）
      ******************************************************************
       MOV9-RTN.
           MOVE  ZERO           TO  CNT1  CNT2.
       MOV9-010.
           ADD   1              TO  CNT1  CNT2.
           IF  CNT1           =  1
               IF  W-TE(CNT1)     =  "№"
                   SUBTRACT   1      FROM   CNT2
                   GO  TO  MOV9-010
               END-IF
           END-IF
           IF (CNT1           =  13)  OR  (CNT2            =  8)
               GO  TO  MOV9-EX
           END-IF
           MOVE  W-TE(CNT1)     TO  W-TEKI(CNT2).
           IF  W-TE(CNT1) NOT =  SPACE
               GO  TO  MOV9-010
           END-IF.
       MOV9-020.
           ADD   1              TO  CNT1.
           IF  CNT1           =  13
               GO  TO  MOV9-EX
           END-IF
           IF  W-TE(CNT1)     =  SPACE
               GO  TO  MOV9-020
           END-IF
           ADD   1              TO  CNT2
           IF  CNT2           =  8
               GO  TO  MOV9-EX
           ELSE
               MOVE  W-TE(CNT1)     TO  W-TEKI(CNT2)
               GO  TO  MOV9-010
           END-IF.
       MOV9-EX.
           EXIT.
      ******************************************************************
      *    SUR-SET-RTN     数量セット処理                              *
      ******************************************************************
       SUR-SET-RTN.
           MOVE  1             TO  S.
       SUR-SET-010.
           IF  S      >      10
               GO  TO  SUR-SET-EX
           END-IF
           IF  W-DC   =      0
               ADD   JNSR-081(S)   TO  WG-SUR(I N S)
               ADD   JNSR-081(S)   TO  WG-GOK(I)  WG-SOKSU(I)
           ELSE
               ADD   JTW-081(S)    TO  WG-SUR(I N S)
               ADD   JTW-081(S)    TO  WG-GOK(I)  WG-SOKSU(I)
           END-IF
           ADD   1             TO  S.
           GO  TO  SUR-SET-010.
       SUR-SET-EX.
           EXIT.
      ******************************************************************
      *    DSP1-RTN   １～１２行　画面表示処理                         *
      ******************************************************************
       DSP1-RTN.
           MOVE  1               TO  I.
           MOVE  3               TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       DSP1-010.
           IF  I      >        12
                 OR     WG-SW(I)     =  0
               GO  TO  DSP1-EX
           END-IF
           CALL "SD_Output" USING
            "DSP-TBL12" DSP-TBL12 "p" RETURNING RESU.
           ADD   1               TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  DSP1-010.
       DSP1-EX.
           EXIT.
      ******************************************************************
      *    DSP2-RTN   行明細　　　画面表示処理                         *
      ******************************************************************
       DSP2-RTN.
           MOVE  "得意先名"    TO  WG-DAI.
           MOVE  WG-TOKCD(I)     TO  TC-TCD.
           MOVE  001             TO  TC-CCD.
      *           READ  TC-M       UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE         TO  TC-NAME
           END-IF
           MOVE  TC-NAME         TO  WG-TOKNM.
           CALL "SD_Output" USING "DSP-DAI" DSP-DAI "p" RETURNING RESU.
           IF  WG-KBN(I)  NOT <  22  AND  NOT  >  25
               CALL "SD_Output" USING
                "DSP-TOKNM" DSP-TOKNM "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-JYUNO" DSP-JYUNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-DENNO" DSP-DENNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-OKRNO" DSP-OKRNO "p" RETURNING RESU
           END-IF
           IF  WG-KBN(I)  =  10   OR   11
               CALL "SD_Output" USING
                "DSP-DENNO" DSP-DENNO "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "DSP-KURA" DSP-KURA "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-UNSO" DSP-UNSO "p" RETURNING RESU.
           MOVE  19            TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           MOVE  14            TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           MOVE  1             TO  N  S.
       DSP2-010.
           IF  N               >  4
               GO  TO  DSP2-020
           END-IF
           CALL "SD_Output" USING "DSP-SUR" DSP-SUR "p" RETURNING RESU.
           IF  S           NOT =  10
               ADD  1        TO  S
               ADD  6        TO  COLU
               CALL "SD_Arg_Match_Col" USING
                "COLU" "2" COLU RETURNING RESU
               GO  TO  DSP2-010
           END-IF
           ADD   1              TO  N  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           MOVE  1              TO  S.
           MOVE  14             TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           GO  TO  DSP2-010.
       DSP2-020.
           CALL "SD_Output" USING "DSP-GOK" DSP-GOK "p" RETURNING RESU.
       DSP2-EX.
           EXIT.
      ******************************************************************
      *    ZAI-RTN    行明細　　　画面表示処理  （在庫）               *
      ******************************************************************
       ZAI-RTN.
           INITIALIZE                WG-ZAI-TBL.
           MOVE  "在　　庫"      TO  WG-DAI.
           INITIALIZE                NJZAI-KEY.
           MOVE  9               TO  NJZAI-01.
           MOVE  WG-HINCD        TO  NJZAI-02.
      *           START  NJZAI  KEY  NOT <  NJZAI-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "JZA2-ERR" JZA2-ERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ZAI-EX
           END-IF.
       ZAI-010.
      *           READ  NJZAI      NEXT  UNLOCK  AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ZAI-020
           END-IF
           IF  NJZAI-02     NOT =  WG-HINCD
               GO  TO  ZAI-020
           END-IF
           MOVE  NJZAI-03         TO  N.
           PERFORM  ZAI-SET-RTN   THRU  ZAI-SET-EX.
           GO  TO  ZAI-010.
      *
       ZAI-020.
           PERFORM  DSP3-RTN   THRU  DSP3-EX.
       ZAI-EX.
           EXIT.
      ******************************************************************
      *    ZAI-SET-RTN     数量セット処理    （在庫）                  *
      ******************************************************************
       ZAI-SET-RTN.
           MOVE  ZERO          TO  S.
       ZAI-SET-010.
           ADD   1             TO  S.
           IF  S               =   11
               GO  TO  ZAI-SET-EX
           END-IF
           COMPUTE  WG-ZAI(N,S)  =    WG-ZAI(N,S)  +  NJZAI-0411(S)  -
                    NJZAI-0511(S)  +  NJZAI-0611(S)  +  NJZAI-0711(S)
                 -  NJZAI-0811(S)  -  NJZAI-0911(S)  +  NJZAI-1111(S).
           COMPUTE  WG-ZGOK      =    WG-ZGOK      +  NJZAI-0411(S)  -
                    NJZAI-0511(S)  +  NJZAI-0611(S)  +  NJZAI-0711(S)
                 -  NJZAI-0811(S)  -  NJZAI-0911(S)  +  NJZAI-1111(S).
           GO  TO  ZAI-SET-010.
       ZAI-SET-EX.
           EXIT.
      ******************************************************************
      *    NSK-RTN    行明細　　画面表示処理  （入出庫計）             *
      ******************************************************************
       NSK-RTN.
           INITIALIZE                WG-ZAI-TBL.
           IF  WG-ACT2         =  91
               MOVE  "入庫計　"    TO  WG-DAI
           ELSE
               MOVE  "出庫計　"    TO  WG-DAI
           END-IF
           MOVE      0          TO  WG-DC.
           MOVE      WG-HINCD   TO  JNSR-01.
           MOVE      WG-FHIZ    TO  JNSR-02.
           MOVE      ZERO       TO  JNSR-03  JNSR-04  JNSR-05.
      *
      *           START     JNSR       KEY     NOT <      JNSR-KEY1   INVALID
      *///////////////
           CALL "DB_Start" USING
            JNSR_PNAME1 "JNSR-KEY1" " NOT < " JNSR-KEY1 RETURNING RET.
           IF  RET = 1
               GO  TO   NSK-020
           END-IF.
       NSK-010.
      *           READ      JNSR       NEXT  UNLOCK  AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNSR_PNAME1 BY REFERENCE JNSR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  NSK-020
           END-IF
           IF  JNSR-10         = 2
               GO  TO  NSK-010
           END-IF
           IF  JNSR-01     NOT = WG-HINCD
               GO  TO  NSK-020
           END-IF
           IF  JNSR-02     >     WG-THIZ
               GO  TO  NSK-020
           END-IF
           IF  WG-ACT2         =  91
               IF  JNSR-03     >   19
                   GO  TO  NSK-010
               END-IF
           END-IF
           IF  WG-ACT2         =  92
               IF  JNSR-03     <   20
                   GO  TO  NSK-010
               END-IF
           END-IF
           PERFORM  NSK-SET-RTN   THRU  NSK-SET-EX.
           GO  TO  NSK-010.
      *
       NSK-020.
           IF  WG-ACT2         =  91
               GO  TO  NSK-090
           END-IF
           IF  WG-THIZ    NOT  =  99999999
               GO  TO  NSK-090
           END-IF
           MOVE      1          TO  WG-DC.
           MOVE      WG-HINCD   TO  JTW-01.
           MOVE      WG-FHIZ    TO  JTW-02.
           MOVE      ZERO       TO  JTW-03  JTW-04  JTW-05.
      *
      *           START     JT-W170    KEY     NOT <      JTW-KEY1     INVALID
      *///////////////
           CALL "DB_Start" USING
            JT-W170_PNAME1 "JTW-KEY1" " NOT < " JTW-KEY1 RETURNING RET.
           IF  RET = 1
               GO  TO   NSK-090
           END-IF.
       NSK-030.
      *           READ      JT-W170    NEXT  UNLOCK  AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-W170_PNAME1 BY REFERENCE JTW-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  NSK-090
           END-IF
           IF  JTW-10          = 2
               GO  TO  NSK-030
           END-IF
           IF  JTW-01      NOT = WG-HINCD
               GO  TO  NSK-090
           END-IF
           IF  JTW-02      >     WG-THIZ
               GO  TO  NSK-090
           END-IF
           PERFORM  NSK-SET-RTN   THRU  NSK-SET-EX.
           GO  TO  NSK-030.
       NSK-090.
           PERFORM  DSP3-RTN   THRU  DSP3-EX.
       NSK-EX.
           EXIT.
      ******************************************************************
      *    NSK-SET-RTN     数量セット処理    （入庫・出庫計）          *
      ******************************************************************
       NSK-SET-RTN.
           IF  WG-ACT2     =  91
               IF  WG-DC         =     0
                   IF  JNSR-03         =  12
                       GO  TO  NSK-SET-010
                   END-IF
               END-IF
           END-IF
           IF  WG-ACT2     =  92
               IF  WG-DC         =     0
                   IF  JNSR-03         =  23
                       GO  TO  NSK-SET-010
                   END-IF
               END-IF
           END-IF
           IF  WG-ACT2     =  92
               IF  WG-DC         =     1
                   IF  JTW-03          =  23
                       GO  TO  NSK-SET-010
                   END-IF
               END-IF
           END-IF
           IF  WG-DC       =     0
               MOVE  JNSR-07      TO  N
               ADD   JNSR-081(01) TO  WG-ZAI(N,01)  WG-ZGOK
               ADD   JNSR-081(02) TO  WG-ZAI(N,02)  WG-ZGOK
               ADD   JNSR-081(03) TO  WG-ZAI(N,03)  WG-ZGOK
               ADD   JNSR-081(04) TO  WG-ZAI(N,04)  WG-ZGOK
               ADD   JNSR-081(05) TO  WG-ZAI(N,05)  WG-ZGOK
               ADD   JNSR-081(06) TO  WG-ZAI(N,06)  WG-ZGOK
               ADD   JNSR-081(07) TO  WG-ZAI(N,07)  WG-ZGOK
               ADD   JNSR-081(08) TO  WG-ZAI(N,08)  WG-ZGOK
               ADD   JNSR-081(09) TO  WG-ZAI(N,09)  WG-ZGOK
               ADD   JNSR-081(10) TO  WG-ZAI(N,10)  WG-ZGOK
           ELSE
               MOVE  JTW-07       TO  N
               ADD   JTW-081(01)  TO  WG-ZAI(N,01)  WG-ZGOK
               ADD   JTW-081(02)  TO  WG-ZAI(N,02)  WG-ZGOK
               ADD   JTW-081(03)  TO  WG-ZAI(N,03)  WG-ZGOK
               ADD   JTW-081(04)  TO  WG-ZAI(N,04)  WG-ZGOK
               ADD   JTW-081(05)  TO  WG-ZAI(N,05)  WG-ZGOK
               ADD   JTW-081(06)  TO  WG-ZAI(N,06)  WG-ZGOK
               ADD   JTW-081(07)  TO  WG-ZAI(N,07)  WG-ZGOK
               ADD   JTW-081(08)  TO  WG-ZAI(N,08)  WG-ZGOK
               ADD   JTW-081(09)  TO  WG-ZAI(N,09)  WG-ZGOK
               ADD   JTW-081(10)  TO  WG-ZAI(N,10)  WG-ZGOK
           END-IF
           GO  TO  NSK-SET-EX.
       NSK-SET-010.
           IF  WG-DC       =     0
               MOVE  JNSR-07      TO  N
               SUBTRACT JNSR-081(01) FROM  WG-ZAI(N,01)  WG-ZGOK
               SUBTRACT JNSR-081(02) FROM  WG-ZAI(N,02)  WG-ZGOK
               SUBTRACT JNSR-081(03) FROM  WG-ZAI(N,03)  WG-ZGOK
               SUBTRACT JNSR-081(04) FROM  WG-ZAI(N,04)  WG-ZGOK
               SUBTRACT JNSR-081(05) FROM  WG-ZAI(N,05)  WG-ZGOK
               SUBTRACT JNSR-081(06) FROM  WG-ZAI(N,06)  WG-ZGOK
               SUBTRACT JNSR-081(07) FROM  WG-ZAI(N,07)  WG-ZGOK
               SUBTRACT JNSR-081(08) FROM  WG-ZAI(N,08)  WG-ZGOK
               SUBTRACT JNSR-081(09) FROM  WG-ZAI(N,09)  WG-ZGOK
               SUBTRACT JNSR-081(10) FROM  WG-ZAI(N,10)  WG-ZGOK
           ELSE
               MOVE  JTW-07       TO  N
               SUBTRACT JTW-081(01)  FROM  WG-ZAI(N,01)  WG-ZGOK
               SUBTRACT JTW-081(02)  FROM  WG-ZAI(N,02)  WG-ZGOK
               SUBTRACT JTW-081(03)  FROM  WG-ZAI(N,03)  WG-ZGOK
               SUBTRACT JTW-081(04)  FROM  WG-ZAI(N,04)  WG-ZGOK
               SUBTRACT JTW-081(05)  FROM  WG-ZAI(N,05)  WG-ZGOK
               SUBTRACT JTW-081(06)  FROM  WG-ZAI(N,06)  WG-ZGOK
               SUBTRACT JTW-081(07)  FROM  WG-ZAI(N,07)  WG-ZGOK
               SUBTRACT JTW-081(08)  FROM  WG-ZAI(N,08)  WG-ZGOK
               SUBTRACT JTW-081(09)  FROM  WG-ZAI(N,09)  WG-ZGOK
               SUBTRACT JTW-081(10)  FROM  WG-ZAI(N,10)  WG-ZGOK
           END-IF.
       NSK-SET-EX.
           EXIT.
      ******************************************************************
      *    DSP3-RTN   行明細　画面表示処理  （入庫計・出庫計・在庫）   *
      ******************************************************************
       DSP3-RTN.
           CALL "SD_Output" USING "CLR-06" CLR-06 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-07" CLR-07 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-13" CLR-13 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-08" CLR-08 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-09" CLR-09 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-11" CLR-11 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-12" CLR-12 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-DAI" DSP-DAI "p" RETURNING RESU.
           MOVE  1              TO  N  S.
           MOVE  19             TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           MOVE  14             TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
       DSP3-010.
           IF  N               >  4
               GO  TO  DSP3-020
           END-IF
           CALL "SD_Output" USING "DSP-ZAI" DSP-ZAI "p" RETURNING RESU.
           IF  S           NOT =  10
               ADD  1        TO  S
               ADD  6        TO  COLU
               CALL "SD_Arg_Match_Col" USING
                "COLU" "2" COLU RETURNING RESU
               GO  TO  DSP3-010
           END-IF
           ADD   1              TO  N  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           MOVE  1              TO  S.
           MOVE  14             TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           GO  TO  DSP3-010.
       DSP3-020.
           CALL "SD_Output" USING
            "DSP-ZGOK" DSP-ZGOK "p" RETURNING RESU.
       DSP3-EX.
           EXIT.
      ******************************************************************
      *    CLR-RTN         画面クリア処理  　　　　　　　              *
      ******************************************************************
       CLR-RTN.
           MOVE  ZERO          TO  WG-HINCD  WG-FHIZ  WG-THIZ.
           MOVE  SPACE         TO  WG-HINNM.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-03" CLR-03 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-04" CLR-04 "p" RETURNING RESU.
       CLR-010.
           MOVE  1             TO  I.
           MOVE  3             TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR-015.
           IF  I     >     12
               GO  TO  CLR-020
           END-IF
           INITIALIZE          WG-TBL12(I).
           CALL "SD_Output" USING
            "CLR-TBL12" CLR-TBL12 "p" RETURNING RESU.
           ADD   1             TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  CLR-015.
       CLR-020.
           MOVE  19            TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR-025.
           IF  LIN   >     22
               GO  TO  CLR-030
           END-IF
           CALL "SD_Output" USING
            "CLR-TBL4" CLR-TBL4 "p" RETURNING RESU.
           ADD   1             TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  CLR-025.
       CLR-030.
           CALL "SD_Output" USING "CLR-05" CLR-05 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-06" CLR-06 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-07" CLR-07 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-08" CLR-08 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-09" CLR-09 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-10" CLR-10 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-13" CLR-13 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-11" CLR-11 "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-12" CLR-12 "p" RETURNING RESU.
       CLR-EX.
           EXIT.
      ******************************************************************
      *                    ＥＮＤ－ＲＴＮ                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-W170_IDLST JT-W170_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
       END-EX.
           EXIT.
       COPY  LPMSG.
