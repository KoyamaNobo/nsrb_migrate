       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM211.
      *********************************************************
      *    PROGRAM         :  社店マスタリスト                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　社店マスター　プルーフリスト　　＊＊＊".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZZ9.
       01  HEAD2.
           02  F              PIC  X(085) VALUE SPACE.
           02  F              PIC  N(003) VALUE "取引先".
           02  F              PIC  X(005) VALUE   " 分類".
           02  F              PIC  X(006) VALUE   "  伝区".
           02  F              PIC  X(007) VALUE   " 発注日".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(008) VALUE   "品ｺｰﾄﾞ  ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(003) VALUE "伝票№".
       01  HEAD3.
           02  F              PIC  X(010) VALUE   "    ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "直　送　先　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(002) VALUE "社店".
           02  F              PIC  X(006) VALUE   "ｺｰﾄﾞ  ".
           02  F              PIC  N(009) VALUE "社　名　／　店　名".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(008) VALUE   "伝№FROM".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(002) VALUE   "TO".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  X(006) VALUE   "ｶﾚﾝﾄ№".
       01  MEI1.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  W-MEI1.
             03  F            PIC  X(001) VALUE SPACE.
             03  M-S011       PIC  9(004).
             03  M-V          PIC  X(001).
             03  M-S012       PIC  9(003).
             03  F            PIC  X(001) VALUE SPACE.
             03  M-NAME       PIC  N(026).
             03  F            PIC  X(001) VALUE SPACE.
             03  M-S02        PIC  9(009).
             03  F            PIC  X(001) VALUE SPACE.
             03  M-S03        PIC  N(016).
             03  F            PIC  X(001) VALUE SPACE.
             03  M-S05        PIC  9(006).
             03  F            PIC  X(001) VALUE SPACE.
             03  M-S06        PIC  X(004).
             03  F            PIC  X(003) VALUE SPACE.
             03  M-S07        PIC  9(002).
             03  F            PIC  X(004) VALUE SPACE.
             03  M-S09        PIC  9(001).
             03  F            PIC  X(007) VALUE SPACE.
             03  M-S10        PIC  9(001).
             03  F            PIC  X(007) VALUE SPACE.
             03  M-S11        PIC  9(001).
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  MEI2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  W-MEI2.
             03  F            PIC  X(060) VALUE SPACE.
             03  M-S04        PIC  N(016).
             03  F            PIC  X(001) VALUE SPACE.
             03  M-S081       PIC  9(010).
             03  F            PIC  X(004) VALUE SPACE.
             03  M-S082       PIC  9(010).
             03  F            PIC  X(004) VALUE SPACE.
             03  M-S083       PIC  9(010).
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-SCR.
           02  W-TCDFR        PIC  9(004).
           02  W-TCDTO        PIC  9(004).
           02  W-DMM          PIC  9(001).
       01  W-DATA.
           02  W-YMD          PIC  9(006).
           02  W-PAGE         PIC  9(004).
           02  W-TCD          PIC  9(004).
           02  W-FLG          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITCM.
           COPY LSTENM.
           COPY LSPF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  SCR-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  FILLER.
             03  A-TCDFR    PIC  9(004).
             03  A-TCDTO    PIC  9(004).
           02  FILLER.
             03  A-DMM      PIC  9(001).
       01  DSP-AREA.
           02  D-TITLE    PIC  N(020)
               VALUE  "＊＊＊　　社店マスター　リスト　　＊＊＊".
           02  D-TCD      PIC  X(040)
               VALUE    "<  得意先ｺｰﾄﾞ      より      迄打出し  >".
           02  D-DMM      PIC  X(021)
               VALUE    "確認 OK=1 NO=9   ﾘﾀｰﾝ".
           COPY LSSEM.
      *
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *SCR-CLEAR
       CALL "SD_Init" USING
           "SCR-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
          "01SCR-CLEAR" "X" "1" "0" "12" " " "SCR-CLEAR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "6" "0" "8" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCDFR" "9" "6" "29" "4" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCDFR" BY REFERENCE W-TCDFR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCDTO" "9" "6" "39" "4" "A-TCDFR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCDTO" BY REFERENCE W-TCDTO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "23" "0" "1" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "66" "1" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TITLE" "N" "1" "16" "40" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TCD" "X" "6" "15" "40" "D-TITLE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DMM" "X" "23" "50" "21" "D-TCD" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAIN-01.
           CALL "DB_F_Open" USING
            "INPUT" STENM_PNAME1 "SHARED" BY REFERENCE STENM_IDLST "1"
            "STE-KEY2" BY REFERENCE STE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "SD_Output" USING
            "SCR-CLEAR" SCR-CLEAR "p" RETURNING RESU.
       MAIN-02.
           PERFORM    ACP-RTN  THRU  ACP-EX.
           PERFORM    LST-RTN  THRU  LST-EX.
       MAIN-05.
           CALL "DB_F_Close" USING
            BY REFERENCE STENM_IDLST STENM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "SD_Output" USING
            "SCR-CLEAR" SCR-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *
      *----入力処理-------------------------------------------------------------
       ACP-RTN.
       ACP-00.
           INITIALIZE    W-SCR.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       ACP-01.
           CALL "SD_Accept" USING BY REFERENCE A-TCDFR "A-TCDFR" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-01
           END-IF.
       ACP-02.
           CALL "SD_Accept" USING BY REFERENCE A-TCDTO "A-TCDTO" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-01
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-02
           END-IF
           IF  W-TCDTO < W-TCDFR
               GO TO    ACP-02
           END-IF.
       ACP-03.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO    ACP-02
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO    ACP-03
           END-IF
           IF  W-DMM NOT = 1   AND 9
               GO TO    ACP-03
           END-IF
           IF  W-DMM = 9
               GO TO    ACP-01
           END-IF.
       ACP-EX.
           EXIT.
      *
      *----出力処理-------------------------------------------------------------
       LST-RTN.
           ACCEPT    W-YMD  FROM  DATE.
           MOVE    ZERO TO  STE-KEY2  W-PAGE.
           MOVE    W-TCDFR  TO  STE-011.
      *           START   STENM    KEY NOT < STE-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            STENM_PNAME1 "STE-KEY2" " NOT < " STE-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO    LST-EX
           END-IF.
      *           READ    STENM    NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" STENM_PNAME1 BY REFERENCE STE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO    LST-EX
           END-IF
           IF  W-TCDTO < STE-011
               GO TO    LST-EX
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM    HEAD-01  THRU  HEAD-EX.
       LST-01.
           MOVE    ZERO  TO  W-FLG.
           MOVE    STE-011  TO  W-TCD.
       LST-02.
           MOVE    SPACE  TO  W-MEI1.
           IF  W-FLG = ZERO
               MOVE    W-TCD  TO  M-S011
               MOVE    "-"  TO  M-V
               MOVE    9  TO  W-FLG
           END-IF
           MOVE    STE-011  TO  TC-TCD.
           MOVE    STE-012  TO  TC-CCD  M-S012.
           MOVE    STE-02   TO  M-S02.
           MOVE    STE-03   TO  M-S03.
           MOVE    STE-05   TO  M-S05.
           MOVE    STE-06   TO  M-S06.
           MOVE    STE-07   TO  M-S07.
           MOVE    STE-09   TO  M-S09.
           MOVE    STE-10   TO  M-S10.
           MOVE    STE-11   TO  M-S11.
      *           READ    TC-M  WITH  UNLOCK
      *                   INVALID KEY    INITIALIZE    M-NAME
      *               NOT INVALID KEY    MOVE    TC-NAME  TO  M-NAME.
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE    M-NAME
           ELSE
               MOVE    TC-NAME  TO  M-NAME
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE    W-TCD  TO  M-S011
               MOVE    "-"  TO  M-V
               PERFORM    HEAD-RTN  THRU  HEAD-EX
           END-IF
           MOVE    SPACE  TO  SP-R.
           MOVE    MEI1   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE    SPACE   TO  W-MEI2.
           MOVE    STE-04  TO  M-S04.
           IF  STE-11 = 1
               MOVE    STE-081  TO  M-S081
               MOVE    STE-082  TO  M-S082
               MOVE    STE-083  TO  M-S083
           END-IF
           MOVE    SPACE  TO  SP-R.
           MOVE    MEI2   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE    SPACE  TO  SP-R.
       LST-03.
      *           READ    STENM    NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" STENM_PNAME1 BY REFERENCE STE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO    LST-09
           END-IF
           IF  STE-011 > W-TCDTO
               GO TO    LST-09
           END-IF
           IF  STE-011 NOT = W-TCD
               GO TO    LST-01
           END-IF
           GO TO    LST-02.
       LST-09.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      *
      *---------------　見出し　印字　------------------------------------------
       HEAD-RTN.
           MOVE     SPACE  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HEAD-01.
           ADD     1  TO  W-PAGE.
           MOVE    W-PAGE   TO  H-PAGE.
           MOVE    W-YMD  TO  H-DATE.
           MOVE    SPACE  TO  SP-R.
           MOVE    HEAD1  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE    SPACE  TO  SP-R.
           MOVE    HEAD2  TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE    SPACE  TO  SP-R.
           MOVE    HEAD3  TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE    SPACE  TO  SP-R.
       HEAD-EX.
           EXIT.
