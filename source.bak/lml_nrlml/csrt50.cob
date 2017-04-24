000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. CSRT10.
000030*************************************************************
000040**   簡易ソート処理      DKY=NO_                           **
000050**    #SORT起動 (CBLRUN)                                   **
000060*************************************************************
000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SOURCE-COMPUTER. SYSTEM7200.
000100 OBJECT-COMPUTER. SYSTEM7200.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.
000130 DATA DIVISION.
000140 FILE SECTION.
000150 WORKING-STORAGE SECTION.
000160 01  ERR-STAT           PIC  X(002).
000170 01  STN-NO.
000180     03  STN-NO-1       PIC  X(003) VALUE SPACE.
000190     03  STN-NO-2       PIC  9(003) VALUE ZERO.
000200 01  W-DATA.
000210     02  HIZUKE.
000220       03  YY           PIC  9(002) VALUE ZERO.
000230       03  MM           PIC  9(002) VALUE ZERO.
000240       03  DD           PIC  9(002) VALUE ZERO.
000250     02  K-MSG          PIC  X(040) VALUE SPACE.
000260     02  W-KBN          PIC  9(002) VALUE ZERO.
000270     02  W-FID1.
000280       03  W-FID11      PIC  X(006) VALUE SPACE.
000290       03  W-FID12      PIC  9(003).
000300       03  W-FID13      PIC  X(003) VALUE SPACE.
000310     02  W-FID2.
000320       03  W-FID21      PIC  X(006) VALUE SPACE.
000330       03  W-FID22      PIC  9(003).
000340       03  W-FID23      PIC  X(003) VALUE SPACE.
000350     02  I-ID           PIC  X(012) VALUE SPACE.
000360     02  O-ID           PIC  X(012) VALUE SPACE.
000370     02  SRTKEY         PIC  X(062) VALUE SPACE.
000380     02  SRTOUT1        PIC  X(060) VALUE SPACE.
000390     02  SRTOUT2        PIC  X(060) VALUE SPACE.
000400     02  SRTSUM1        PIC  X(060) VALUE SPACE.
000410*****02  SRTSUM2        PIC  X(039) VALUE SPACE.                  D.020207
000420     02  SRTSUM2        PIC  X(049) VALUE SPACE.                  I.020207
000430     02  SRTSEL1        PIC  X(060) VALUE SPACE.
000440     02  SRTSEL2        PIC  X(038) VALUE SPACE.
000450*
000460*****　指定ファイル　　　　　→　ワークファイル(ステーション対応)
000470*****        W-KBN  =  10
000480*****        W-KBN  =  01    ←
000490*****        W-KBN  =  55         ＩＦＩ≠ＯＦＩ（指定ファイル）
000500 01  ASORT-PAR.
000510     03  F              PIC  X(006) VALUE "#SORT;".
000520*****03  F              PIC  X(027) VALUE                         D.020207
000530*****     "SRT=          _IDE=MSD_IFI=".                          D.020207
000540     03  F              PIC  X(017) VALUE                         I.020207
000550          "SRT=_IDE=MSD_IFI=".                                    I.020207
000560     03  A-I-ID         PIC  X(012) VALUE SPACE.
000570     03  F              PIC  X(013) VALUE "_ODE=MSD_OFI=".
000580     03  A-O-ID         PIC  X(012) VALUE SPACE.
000590     03  F              PIC  X(042) VALUE
000600          "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
000610     03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
000620     03  A-SRTKEY       PIC  X(056) VALUE SPACE.
000630     03  F              PIC  X(005) VALUE "_OUT=".
000640     03  A-SRTOUT1      PIC  X(060) VALUE SPACE.
000650     03  A-SRTOUT2      PIC  X(060) VALUE SPACE.
000660     03  F              PIC  X(005) VALUE "_SUM=".
000670     03  A-SRTSUM1      PIC  X(060) VALUE SPACE.
000680     03  A-SRTSUM2      PIC  X(049) VALUE SPACE.                  I.020207
000690*****03  A-SRTSUM2      PIC  X(039) VALUE SPACE.                  D.020207
000700     03  F              PIC  X(010) VALUE "_TOT=_SEL=".
000710     03  A-SRTSEL1      PIC  X(060) VALUE SPACE.
000720     03  A-SRTSEL2      PIC  X(024) VALUE SPACE.
000730     03  F              PIC  X(006) VALUE "_ALT=_".
000740     03  F              PIC  X(002) VALUE "/>".
000750*****　ワークファイル(ｽﾃｰｼｮﾝ対応)　→　ワークファイル(ｽﾃｰｼｮﾝ対応)
000760*****       W-KBN  =  00         ＩＦＩ＝ＯＦＩ
000770*****       W-KBN  =  22         ＩＦＩ≠ＯＦＩ
000780*****       W-KBN  =  11         ＩＦＩ＝ＯＦＩ（指定ファイル）
000790 01  BSORT-PAR.
000800     03  F              PIC  X(006) VALUE "#SORT;".
000810*****03  F              PIC  X(027) VALUE                         D.020207
000820*****    "SRT=          _IDE=MSD_IFI=".                           D.020207
000830     03  F              PIC  X(017) VALUE                         I.020207
000840         "SRT=_IDE=MSD_IFI=".                                     I.020207
000850     03  B-I-ID         PIC  X(012) VALUE SPACE.
000860     03  F              PIC  X(013) VALUE "_ODE=MSD_OFI=".
000870     03  B-O-ID         PIC  X(012) VALUE SPACE.
000880     03  F              PIC  X(034) VALUE
000890         "_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
000900     03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
000910     03  B-SRTKEY       PIC  X(062) VALUE SPACE.
000920     03  F              PIC  X(005) VALUE "_OUT=".
000930     03  B-SRTOUT1      PIC  X(060) VALUE SPACE.
000940     03  B-SRTOUT2      PIC  X(060) VALUE SPACE.
000950     03  F              PIC  X(005) VALUE "_SUM=".
000960     03  B-SRTSUM1      PIC  X(060) VALUE SPACE.
000970     03  B-SRTSUM2      PIC  X(049) VALUE SPACE.                  I.020207
000980*****03  B-SRTSUM2      PIC  X(039) VALUE SPACE.                  D.020207
000990     03  F              PIC  X(010) VALUE "_TOT=_SEL=".
001000     03  B-SRTSEL1      PIC  X(060) VALUE SPACE.
001010     03  B-SRTSEL2      PIC  X(026) VALUE SPACE.
001020     03  F              PIC  X(006) VALUE "_ALT=_".
001030     03  F              PIC  X(002) VALUE "/>".
001040*****　ワークファイル(ｽﾃｰｼｮﾝ対応) 　→　入力ファイル(ODE入力)
001050*****       W-KBN  =  03
001060 01  CSORT-PAR.
001070     03  F              PIC  X(006) VALUE "#SORT;".
001080*****03  F              PIC  X(027) VALUE                         D.020207
001090*****     "SRT=          _IDE=MSD_IFI=".                          D.020207
001100     03  F              PIC  X(017) VALUE                         I.020207
001110          "SRT=_IDE=MSD_IFI=".                                    I.020207
001120     03  C-I-ID         PIC  X(012) VALUE SPACE.
001130     03  F              PIC  X(005) VALUE "_ODE=".
001140     03  C-SRTODE       PIC  X(003) VALUE SPACE.
001150     03  F              PIC  X(005) VALUE "_OFI=".
001160     03  C-O-ID         PIC  X(012) VALUE SPACE.
001170     03  F              PIC  X(042) VALUE
001180          "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
001190     03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
001200     03  C-SRTKEY       PIC  X(050) VALUE SPACE.
001210     03  F              PIC  X(005) VALUE "_OUT=".
001220     03  C-SRTOUT1      PIC  X(060) VALUE SPACE.
001230     03  C-SRTOUT2      PIC  X(060) VALUE SPACE.
001240     03  F              PIC  X(005) VALUE "_SUM=".
001250     03  C-SRTSUM1      PIC  X(060) VALUE SPACE.
001260     03  C-SRTSUM2      PIC  X(045) VALUE SPACE.                  I.020207
001270*****03  C-SRTSUM2      PIC  X(035) VALUE SPACE.                  D.020207
001280     03  F              PIC  X(010) VALUE "_TOT=_SEL=".
001290     03  C-SRTSEL1      PIC  X(060) VALUE SPACE.
001300     03  C-SRTSEL2      PIC  X(034) VALUE SPACE.
001310     03  F              PIC  X(006) VALUE "_ALT=_".
001320     03  F              PIC  X(002) VALUE "/>".
001330*****　入力ファイル(IDE入力)        →　ワークファイル(ｽﾃｰｼｮﾝ対応)
001340*****       W-KBN  =  30
001350 01  DSORT-PAR.
001360     03  F              PIC  X(006) VALUE "#SORT;".
001370*****03  F              PIC  X(019) VALUE "SRT=          _IDE=".  D.020207
001380     03  F              PIC  X(009) VALUE "SRT=_IDE=".            I.020207
001390     03  D-SRTIDE       PIC  X(003) VALUE SPACE.
001400     03  F              PIC  X(005) VALUE "_IFI=".
001410     03  D-I-ID         PIC  X(012) VALUE SPACE.
001420     03  F              PIC  X(013) VALUE "_ODE=MSD_OFI=".
001430     03  D-O-ID         PIC  X(012) VALUE SPACE.
001440     03  F              PIC  X(043) VALUE
001450          "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=1_".
001460     03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
001470     03  D-SRTKEY       PIC  X(050) VALUE SPACE.
001480     03  F              PIC  X(005) VALUE "_OUT=".
001490     03  D-SRTOUT1      PIC  X(060) VALUE SPACE.
001500     03  D-SRTOUT2      PIC  X(060) VALUE SPACE.
001510     03  F              PIC  X(005) VALUE "_SUM=".
001520     03  D-SRTSUM1      PIC  X(060) VALUE SPACE.
001530     03  D-SRTSUM2      PIC  X(045) VALUE SPACE.                  I.020207
001540*****03  D-SRTSUM2      PIC  X(035) VALUE SPACE.                  D.020207
001550     03  F              PIC  X(010) VALUE "_TOT=_SEL=".
001560     03  D-SRTSEL1      PIC  X(060) VALUE SPACE.
001570     03  D-SRTSEL2      PIC  X(033) VALUE SPACE.
001580     03  F              PIC  X(006) VALUE "_ALT=_".
001590     03  F              PIC  X(002) VALUE "/>".
001600*****　入力ファイル　　　　　→　入力ファイル(ODE入力)
001610*****        W-KBN  =  13
001620 01  ESORT-PAR.
001630     03  F              PIC  X(006) VALUE "#SORT;".
001640*****03  F              PIC  X(027) VALUE                         D.020207
001650*****     "SRT=          _IDE=MSD_IFI=".                          D.020207
001660     03  F              PIC  X(017) VALUE                         I.020207
001670          "SRT=_IDE=MSD_IFI=".                                    I.020207
001680     03  E-I-ID         PIC  X(012) VALUE SPACE.
001690     03  F              PIC  X(005) VALUE "_ODE=".
001700     03  E-SRTODE       PIC  X(003) VALUE SPACE.
001710     03  F              PIC  X(005) VALUE "_OFI=".
001720     03  E-O-ID         PIC  X(012) VALUE SPACE.
001730     03  F              PIC  X(042) VALUE
001740          "_IFO=SHA_WKD=TEM_WSZ=_LST=_SAV=_NXT=_NOV=_".
001750     03  F              PIC  X(013) VALUE "DKY=NO  _KEY=".
001760     03  E-SRTKEY       PIC  X(056) VALUE SPACE.
001770     03  F              PIC  X(005) VALUE "_OUT=".
001780     03  E-SRTOUT1      PIC  X(060) VALUE SPACE.
001790     03  E-SRTOUT2      PIC  X(060) VALUE SPACE.
001800     03  F              PIC  X(005) VALUE "_SUM=".
001810     03  E-SRTSUM1      PIC  X(060) VALUE SPACE.
001820     03  E-SRTSUM2      PIC  X(045) VALUE SPACE.                  I.020207
001830*****03  E-SRTSUM2      PIC  X(035) VALUE SPACE.                  D.020207
001840     03  F              PIC  X(010) VALUE "_TOT=_SEL=".
001850     03  E-SRTSEL1      PIC  X(060) VALUE SPACE.
001860     03  E-SRTSEL2      PIC  X(028) VALUE SPACE.
001870     03  F              PIC  X(006) VALUE "_ALT=_".
001880     03  F              PIC  X(002) VALUE "/>".
001890 01  PAR-SIZ.
001900     03  PAR-SIZ01      PIC  9(004) VALUE 512.
001910 SCREEN SECTION.
001920 SD  C-CRT
001930     END  STATUS    ESTS.
001940 01  C-MID1    CLEAR    SCREEN.
001950     02  LINE  01.
001960       03  COLUMN  20  PIC  X(040) FROM  K-MSG  REVERSE.
001970       03  COLUMN  68  PIC  X(011) VALUE "DATE.  /  /".
001980       03  COLUMN  73  PIC  9(002) FROM  YY.
001990       03  COLUMN  76  PIC  9(002) FROM  MM.
002000       03  COLUMN  79  PIC  9(002) FROM  DD.
002010 01  C-MID2.
002020     03  LINE  12  COLUMN  25  VALUE "*************************".
002030     03  LINE  13  COLUMN  25  VALUE "**                     **".
002040     03  LINE  13  COLUMN  27  VALUE   "  ソート処理実行中".
002050     03  LINE  14  COLUMN  25  VALUE "*************************".
002060 01  C-ERR.
002070     02  LINE  24.
002080*****  03  E-ME1   COLUMN  15  PIC  X(017) VALUE
002090*****       "***  FILM ﾅｼ  ***".
002100*****  03  E-ME9   COLUMN  15  PIC  X(046) VALUE
002110*****       "【　この画面では実行不可　】         END=RESET".
002120       03  E-ME98  COLUMN  75  PIC  X(005) VALUE ""27"J"05"".
002130       03  E-ME99  COLUMN  75  PIC  X(005) VALUE ""27"B"05"".
002140 PROCEDURE                   DIVISION.
002150 M-05.
002160     CALL "CBLSTNNO" USING STN-NO.
002170     MOVE STN-NO-2 TO W-FID12 W-FID22.
002180     ACCEPT HIZUKE FROM DATE.
002190*
002200     ACCEPT W-KBN.
002210     IF W-KBN = 30
002220         ACCEPT D-SRTIDE.
002230*****IF W-KBN = 10 OR 30                                          D.000519
002240*****IF W-KBN = 10 OR 30 OR 13                                    D.010118
002250*****IF W-KBN = 10 OR 30 OR 13 OR 55                              D.020209
002260     IF W-KBN = 10 OR 11 OR 30 OR 13 OR 55                        I.020209
002270         ACCEPT I-ID.
002280*****IF W-KBN NOT = 13                                            D.010118
002290*****IF W-KBN NOT = 13 AND 55                                     D.020209
002300     IF W-KBN NOT = 11 AND 13 AND 55                              I.020209
002310         ACCEPT W-FID11.
002320*****IF W-KBN = 00 OR 22 OR 03                                    D.970617
002330     IF W-KBN = 00 OR 22 OR 01 OR 03                              I.970617
002340         MOVE W-FID1 TO I-ID.
002350*
002360*****IF W-KBN = 01                                                D.010118
002370     IF W-KBN = 01 OR 55                                          I.010118
002380         ACCEPT O-ID.                                             I.970617
002390     IF W-KBN = 03
002400         ACCEPT C-SRTODE
002410         ACCEPT O-ID.
002420     IF W-KBN = 13                                                I.000519
002430         ACCEPT E-SRTODE                                          I.000519
002440         ACCEPT O-ID.                                             I.000519
002450     IF W-KBN = 22
002460         ACCEPT W-FID21
002470         MOVE W-FID2 TO O-ID.
002480     IF W-KBN = 00 OR 10 OR 30
002490         MOVE W-FID1 TO O-ID.
002500     IF W-KBN = 11                                                I.020209
002510         MOVE I-ID TO O-ID.                                       I.020209
002520*
002530     IF SPACE = I-ID OR O-ID
002540         MOVE 255 TO COMPLETION-CODE
002550         STOP RUN.
002560*
002570     ACCEPT SRTKEY.                                               ｿ-ﾄｷ-
002580     ACCEPT SRTOUT1.                                              OUTｷ-
002590     ACCEPT SRTOUT2.                                              OUTｷ-
002600     ACCEPT SRTSUM1.                                              SUMｷ-
002610     ACCEPT SRTSUM2.                                              SUMｷ-
002620     ACCEPT SRTSEL1.                                              SELｷ-
002630     ACCEPT SRTSEL2.                                              SELｷ-
002640     ACCEPT K-MSG.
002650*
002660     DISPLAY C-MID1.
002670*****IF W-FID11 = "WK0064" OR "WK0102"
002680*****    IF W-FID12 NOT = 000 AND 001 AND 007
002690*****        DISPLAY E-ME9 E-ME99
002700*****        MOVE 255 TO COMPLETION-CODE
002710*****        STOP RUN.
002720*****IF W-FID21 = "WK0064" OR "WK0102"
002730*****    IF W-FID22 NOT = 000 AND 001 AND 007
002740*****        DISPLAY E-ME9 E-ME99
002750*****        MOVE 255 TO COMPLETION-CODE
002760*****        STOP RUN.
002770     DISPLAY C-MID2.
002780*
002790*****IF W-KBN = 10                                                D.970617
002800*****IF W-KBN = 10 OR 01                                          D.010118
002810     IF W-KBN = 10 OR 01 OR 55                                    I.010118
002820         MOVE I-ID TO A-I-ID
002830         MOVE O-ID TO A-O-ID
002840         MOVE SRTKEY TO A-SRTKEY
002850         MOVE SRTOUT1 TO A-SRTOUT1
002860         MOVE SRTOUT2 TO A-SRTOUT2
002870         MOVE SRTSUM1 TO A-SRTSUM1
002880         MOVE SRTSUM2 TO A-SRTSUM2
002890         MOVE SRTSEL1 TO A-SRTSEL1
002900         MOVE SRTSEL2 TO A-SRTSEL2
002910         CALL "CBLRUN" USING ASORT-PAR PAR-SIZ.
002920*****IF W-KBN = 00 OR 22                                          D.020209
002930     IF W-KBN = 00 OR 11 OR 22                                    I.020209
002940         MOVE I-ID TO B-I-ID
002950         MOVE O-ID TO B-O-ID
002960         MOVE SRTKEY TO B-SRTKEY
002970         MOVE SRTOUT1 TO B-SRTOUT1
002980         MOVE SRTOUT2 TO B-SRTOUT2
002990         MOVE SRTSUM1 TO B-SRTSUM1
003000         MOVE SRTSUM2 TO B-SRTSUM2
003010         MOVE SRTSEL1 TO B-SRTSEL1
003020         MOVE SRTSEL2 TO B-SRTSEL2
003030         CALL "CBLRUN" USING BSORT-PAR PAR-SIZ.
003040     IF W-KBN = 03
003050         MOVE I-ID TO C-I-ID
003060         MOVE O-ID TO C-O-ID
003070         MOVE SRTKEY TO C-SRTKEY
003080         MOVE SRTOUT1 TO C-SRTOUT1
003090         MOVE SRTOUT2 TO C-SRTOUT2
003100         MOVE SRTSUM1 TO C-SRTSUM1
003110         MOVE SRTSUM2 TO C-SRTSUM2
003120         MOVE SRTSEL1 TO C-SRTSEL1
003130         MOVE SRTSEL2 TO C-SRTSEL2
003140         CALL "CBLRUN" USING CSORT-PAR PAR-SIZ.
003150     IF W-KBN = 30
003160         MOVE I-ID TO D-I-ID
003170         MOVE O-ID TO D-O-ID
003180         MOVE SRTKEY TO D-SRTKEY
003190         MOVE SRTOUT1 TO D-SRTOUT1
003200         MOVE SRTOUT2 TO D-SRTOUT2
003210         MOVE SRTSUM1 TO D-SRTSUM1
003220         MOVE SRTSUM2 TO D-SRTSUM2
003230         MOVE SRTSEL1 TO D-SRTSEL1
003240         MOVE SRTSEL2 TO D-SRTSEL2
003250         CALL "CBLRUN" USING DSORT-PAR PAR-SIZ.
003260     IF W-KBN = 13                                                I.000519
003270         MOVE I-ID TO E-I-ID                                      I.000519
003280         MOVE O-ID TO E-O-ID                                      I.000519
003290         MOVE SRTKEY TO E-SRTKEY                                  I.000519
003300         MOVE SRTOUT1 TO E-SRTOUT1                                I.000519
003310         MOVE SRTOUT2 TO E-SRTOUT2                                I.000519
003320         MOVE SRTSUM1 TO E-SRTSUM1                                I.000519
003330         MOVE SRTSUM2 TO E-SRTSUM2                                I.000519
003340         MOVE SRTSEL1 TO E-SRTSEL1                                I.000519
003350         MOVE SRTSEL2 TO E-SRTSEL2                                I.000519
003360         CALL "CBLRUN" USING ESORT-PAR PAR-SIZ.                   I.000519
003370     STOP RUN.
