000010 FD  JSRU                                                         ｼｭｯｶﾙｲｾｷ
000020     BLOCK    5     RECORDS                                       ﾌｧｲﾙ
000030     LABEL    RECORD   STANDARD
000040     VALUE     OF  IDENTIFICATION  "JSRU1"
000050     ALTERNATE     IDENTIFICATION  "JSRU2".
000060*
000070 01  JSRU-R.
000080     02   JSRU-01               PIC 9(1).                         ｱｽﾞｶﾘｸﾌﾞ
000090     02   JSRU-KEY1.
000100          03   JSRU-06.                                           ｼﾞｭﾁｭｳ
000110               04  JSRU-061     PIC 9(6).                         ｼﾞｭﾁｭｳNO
000120               04  JSRU-062     PIC 9(1).                         ｷﾞｮｳ
000130          03   JSRU-03.                                           ｼｭｯｶﾋﾞ
000140               04  JSRU-031     PIC 9(2).                         ﾈﾝ
000150               04  JSRU-032     PIC 9(2).                         ﾂｷ
000160               04  JSRU-033     PIC 9(2).                         ﾋ
000170          03   JSRU-03A         PIC 9(1).                         ﾚｺｰﾄﾞKBN
000180          03   JSRU-02.                                           ｻｼｽﾞ
000190               04   JSRU-021    PIC 9(6).                         ｼｭｯｶｻｼｽﾞ
000200               04   JSRU-022    PIC 9(1).                         ｷﾞｮｳ
000210     02   JSRU-04.
000220          03  JSRU-041          PIC 9(4).                         ﾄｸｲｺｰﾄﾞ
000230          03  JSRU-042          PIC 9(3).                         ﾁｮｸｿｳ NO
000240     02   JSRU-05               PIC 9(1).                         ｸﾗ ｺｰﾄﾞ
000250     02   JSRU-07               PIC 9(6).                         ﾋﾝｺｰﾄﾞ
000260     02   JSRU-08               PIC 9(1).                         ｻｲｽﾞｸﾌﾞﾝ
000270     02   JSRU-09.                                                ｼｭｯｶｽｳ
000280          03  JSRU-091    OCCURS  10.                             ｻｲｽﾞﾍﾞﾂ
000290              04  JSRU-0911     PIC S9(4)   COMP-3.
000300          03  JSRU-092          PIC S9(6)   COMP-3.               ｹｲ
000310     02   JSRU-10               PIC X(1).                         ﾃﾞﾝｸ
000320     02   JSRU-11.                                                預り
000330          03  JSRU-111          PIC 9(6).                         №
000340          03  JSRU-112          PIC 9(1).                         行№
000350     02   JSRU-KEY2.
000360          03   JSRU-12          PIC 9(4).                         得意先Ｃ
000370          03   JSRU-13.                                           出荷日
000380              04  JSRU-131      PIC 9(2).                           年　
000390              04  JSRU-132      PIC 9(2).                         　月　
000400              04  JSRU-133      PIC 9(2).                         　日
000410          03   JSRU-14          PIC 9(1).                         レコー区
000420          03   JSRU-15.                                           伝票№
000430              04  JSRU-151      PIC 9(6).                         出荷指№
000440              04  JSRU-152      PIC 9(1).                         　行№
000450*****02   FILLER                PIC X(5).                         D.941228
000460     02   JSRU-16               PIC 9(2).                         I.941228
000470     02   JSRU-17               PIC 9(1).                         I.950217
000480*****02   FILLER                PIC X(3).                         D.950217
000490     02   FILLER                PIC X(2).                         I.950217
