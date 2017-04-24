000010 FD  JT-WKN2                                                      ﾆｭｳｼｭｯｺ
000020     BLOCK    5     RECORDS
000030     LABEL    RECORD   STANDARD
000040     VALUE    OF  IDENTIFICATION  JT-IWS094ID.
000050*
000060 01  WN02-R.
000070     02  WN02-KEY.                                                KEY
000080         03  WN02-01           PIC 9(06)  COMP-3.                 品名ｺｰﾄﾞ
000090         03  WN02-02           PIC 9(06)  COMP-3.                 年月日
000100         03  WN02-03           PIC 9(02).                         入出力区
000110         03  WN02-04           PIC 9(06)  COMP-3.                 伝票№
000120         03  WN02-06           PIC 9(01).                         行
000130     02  WN02-07               PIC 9(01).                         倉ｺｰﾄﾞ
000140     02  WN02-05               PIC 9(01).                         サイズ
000150     02  WN02-08.                                                 入出庫数
000160         03  WN02-081          PIC S9(04)  COMP-3    OCCURS  10.
000170     02  WN02-09               PIC 9(01).                         生産区分
000180     02  WN02-10               PIC 9(04).                         得意先CD
000190     02  WN02-11               PIC 9(03).                         直送先D
000200     02  WN02-12               PIC N(05).                         配達
000210     02  WN02-13               PIC N(06).                         摘要
000220     02  WN02-14               PIC 9(06)  COMP-3.                 送状№
000230     02  FILLER                PIC X(13).
