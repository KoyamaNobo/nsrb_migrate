000010 FD  JT-WK05
000020*****BLOCK    1     RECORDS                                       D.980108
000030     BLOCK    2     RECORDS                                       I.980108
000040     LABEL    RECORD   STANDARD
000050     VALUE    OF  IDENTIFICATION   WK0128ID.                      I.980108
000060*****VALUE    OF  IDENTIFICATION   JT-OWS256ID.                   D.960209
000070*****VALUE    OF  IDENTIFICATION   WK0256ID.                      D.980108
000080*
000090 01  WK05-R.
000100     02   WK05-01               PIC 9(1).                         ｱｽﾞｶﾘｸﾌﾞ
000110     02   WK05-KEY.
000120          03   WK05-06.                                           ｼﾞｭﾁｭｳ
000130               04  WK05-061     PIC 9(6).                         ｼﾞｭﾁｭｳNO
000140               04  WK05-062     PIC 9(1).                         ｷﾞｮｳ
000150          03   WK05-03.                                           ｼｭｯｶﾋﾞ
000160*****          04  WK05-031     PIC 9(2).                         D.980515
000170               04  WK05-031     PIC 9(4).                         I.980515
000180               04  WK05-032     PIC 9(2).                         ﾂｷ
000190               04  WK05-033     PIC 9(2).                         ﾋ
000200          03   WK05-030   REDEFINES  WK05-03  PIC 9(8).           I.980515
000210*****     03   WK05-030   REDEFINES  WK05-03  PIC 9(6).           D.980515
000220*****     03   WK05-03A         PIC 9(1).                         D.980108
000230          03   WK05-02.                                           ｻｼｽﾞ
000240               04   WK05-021    PIC 9(6).                         ｼｭｯｶｻｼｽﾞ
000250               04   WK05-022    PIC 9(1).                         ｷﾞｮｳ
000260     02   WK05-04.
000270          03  WK05-041          PIC 9(4).                         ﾄｸｲｺｰﾄﾞ
000280          03  WK05-042          PIC 9(3).                         ﾁｮｸｿｳ NO
000290*****02   WK05-05               PIC 9(1).                         D.980108
000300     02   WK05-07               PIC 9(6).                         ﾋﾝｺｰﾄﾞ
000310     02   WK05-08               PIC 9(1).                         ｻｲｽﾞｸﾌﾞﾝ
000320     02   WK05-09.                                                ｼｭｯｶｽｳ
000330          03  WK05-091    OCCURS  10.                             ｻｲｽﾞﾍﾞﾂ
000340              04  WK05-0911     PIC S9(4)   COMP-3.
000350          03  WK05-092          PIC S9(6)   COMP-3.               ｹｲ
000360     02   WK05-10               PIC X(1).                         ﾃﾞﾝｸ
000370*****02   WK05-11.                                                D.980108
000380*****     03  WK05-111          PIC 9(6).                         D.980108
000390*****     03  WK05-112          PIC 9(1).                         D.980108
000400*****02   WK05-KEY2.                                              D.980108
000410*****     03   WK05-12          PIC 9(4).                         D.980108
000420*****     03   WK05-13.                                           D.980108
000430*****         04  WK05-131      PIC 9(2).                         D.980108
000440*****         04  WK05-132      PIC 9(2).                         D.980108
000450*****         04  WK05-133      PIC 9(2).                         D.980108
000460*****     03   WK05-14          PIC 9(1).                         D.980108
000470*****     03   WK05-15.                                           D.980108
000480*****         04  WK05-151      PIC 9(6).                         D.980108
000490*****         04  WK05-152      PIC 9(1).                         D.980108
000500*****02   FILLER                PIC X(5).                         D.950106
000510*****02   WK05-16               PIC 9(2).                         D.980108
000520*****02   FILLER                PIC X(3).                         D.980108
000530*****02   FILLER                PIC X(5).                         D.950106
000540*****02   FILLER                PIC X(4).                         D.980108
000550*****02   FILLER                PIC X(36).                        D.980515
000560*****02   FILLER                PIC X(32).                        D.990408
000570     02   FILLER                PIC X(29).                        I.990408
000580     02   WK05-94               PIC 9(3).                         出荷直送
000590     02   WK05-95               PIC X(6).                         受注品名
000600     02   WK05-96.                                                受注日
000610*****     03  WK05-961          PIC 9(2).                         D.980515
000620          03  WK05-961          PIC 9(4).                         I.980515
000630          03  WK05-961L  REDEFINES  WK05-961.                     I.980527
000640              04  WK05-9611     PIC 9(2).                         I.980527
000650              04  WK05-9612     PIC 9(2).                         I.980527
000660          03  WK05-962          PIC 9(2).                         　月　
000670          03  WK05-963          PIC 9(2).                         　日
000680*****02   WK05-97               PIC 9(4).                         D.950106
000690     02   WK05-97               PIC 9(5).                         I.950106
000700     02   WK05-98               PIC S9(3).                        セット数
000710     02   WK05-99               PIC 9(2).                         月度
000720*****02   FILLER                PIC X(128).                       D.980108
