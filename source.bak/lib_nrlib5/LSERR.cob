000010**
000020**   MESSEGE  AREA                                                61.04.23
000030**
000040 01  DISP-ERR-AREA LINE ERR-LIN.                                  A-890816
000050     02  DISP-MSG-01.
000060         03  COLUMN  1        PIC X(60) FROM ERR-MSGX.
000070     02  DISP-MSG-SPACE.
000080         03  COLUMN  1        PIC X(60) FROM ERR-SPACE.
000090     02  DISP-BUZ-B.
000100         03  COLUMN  80       PIC X(05) VALUE ""27"B"10"".
000110     02  DISP-BUZ-J.
000120         03  COLUMN  80       PIC X(05) VALUE ""27"J"01"".
000130     02  NOR-M01.
000140         03  COLUMN  1        VALUE
000150         NX"21762121255E2539253F2121".
000160*           *       ﾏ   ｽ   ﾀ
000170         03  COLUMN  13       VALUE
000180         NX"45504F3F3A5121212176".
000190*           ﾄｳ  ﾛｸ  ｽﾞﾐ     *
000200     02  NOR-D01.
000210         03  COLUMN  1        VALUE
000220         NX"217621212547213D253F2121".
000230*           *       ﾃﾞ  ｰ   ﾀ
000240         03  COLUMN  13       VALUE
000250         NX"45504F3F3A5121212176".
000260*           ﾄｳ  ﾛｸ  ｽﾞﾐ     *
000270     02  INV-M01.
000280         03  COLUMN  1        VALUE
000290         NX"21762121255E2539253F2121".
000300*           *       ﾏ   ｽ   ﾀ
000310         03  COLUMN  13       VALUE
000320         NX"4C2445504F3F21212176".
000330*           ﾐ   ﾄｳ  ﾛｸ      *
000340     02  INV-D01.
000350         03  COLUMN  1        VALUE
000360         NX"217621212547213D253F2121".
000370*           *       ﾃﾞ  ｰ   ﾀ
000380         03  COLUMN  13       VALUE
000390         NX"4C2445504F3F21212176".
000400*           ﾐ   ﾄｳ  ﾛｸ      *
000410     02  OK-01.
000420         03  COLUMN   1       VALUE
000430         NX"21762121234F2121234B21212176".
000440*           *       O       K       *
000450     02  CAN-01.
000460         03  COLUMN   1       VALUE
000470         NX"21762121252D25632573".
000480*           *       ｷ   ｬ   ﾝ
000490         03  COLUMN  11       VALUE
000500         NX"253B256B21212176".
000510*           ｾ   ﾙ       *
000520     02  ERR-01.
000530         03  COLUMN  1        VALUE
000540         NX"21762121467E4E4F2528".
000550*           *       ﾆｭｳ ﾘｮｸ ｴ
000560         03  COLUMN  11       VALUE
000570         NX"2569213D21212176".
000580*           ﾗ   ｰ       *
000590     02  ERR-02.
000600         03  COLUMN  1        VALUE
000610         NC"＊　データ　なし　　＊".
000620     02  ERR-DIS.
000630         03  COLUMN 2         VALUE
000640         "<<<  ".
000650         03  COLUMN 7         PIC X(12)   FROM ERR-F.
000660         03  COLUMN 19        PIC X(01)   FROM ERR-M.
000670         03  COLUMN 20        VALUE
000680         "ｴﾗｰ STATUS=".
000690         03  COLUMN 31        PIC X(02)   FROM ERR-FLG.
000700         03  COLUMN 33        VALUE
000710         "  >>>".
000720         03  COLUMN 38        VALUE
000730         " KEY=".
000740         03  COLUMN 43        PIC X(30)   FROM ERR-K.
000750**
