000010**
000020**   MESSEGE  AREA                                                61.04.23
000030**
000040 01  DISP-ERR-AREA LINE 24.
000050     02  DISP-MSG-01.
000060         03  COLUMN   1       PIC X(60) FROM ERR-MSGX.
000070     02  DISP-MSG-SPACE.
000080         03  COLUMN   1       PIC X(60) FROM ERR-SPACE.
000090     02  DISP-MSG-SPACES.                                         I.980122
000100         03  COLUMN   1       PIC X(40) FROM ERR-SPACE.           I.980122
000110     02  DISP-BUZ-B.
000120         03  COLUMN  80       PIC X(05) VALUE ""27"B"10"".
000130     02  DISP-BUZ-J.
000140         03  COLUMN  80       PIC X(05) VALUE ""27"J"01"".
000150     02  NOR-M01.
000160         03  COLUMN   1       VALUE   NC"＊　マスタ　登録済　＊".
000170     02  NOR-D01.
000180         03  COLUMN   1       VALUE   NC"＊　デ―タ　登録済　＊".
000190     02  INV-M01.
000200         03  COLUMN   1       VALUE   NC"＊　マスタ　未登録　＊".
000210     02  INV-D01.
000220         03  COLUMN   1       VALUE   NC"＊　デ―タ　未登録　＊".
000230     02  OK-01.
000240         03  COLUMN   1       VALUE   NC"＊　Ｏ　Ｋ　＊".
000250     02  CAN-01.
000260         03  COLUMN   1       VALUE   NC"＊　キャンセル　＊".
000270     02  ERR-01.
000280         03  COLUMN   1       VALUE   NC"＊　入力エラ―　＊".
000290     02  ERR-02.
000300         03  COLUMN   1       VALUE   NC"＊　データ　なし　　＊".
000310     02  ERR-DIS.
000320         03  COLUMN 2         VALUE
000330         "<<<  ".
000340         03  COLUMN 7         PIC X(12)   FROM ERR-F.
000350         03  COLUMN 19        PIC X(01)   FROM ERR-M.
000360         03  COLUMN 20        VALUE
000370         "ｴﾗｰ STATUS=".
000380         03  COLUMN 31        PIC X(02)   FROM ERR-FLG.
000390         03  COLUMN 33        VALUE
000400         "  >>>".
000410         03  COLUMN 38        VALUE
000420         " KEY=".
000430         03  COLUMN 43        PIC X(30)   FROM ERR-K.
000440**
