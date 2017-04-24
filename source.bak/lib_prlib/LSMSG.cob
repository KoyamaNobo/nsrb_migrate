000010**
000020**   MESSEGE  AREA
000030**
000040 01  DISP-ERR-AREA LINE 24.
000050     02  DISP-MSG-01.
000060         03  COLUMN  2        PIC X(50) FROM ERR-MSGX.
000070     02  DISP-MSG-SPACE.
000080         03  COLUMN  2        PIC X(50) FROM ERR-SPACE.
000090     02  DISP-BUZ-B.
000100         03  COLUMN  80       PIC X(05) VALUE ""27"B"10"".
000110     02  DISP-BUZ-J.
000120         03  COLUMN  80       PIC X(05) VALUE ""27"J"01"".
000130     02  NOR-M01.
000140         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000150             COLOR   RED.
000160         03  COLUMN  2        VALUE
000170         NC"＊　マスタ　登録済　＊".
000180     02  NOR-D01.
000190         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000200             COLOR   RED.
000210         03  COLUMN  2        VALUE
000220         NC"＊　データ　登録済　＊".
000230     02  INV-M01.
000240         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000250             COLOR   RED.
000260         03  COLUMN  2        VALUE
000270         NC"＊　マスタ　未登録　＊".
000280     02  INV-D01.
000290         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000300             COLOR   RED.
000310         03  COLUMN  2        VALUE
000320         NC"＊　データ　未登録　＊".
000330     02  OK-01.
000340         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000350             COLOR   WHITE.
000360         03  COLUMN  2        VALUE
000370         NC"＊　Ｏ　Ｋ　＊".
000380     02  CAN-01.
000390         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000400             COLOR   WHITE.
000410         03  COLUMN  2        VALUE
000420         NC"＊　キャンセル　＊".
000430     02  ERR-01.
000440         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000450             COLOR   RED.
000460         03  COLUMN  2        VALUE
000470         NC"＊　入力エラー　＊".
000480     02  INV-MCT.
000490         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000500             COLOR   RED.
000510         03  COLUMN  2      VALUE "＊　コントロールＭ未登録　＊".
000520     02  INV-CON.
000530         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000540             COLOR   YELLOW.
000550         03  COLUMN  2        VALUE
000560         NC"＊　コントロールＦ未登録　処理続行不可　＊".
000570     02  ERR-YMD.
000580         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000590             COLOR   RED.
000600         03  COLUMN  2        VALUE
000610         NC"＊　日付入力エラー　＊".
000620*******
000630     02  ERR-DIS.
000640         03  COLUMN  2        PIC X(50)   FROM ERR-SPACE
000650             COLOR   YELLOW.
000660         03  COLUMN  2        VALUE
000670         "<<<  ".
000680         03  COLUMN  7        PIC X(12)   FROM ERR-F.
000690         03  COLUMN 19        PIC X(01)   FROM ERR-M.
000700         03  COLUMN 20        VALUE
000710         "ｴﾗｰ STATUS=".
000720         03  COLUMN 31        PIC X(02)   FROM ERR-FLG.
000730         03  COLUMN 33        VALUE
000740         "  >>>".
000750         03  COLUMN 38        VALUE
000760         " KEY=".
000770         03  COLUMN 43        PIC X(30)   FROM ERR-K
000780             COLOR  YELLOW.
000790*
