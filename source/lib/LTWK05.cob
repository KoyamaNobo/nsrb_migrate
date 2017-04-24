       01  JT-WK05.
           02  JT-WK05_PNAME1         PIC  X(009) VALUE SPACE.
           02  F                      PIC  X(001).
           02  JT-WK05_LNAME          PIC  X(007) VALUE "JT-WK05".
           02  F                      PIC  X(001).
           02  JT-WK05_KEY1           PIC  X(100) VALUE SPACE.
           02  JT-WK05_SORT           PIC  X(100) VALUE SPACE.
           02  JT-WK05_IDLST          PIC  X(100) VALUE SPACE.
           02  JT-WK05_RES            USAGE  POINTER.
      *
       01  WK05-R.
           02   WK05-01               PIC 9(1).                         ｱｽﾞｶﾘｸﾌﾞ
           02   WK05-KEY.
                03   WK05-06.                                           ｼﾞｭﾁｭｳ
                     04  WK05-061     PIC 9(6).                         ｼﾞｭﾁｭｳNO
                     04  WK05-062     PIC 9(1).                         ｷﾞｮｳ
                03   WK05-03.                                           ｼｭｯｶﾋﾞ
                     04  WK05-031     PIC 9(4).
                     04  WK05-032     PIC 9(2).                         ﾂｷ
                     04  WK05-033     PIC 9(2).                         ﾋ
                03   WK05-030   REDEFINES  WK05-03  PIC 9(8).
                03   WK05-02.                                           ｻｼｽﾞ
                     04   WK05-021    PIC 9(6).                         ｼｭｯｶｻｼｽﾞ
                     04   WK05-022    PIC 9(1).                         ｷﾞｮｳ
           02   WK05-04.
                03  WK05-041          PIC 9(4).                         ﾄｸｲｺｰﾄﾞ
                03  WK05-042          PIC 9(3).                         ﾁｮｸｿｳ NO
           02   WK05-07               PIC 9(6).                         ﾋﾝｺｰﾄﾞ
           02   WK05-08               PIC 9(1).                         ｻｲｽﾞｸﾌﾞﾝ
           02   WK05-09.                                                ｼｭｯｶｽｳ
                03  WK05-091    OCCURS  10.                             ｻｲｽﾞﾍﾞﾂ
                    04  WK05-0911     PIC S9(4)   COMP-3.
                03  WK05-092          PIC S9(6)   COMP-3.               ｹｲ
           02   WK05-10               PIC X(1).                         ﾃﾞﾝｸ
           02   FILLER                PIC X(29).
           02   WK05-94               PIC 9(3).                         出荷直送
           02   WK05-95               PIC X(6).                         受注品名
           02   WK05-96.                                                受注日
                03  WK05-961          PIC 9(4).
                03  WK05-961L  REDEFINES  WK05-961.
                    04  WK05-9611     PIC 9(2).
                    04  WK05-9612     PIC 9(2).
                03  WK05-962          PIC 9(2).                         　月　
                03  WK05-963          PIC 9(2).                         　日
           02   WK05-97               PIC 9(5).
           02   WK05-98               PIC S9(3).                        セット数
           02   WK05-99               PIC 9(2).                         月度
       77  F                          PIC X(1).
