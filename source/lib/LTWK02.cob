       01  JT-WK02.                                                     ｼﾞｭﾁｭｳﾄﾗﾝ
           02  JT-WK02_PNAME1        PIC  X(009) VALUE SPACE.
           02  F                     PIC  X(001).
           02  JT-WK02_LNAME         PIC  X(007) VALUE "JT-WK02".
           02  F                     PIC  X(001).
           02  JT-WK02_KEY1          PIC  X(100) VALUE SPACE.
           02  JT-WK02_SORT          PIC  X(100) VALUE SPACE.
           02  JT-WK02_IDLST         PIC  X(100) VALUE SPACE.
           02  JT-WK02_RES           USAGE  POINTER.
      *
       01  W02-R.
           02   W02-01               PIC 9(06).                         受注№
           02   W02-02               PIC 9(01).                         行
           02   W02-03               PIC 9(01).                         預り区分
           02   W02-04.                                                 年月日
                03  W02-041          PIC 9(04).
                03  W02-041L  REDEFINES  W02-041.
                    04  W02-0411     PIC 9(02).
                    04  W02-0412     PIC 9(02).
                03  W02-042          PIC 9(02).                         月
                03  W02-043          PIC 9(02).                         日
           02   W02-05.
                03  W02-051          PIC 9(04).                         得意先CD
                03  W02-052          PIC 9(03).                         直送先NO
           02   W02-06               PIC 9(06).                         品名
           02   W02-06A              PIC 9(01).                         ｻｲｽﾞ区分
           02   W02-07.                                                 ｻｲｽﾞ
                03  W02-071.
                    04  W02-0711     PIC S9(06)    OCCURS  10.
           02   W02-08.                                                 納期
                03  W02-081          PIC 9(04).
                03  W02-082          PIC 9(02).                         月
                03  W02-083          PIC 9(02).                         日
           02   W02-08L   REDEFINES  W02-08.
                03  F                PIC 9(02).
                03  W02-08S          PIC 9(06).
           02   F                    PIC X(06).
           02   W02-10               PIC 9(01).                         ACTION
           02   W02-11               PIC N(32).                         摘要
           02   W02-12               PIC 9(03).                         SEQ-NO
           02   FILLER               PIC X(08).
           02   W02-16               PIC S9(03).                        ｾｯﾄ数 　
           02   W02-90               PIC 9(01).
           02   W02-15               PIC 9(05).
           02   W02-17               PIC X(10).
           02   W02-23               PIC 9(04).
           02   FILLER               PIC X(46).
           02   W02-JS               PIC 9(01).
           02   W02-891S.
                03   W02-8911        PIC 9(02).
                03   W02-8912        PIC 9(02).
                03   W02-8913        PIC 9(02).
       77  F                         PIC X(01).
