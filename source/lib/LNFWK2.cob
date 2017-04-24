      ***********************************
      ******    荷札ワーク２　　　　　　*
      ******                ISAM        *
      ******                 42/6       *
      ***********************************
       01  NF-WK2.
           02  NF-WK2_PNAME1           PIC  X(017) VALUE SPACE.
           02  F                       PIC  X(001).
           02  NF-WK2_LNAME            PIC  X(006) VALUE "NF-WK2".
           02  F                       PIC  X(001).
           02  NF-WK2_KEY1             PIC  X(100) VALUE SPACE.
           02  NF-WK2_SORT             PIC  X(100) VALUE SPACE.
           02  NF-WK2_IDLST            PIC  X(100) VALUE SPACE.
           02  NF-WK2_RES              USAGE  POINTER.
      *
       01  WK2-R.
           02   WK2-KEY.                                                KEY
                03   WK2-01            PIC 9(06).                       送状№
                03   WK2-02.                                            枝番
                     04   WK2-021      PIC 9(02).                       　入力
                     04   WK2-022      PIC 9(03).                       　連番
                03   WK2-03            PIC 9(01).                       一足箱KB
                03   WK2-04            PIC 9(06).                       品名ｺｰﾄﾞ
                03   WK2-05            PIC 9(03).                       ｻｲｽﾞｺｰﾄﾞ
           02   WK2-06                 PIC 9(03).                       セット数
           02   WK2-07                 PIC 9(06).                       指図伝NO
           02   WK2-08                 PIC 9(03).                       ｶｰﾄﾝ入数
           02   WK2-09                 PIC S9(04).                      残数
           02   F                      PIC X(04).
           02   WK2-70                 PIC 9(01).
       77  F                           PIC X(01).
