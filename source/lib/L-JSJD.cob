      ***********************************************
      *****                                     *****
      **      出荷実績 ファイル　　　　　　　　　　**
      *****         ( JSJD    )  256/1          *****
      ***********************************************
       01  JSJD.
           02  JSJD_PNAME1        PIC  X(005) VALUE "JSJD1".
           02  F                  PIC  X(001).
           02  JSJD_PNAME2        PIC  X(005) VALUE "JSJD2".
           02  F                  PIC  X(001).
           02  JSJD_LNAME         PIC  X(004) VALUE "JSJD".
           02  F                  PIC  X(001).
           02  JSJD_KEY1          PIC  X(100) VALUE SPACE.
           02  JSJD_KEY2          PIC  X(100) VALUE SPACE.
           02  JSJD_KEY3          PIC  X(100) VALUE SPACE.
           02  JSJD_KEY4          PIC  X(100) VALUE SPACE.
           02  JSJD_SORT          PIC  X(100) VALUE SPACE.
           02  JSJD_IDLST         PIC  X(100) VALUE SPACE.
           02  JSJD_RES           USAGE  POINTER.
      *
      *
       01  JSJD-REC.
           02  JSJD-KEY.
               03  JSJD-01        PIC 9(01).                            倉ｺｰﾄﾞ
               03  JSJD-02.                                             直送先
                   04  JSJD-021   PIC 9(04).                            得意先CD
                   04  JSJD-022   PIC 9(03).                            直送先CD
               03  JSJD-KEY2.
                   04  JSJD-03    PIC 9(06).                            指図№
                   04  JSJD-04    PIC 9(01).                            行№
           02  JSJD-05            PIC 9(01).                            伝区
           02  JSJD-06.                                                 指図日
               03  JSJD-061       PIC 9(04).
               03  JSJD-061L  REDEFINES  JSJD-061.
                   04  JSJD-0611  PIC 9(02).
                   04  JSJD-0612  PIC 9(02).
               03  JSJD-062       PIC 9(02).                            　　月
               03  JSJD-063       PIC 9(02).                            　　日
           02  JSJD-06L   REDEFINES  JSJD-06.
               03  F              PIC 9(02).
               03  JSJD-06S       PIC 9(06).
           02  JSJD-07.                                                 出荷日
               03  JSJD-071       PIC 9(04).
               03  JSJD-071L  REDEFINES  JSJD-071.
                   04  JSJD-0711  PIC 9(02).
                   04  JSJD-0712  PIC 9(02).
               03  JSJD-072       PIC 9(02).                            　　月
               03  JSJD-073       PIC 9(02).                            　　日
           02  JSJD-07L   REDEFINES  JSJD-07.
               03  F              PIC 9(02).
               03  JSJD-07S       PIC 9(06).
           02  JSJD-08.                                                 受注
               03  JSJD-081       PIC 9(06).                            受注№
               03  JSJD-082       PIC 9(01).                            行№
           02  JSJD-09            PIC 9(06).                            品名ｺｰﾄﾞ
           02  JSJD-10            PIC 9(01).                            ｻｲｽﾞ区分
           02  JSJD-11.                                                 出荷指図
               03  JSJD-111   OCCURS  10.                               ｻｲｽﾞ別数
                   04  JSJD-1111      PIC S9(04).                       出荷数
               03  JSJD-112       PIC S9(05).
           02  JSJD-12.                                                 出荷実績
               03  JSJD-121   OCCURS  10.                               ｻｲｽﾞ別数
                   04  JSJD-1211      PIC S9(04).                       実績数
               03  JSJD-122       PIC S9(05).
           02  JSJD-13            PIC 9(01).                            預り区分
           02  JSJD-14            PIC 9(01).                            運送ｺｰﾄﾞ
           02  JSJD-14A           PIC 9(03).                            セット数
           02  JSJD-14B           PIC 9(06).                            送り状№
           02  JSJD-14C           PIC 9(02).                            枝番
           02  JSJD-14D           PIC N(09).                            配達
           02  JSJD-15            PIC N(23).                            摘要
           02  JSJD-20            PIC X(10).
           02  JSJD-15A           PIC S9(03).                           個数
           02  FILLER             PIC X(26).
           02  JSJD-19            PIC X(01).                            処理部署
           02  JSJD-158           PIC 9(01).                            印字ｻｲﾝ
           02  JSJD-16            PIC 9(01).                            一／般区
           02  JSJD-17            PIC 9(01).                            更新ｻｲﾝ
       77  F                      PIC  X(001).
