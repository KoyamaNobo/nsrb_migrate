      ***********************************************
      *****                                     *****
      **             Ｏ／Ｌ受信ファイル　　　　　　**
      *****         ( JOLJF   )  256/1          *****
      ***********************************************
       01  JOLJF.
           02  JOLJF_PNAME1      PIC  X(005) VALUE "JOLJF".
           02  F                 PIC  X(001).
           02  JOLJF_LNAME       PIC  X(005) VALUE "JOLJF".
           02  F                 PIC  X(001).
           02  JOLJF_KEY1        PIC  X(100) VALUE SPACE.
           02  JOLJF_SORT        PIC  X(100) VALUE SPACE.
           02  JOLJF_IDLST       PIC  X(100) VALUE SPACE.
           02  JOLJF_RES         USAGE  POINTER.
      *----出荷指示トラン        （区分＝１１）
       01  JOLJF11-REC.
           02  JOLJF11-01        PIC 9(02).                             ＲＣ区分
           02  JOLJF11-KEYW.
             03  JOLJF11-02      PIC 9(06).                                ｼｭｯｶｼ
             03  JOLJF11-03      PIC 9(01).                                ｷﾞｮｳ
           02  JOLJF11-04        PIC 9(01).                                ﾃﾞﾝｸ
           02  JOLJF11-05.                                                 ｼｭｯｶﾋ
               03  JOLJF11-051   PIC 9(04).
               03  JOLJF11-052   PIC 9(02).                                ﾂｷ
               03  JOLJF11-053   PIC 9(02).                                ﾋ
           02  JOLJF11-06.                                                 ｼｭｯｶﾋ
               03  JOLJF11-061   PIC 9(04).
               03  JOLJF11-062   PIC 9(02).                                ﾂｷ
               03  JOLJF11-063   PIC 9(02).                                ﾋ
           02  JOLJF11-07.                                                 ﾁｮｸｿｳ
               03  JOLJF11-071   PIC 9(04).                                ﾄｸｲｺｰ
               03  JOLJF11-072   PIC 9(03).                                ﾁｮｸ N
           02  JOLJF11-08        PIC 9(01).                                ｸﾗ ｺｰ
           02  JOLJF11-09.                                                 ｼﾞｭﾁｭ
               03  JOLJF11-091   PIC 9(06).                                ｼﾞｭﾁｭ
               03  JOLJF11-092   PIC 9(01).                                ｷﾞｮｳ
           02  JOLJF11-10        PIC 9(06).                                ﾋﾝｺｰﾄ
           02  JOLJF11-11        PIC 9(01).                                ｻｲｽﾞｸ
           02  JOLJF11-12.                                                 ｼｭｯｶｻ
               03  JOLJF11-121   OCCURS  10.                               ｻｲｽﾞﾍ
                   04  JOLJF11-1211      PIC S9(04).
               03  JOLJF11-122   PIC S9(05).
           02  JOLJF11-13.                                                 ｼｭｯｶｼ
               03  JOLJF11-131   OCCURS  10.                               ｻｲｽﾞﾍ
                   04  JOLJF11-1311      PIC S9(04).
               03  JOLJF11-132   PIC S9(05).
           02  JOLJF11-14        PIC 9(01).                                ｱｽﾞｶﾘ
           02  JOLJF11-15        PIC 9(01).                                運送 
           02  JOLJF11-15A       PIC 9(03).                                セッ 
           02  JOLJF11-15B       PIC 9(06).                                送り 
           02  JOLJF11-15C       PIC 9(02).                                枝番
           02  JOLJF11-15D       PIC N(09).                                配達
           02  JOLJF11-16        PIC N(23).                             摘要
           02  JOLJF11-20        PIC X(10).
           02  JOLJF11-16A       PIC S9(03).                            個数
           02  FILLER            PIC X(24).
           02  JOLJF11-19        PIC X(01).                             処理部署
           02  JOLJF11-168       PIC 9(01).                                A-890
           02  JOLJF11-17        PIC 9(01).                             一般 
           02  JOLJF11-18        PIC 9(01).                             更新ｻ
       77  F                     PIC X(01).
