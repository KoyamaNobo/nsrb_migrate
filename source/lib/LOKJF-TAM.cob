      ***********************************************
      *****                                     *****
      **   　　送　り　状　フ　ァ　イ　ル　　　　　**
      *****         ( O K J F  )  64/4          *****
      ***********************************************
       01  OKJF.
           02  OKJF_PNAME1  PIC  X(008) VALUE "OKJF-TAM".
           02  F            PIC  X(001).
           02  OKJF_LNAME   PIC  X(004) VALUE "OKJF".
           02  F            PIC  X(001).
           02  OKJF_KEY1    PIC  X(100) VALUE SPACE.
           02  OKJF_SORT    PIC  X(100) VALUE SPACE.
           02  OKJF_IDLST   PIC  X(100) VALUE SPACE.
           02  OKJF_RES     USAGE  POINTER.
       01  OKJF-R.
           02  OKJF-KEY.                                                KEY
               03  OKJF-01  PIC 9(06).                                  送り状NO
           02  OKJF-02      PIC 9(01).                                  運送業CD
           02  OKJF-03      PIC 9(06).                                  年月日
           02  OKJF-04      PIC 9(01).                                  倉庫　CD
           02  OKJF-05      PIC 9(07).                                  直送先CD
           02  OKJF-06      PIC N(09).                                  配達日
           02  OKJF-07      PIC 9(03).                                  個数
           02  OKJF-08      PIC 9(01).                                  印字 ｻｲﾝ
           02  OKJF-09      PIC 9(01).                                  区分
           02  OKJF-10      PIC 9(01).                                  更新 ｻｲﾝ
           02  OKJF-11      PIC 9(05).
           02  OKJF-12      PIC 9(06).
           02  OKJF-13      PIC 9(01).
           02  F            PIC X(07).
       77  F                PIC X(01).
      *    *******************************
      *    *      印字サイン　 [OKJF-08] *
      *    *    0;  送り状未発行         *
      *    *    1;  送り状発行済　　　   *
      *    *******************************
      *    *    一般／教育区分 [OKJF-09] *
      *    *    0;   一般　　　　　      *
      *    *    1;   教育　　　 　　　   *
      *    *******************************
      *    *    更新済サイン　 [OKJF-10] *
      *    *    0;  未更新　　　　       *
      *    *    1;  更新済　　　　　　   *
      *    *******************************
