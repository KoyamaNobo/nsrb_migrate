      *****************************************
      *    MESSEGE  AREA                      *
      *                                       *
      * WORKING-STORAGE SECTION  :  LSERR     *
      * PROCEDURE DIVISION       :  LSERR_P   *
      *****************************************
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  01DISP-MSG-01    PIC X(60).
           02  DISP-MSG-SPACE.
               03  01DISP-MSG-SPACE PIC X(60).
           02  DISP-BUZ-B.
               03  FILLER           PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER           PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER           PIC N(06) VALUE
                 "＊　マスタ　".
      *           *       ﾏ   ｽ   ﾀ
               03  FILLER           PIC N(05) VALUE
                 "登録済　＊".
      *           ﾄｳ  ﾛｸ  ｽﾞﾐ     *
           02  NOR-D01.
               03  FILLER           PIC N(06) VALUE
                 "＊　デ―タ　".
      *           *       ﾃﾞ  ｰ   ﾀ
               03  FILLER           PIC N(05) VALUE
                 "登録済　＊".
      *           ﾄｳ  ﾛｸ  ｽﾞﾐ     *
           02  INV-M01.
               03  FILLER           PIC N(06) VALUE
                 "＊　マスタ　".
      *           *       ﾏ   ｽ   ﾀ
               03  FILLER           PIC N(05) VALUE
                 "未登録　＊".
      *           ﾐ   ﾄｳ  ﾛｸ      *
           02  INV-D01.
               03  FILLER           PIC N(06) VALUE
                 "＊　デ―タ　".
      *           *       ﾃﾞ  ｰ   ﾀ
               03  FILLER           PIC N(05) VALUE
                 "未登録　＊".
      *           ﾐ   ﾄｳ  ﾛｸ      *
           02  OK-01.
               03  FILLER           PIC N(07) VALUE
                 "＊　Ｏ　Ｋ　＊".
      *           *       O       K       *
           02  CAN-01.
               03  FILLER           PIC N(05) VALUE
                 "＊　キャン".
      *           *       ｷ   ｬ   ﾝ
               03  FILLER           PIC N(04) VALUE
                 "セル　＊".
      *           ｾ   ﾙ       *
           02  ERR-01.
               03  FILLER           PIC N(05) VALUE
                 "＊　入力エ".
      *           *       ﾆｭｳ ﾘｮｸ ｴ
               03  FILLER           PIC N(04) VALUE
                 "ラ―　＊".
      *           ﾗ   ｰ       *
           02  ERR-02.
               03  FILLER           PIC N(11) VALUE
                 "＊　データ　なし　　＊".
           02  ERR-DIS.
               03  FILLER           PIC X(05) VALUE
               "<<<  ".
               03  02ERR-DIS        PIC X(12).
               03  03ERR-DIS        PIC X(01).
               03  FILLER           PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  05ERR-DIS        PIC X(02).
               03  FILLER           PIC X(05) VALUE
               "  >>>".
               03  FILLER           PIC X(05) VALUE
               " KEY=".
               03  08ERR-DIS        PIC X(30).
      **
