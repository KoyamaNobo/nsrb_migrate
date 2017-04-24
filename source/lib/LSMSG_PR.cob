      ******************************************
      *    MESSEGE  AREA                       *
      *                                        *
      * WORKING-STORAGE SECTION  :  LSMSG_PR   *
      * PROCEDURE DIVISION       :  LSMSG_PR_P *
      ******************************************
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  01DISP-MSG-01    PIC X(50).
           02  DISP-MSG-SPACE.
               03  01DISP-MSG-SPACE PIC X(50).
           02  DISP-BUZ-B.
               03  FILLER           PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER           PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  01NOR-M01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "＊　マスタ　登録済　＊".
           02  NOR-D01.
               03  01NOR-D01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "＊　データ　登録済　＊".
           02  INV-M01.
               03  01INV-M01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "＊　マスタ　未登録　＊".
           02  INV-D01.
               03  01INV-D01        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "＊　データ　未登録　＊".
           02  OK-01.
               03  01OK-01          PIC X(50).
      *                   COLOR   WHITE.
               03  FILLER           PIC N(07) VALUE
               "＊　Ｏ　Ｋ　＊".
           02  CAN-01.
               03  01CAN-01         PIC X(50).
      *                   COLOR   WHITE.
               03  FILLER           PIC N(09) VALUE
               "＊　キャンセル　＊".
           02  ERR-01.
               03  01ERR-01         PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(09) VALUE
               "＊　入力エラー　＊".
           02  INV-MCT.
               03  01INV-MCT        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(14) VALUE
                "＊　コントロールＭ未登録　＊".
           02  INV-CON.
               03  01INV-CON        PIC X(50).
      *                   COLOR   YELLOW.
               03  FILLER           PIC N(21) VALUE
               "＊　コントロールＦ未登録　処理続行不可　＊".
           02  ERR-YMD.
               03  01ERR-YMD        PIC X(50).
      *                   COLOR   RED.
               03  FILLER           PIC N(11) VALUE
               "＊　日付入力エラー　＊".
      *******
           02  ERR-DIS.
               03  01ERR-DIS        PIC X(50).
      *                   COLOR   YELLOW.
               03  FILLER           PIC X(05) VALUE
               "<<<  ".
               03  03ERR-DIS        PIC X(12).
               03  04ERR-DIS        PIC X(01).
               03  FILLER           PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  06ERR-DIS        PIC X(02).
               03  FILLER           PIC X(05) VALUE
               "  >>>".
               03  FILLER           PIC X(05) VALUE
               " KEY=".
               03  FILLER           PIC X(30).
      *                   COLOR  YELLOW.
      *
