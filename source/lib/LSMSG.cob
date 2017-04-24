      *****************************************
      *    MESSEGE  AREA                      *
      *                                       *
      * WORKING-STORAGE SECTION  :  LSMSG     *
      * PROCEDURE DIVISION       :  LSMSG_P   *
      *****************************************
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  01DISP-MSG-01     PIC X(60).
           02  DISP-MSG-SPACE.
               03  01DISP-MSG-SPACE  PIC X(60).
           02  DISP-MSG-SPACES.
               03  01DISP-MSG-SPACES PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER            PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER            PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER            PIC X(22) VALUE
                  "＊　マスタ　登録済　＊".
           02  NOR-D01.
               03  FILLER            PIC X(22) VALUE
                  "＊　デ―タ　登録済　＊".
           02  INV-M01.
               03  FILLER            PIC X(22) VALUE
                  "＊　マスタ　未登録　＊".
           02  INV-D01.
               03  FILLER            PIC X(22) VALUE
                  "＊　デ―タ　未登録　＊".
           02  OK-01.
               03  FILLER            PIC X(14) VALUE
                  "＊　Ｏ　Ｋ　＊".
           02  CAN-01.
               03  FILLER            PIC X(18) VALUE
                  "＊　キャンセル　＊".
           02  ERR-01.
               03  FILLER            PIC X(18) VALUE
                  "＊　入力エラ―　＊".
           02  ERR-02.
               03  FILLER            PIC X(22) VALUE
                  "＊　データ　なし　　＊".
           02  ERR-DIS.
               03  FILLER            PIC X(05) VALUE
               "<<<  ".
               03  02ERR-DIS         PIC X(12).
               03  03ERR-DIS         PIC X(01).
               03  FILLER            PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  05ERR-DIS         PIC X(02).
               03  FILLER            PIC X(05) VALUE
               "  >>>".
               03  FILLER            PIC X(05) VALUE
               " KEY=".
               03  08ERR-DIS         PIC X(30).
      **
