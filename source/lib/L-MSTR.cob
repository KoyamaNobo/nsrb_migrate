      ***********************************************
      *****                                     *****
      **   　　マスタ更新　トラン　　　　　　　　　**
      *****         ( MSTRN    )  21/12         *****
      ***********************************************
       01  MSTRN.
           02  MSTRN_PNAME1        PIC  X(005) VALUE "MSTRN".
           02  F                   PIC  X(001).
           02  MSTRN_LNAME         PIC  X(005) VALUE "MSTRN".
           02  F                   PIC  X(001).
           02  MSTRN_KEY1          PIC  X(100) VALUE SPACE.
           02  MSTRN_SORT          PIC  X(100) VALUE SPACE.
           02  MSTRN_IDLST         PIC  X(100) VALUE SPACE.
           02  MSTRN_RES           USAGE  POINTER.
      *
       01  MS-REC.
      *----直送先用
           02  MS1-REC.
               03  MS1-KEY.                                                 KEY
                   04  MS1-011         PIC X(01).                           ID
                   04  MS1-012         PIC 9(07).                           直送先
                   04  MS1-012R        REDEFINES  MS1-012.
                       05  MS1-0121    PIC 9(04).                             得意先
                       05  MS1-0122    PIC 9(03).                             直送先
               03  MS1-02.                                                  更新BIT
                   04  MS1-021         PIC X(01).                             1:玉島
                   04  MS1-022         PIC X(01).                             2:未
                   04  MS1-023         PIC X(01).                             3:両備
                   04  MS1-024         PIC X(01).                             4:未
               03  F                   PIC X(02).
               03  MS1-10              PIC 9(01).                           ACT
               03  MS1-11              PIC 9(06).                           更新日
               03  MS1-11R             REDEFINES  MS1-11.
                   04  MS1-111         PIC 9(02).                             年
                   04  MS1-112         PIC 9(02).                             月
                   04  MS1-113         PIC 9(02).                             日
      *    
      *----出荷品名用
           02  MS2-REC    REDEFINES  MS1-REC.
               03  MS2-KEY.                                                 KEY
                   04  MS2-011         PIC X(01).                           ID
                   04  MS2-012         PIC 9(06).                           品名ＣＤ
                   04  F               PIC X(01).
               03  MS2-02.                                                  更新BIT
                   04  MS2-021         PIC X(01).                             1:玉島
                   04  MS2-022         PIC X(01).                             2:未
                   04  MS2-023         PIC X(01).                             3:両備
                   04  MS2-024         PIC X(01).                             4:未
               03  F                   PIC X(02).
               03  MS2-10              PIC 9(01).                           ACT
               03  MS2-11              PIC 9(06).                           更新日
               03  MS2-11R             REDEFINES  MS2-11.
                   04  MS2-111         PIC 9(02).                             年
                   04  MS2-112         PIC 9(02).                             月
                   04  MS2-113         PIC 9(02).                             日
      *    
      *----ワークマン店名（玉島のみ）
           02  MS3-REC    REDEFINES  MS1-REC.
               03  MS3-KEY.                                                 KEY
                   04  MS3-011         PIC X(01).                           ID
                   04  MS3-012         PIC 9(03).                           店番
                   04  F               PIC X(04).
               03  MS3-02.                                                  更新BIT
                   04  MS3-021         PIC X(01).                             1:玉島
                   04  MS3-022         PIC X(01).                             2:未
                   04  MS3-023         PIC X(01).                             3:両備
                   04  MS3-024         PIC X(01).                             4:未
               03  F                   PIC X(02).
               03  MS3-10              PIC 9(01).                           ACT
               03  MS3-11              PIC 9(06).                           更新日
               03  MS3-11R             REDEFINES  MS3-11.
                   04  MS3-111         PIC 9(02).                             年
                   04  MS3-112         PIC 9(02).                             月
                   04  MS3-113         PIC 9(02).                             日
       77  F                       PIC  X(001).
