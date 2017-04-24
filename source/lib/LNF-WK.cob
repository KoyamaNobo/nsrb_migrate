      ***********************************
      ******    荷札ワーク　　　　　　　*
      ******                SEQ         *
      ******                128/2       *
      ***********************************
       01  NF-WK.
           02  NF-WK_PNAME1            PIC  X(017) VALUE SPACE.
           02  F                       PIC  X(001).
           02  NF-WK_LNAME             PIC  X(005) VALUE "NF-WK".
           02  F                       PIC  X(001).
           02  NF-WK_KEY1              PIC  X(100) VALUE SPACE.
           02  NF-WK_SORT              PIC  X(100) VALUE SPACE.
           02  NF-WK_IDLST             PIC  X(100) VALUE SPACE.
           02  NF-WK_RES               USAGE  POINTER.
      *
       01  NF-R.
           02  NF-R1.
               03   NF1-01                 PIC 9(06).                   送状№
               03   NF1-02.                                             枝番
                    04   NF1-021           PIC 9(02).                   　入力
                    04   NF1-022           PIC 9(03).                   　連番
               03   NF1-03                 PIC 9(01).                   行
               03   NF1-04.                                             発送日
                    04  NF1-041            PIC 9(02).                   　年
                    04  NF1-042            PIC 9(02).                   　月
                    04  NF1-043            PIC 9(02).                   　月
               03   NF1-05                 PIC 9(06).                   品ｺｰﾄﾞ
               03   NF1-06.
                    04  NF1-061            PIC 9(04).                   得意先CD
                    04  NF1-062            PIC 9(03).                   直送先CD
               03   NF1-07                 PIC 9(01).                   運送業者
               03   NF1-08                 PIC 9(01).                   倉 ｺｰﾄﾞ
               03   NF1-09                 PIC S9(03).                  個数
               03   NF1-10.                                             出荷数
                    04 NF1-101             PIC S9(03)   OCCURS  27.     サイズ別
               03   NF1-11                 PIC S9(03).                  枚数
               03   NF1-12                 PIC 9(01).                   一般教育
               03   NF1-13.                                             直送先CD
                    04  NF1-131            PIC 9(04).                   得意先CD
                    04  NF1-132            PIC 9(03).                   直送先CD
           02  NF-R2    REDEFINES  NF-R1.
               03   NF2-01                 PIC 9(06).                   送状№
               03   NF2-02.                                             枝番
                    04   NF2-021           PIC 9(02).                   　入力
                    04   NF2-022           PIC 9(03).                   　連番
               03   NF2-03                 PIC 9(01).                   行
               03   NF2-04                 PIC N(09).                   配達
               03   NF2-05                 PIC N(23).                   摘要
               03   F                      PIC X(44).
               03   NF2-12                 PIC 9(01).                   一般教育
               03   NF2-99.                                             直送先CD
                    04  NF2-991            PIC 9(04).                   得意先CD
                    04  NF2-992            PIC 9(03).                   直送先CD
       77  F                           PIC X(01).
