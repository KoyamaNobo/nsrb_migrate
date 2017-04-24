       01  W-DAICHO.
           02  W-DAI.
             03  W-DAI1.
               04  W-DAID     PIC  X(001)  OCCURS   3.
             03  W-DAI2       PIC  9(006).
             03  W-DAI3       PIC  9(001).
           02  W-ATBLD.
             03  W-TBLD       PIC  X(027) VALUE
                  " ABCDEFGHIJKLMNOPQRSTUVWXYZ".
             03  W-ATBL  REDEFINES W-TBLD.
               04  W-TBLA     PIC  X(001) OCCURS  27.
           02  W-DNOC         PIC  9(001).
           02  W-DNOT         PIC  9(002).
