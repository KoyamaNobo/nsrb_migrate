       01  JT-KW02.
           02  JT-KW02_PNAME1         PIC  X(007) VALUE "JT-KW02".
           02  F                      PIC  X(001).
           02  JT-KW02_LNAME          PIC  X(007) VALUE "JT-KW02".
           02  F                      PIC  X(001).
           02  JT-KW02_KEY1           PIC  X(100) VALUE SPACE.
           02  JT-KW02_KEY2           PIC  X(100) VALUE SPACE.
           02  JT-KW02_SORT           PIC  X(100) VALUE SPACE.
           02  JT-KW02_IDLST          PIC  X(100) VALUE SPACE.
           02  JT-KW02_RES            USAGE  POINTER.
       01  KW02-R.
           03  KW02-01                PIC  9(01).                       óaÇËãÊï™
           03  KW02-KEY.
               05  KW02-02.
                   07  KW02-021       PIC  9(06).                       óa/éÛ NO
                   07  KW02-022       PIC  9(01).                       óa/éÛ çs
       77  F                          PIC  X(01).
