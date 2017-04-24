      ****************************************
      *****      çﬁóøìùåvÅ@ÉèÅ[ÉN        *****
      *****   (  WK0128NNN  128/2  )     *****
      ****************************************
       01  JT-FW.
           02  JT-F_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JT-F_LNAME     PIC  X(005) VALUE "JT-FW".
           02  F              PIC  X(001).
           02  JT-F_KEY1      PIC  X(100) VALUE SPACE.
           02  JT-F_SORT      PIC  X(100) VALUE SPACE.
           02  JT-F_IDLST     PIC  X(100) VALUE SPACE.
           02  JT-F_RES       USAGE  POINTER.
       01  JT-R.
           02  JT-KEY.                                                   ÇjÇdÇx
             03  JT-KEYD.
               04  JT-BC      PIC  9(001).                              Å@Å@ïî 
               04  JT-RC      PIC  9(002).                                  ïi 
             03  JT-JC        PIC  9(003).
           02  JT-JCD  REDEFINES JT-KEY  PIC  9(006).                   çﬁóø∫∞
           02  JT-TSU         PIC S9(007)V9(02).
           02  JT-SSU         PIC S9(007)V9(02).                        ì¸å…êî
           02  JT-SIK         PIC S9(008).
           02  JT-HSU         PIC S9(007)V9(02).                        èoå…êî
           02  JT-ZKS         PIC S9(007)V9(02).                        åJâzêî
           02  JT-ZKK         PIC S9(008).
           02  JT-YC          PIC  9(001).                              ópìrãÊ 
           02  JT-ZC          PIC  9(001).                              ç›å…ãÊ 
           02  JT-SC          PIC  9(001).                              êªïiãÊ 
           02  JT-CSU         PIC S9(007)V9(02).
           02  JT-BKC         PIC  9(002).
           02  JT-BKNO        PIC  9(002).
           02  F              PIC  X(011).
           02  F              PIC  X(043).
       77  F                  PIC  X(001).
