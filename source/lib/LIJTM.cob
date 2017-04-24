      ***************************************
      *****     材料統計　マスター      *****
      *****      (  JTM  85/3  )        *****
      ***************************************
       01  JT-M.
           02  JT-M_PNAME1    PIC  X(003) VALUE "JTM".
           02  F              PIC  X(001).
           02  JT-M_LNAME     PIC  X(004) VALUE "JT-M".
           02  F              PIC  X(001).
           02  JT-M_KEY1      PIC  X(100) VALUE SPACE.
           02  JT-M_SORT      PIC  X(100) VALUE SPACE.
           02  JT-M_IDLST     PIC  X(100) VALUE SPACE.
           02  JT-M_RES       USAGE  POINTER.
       01  JT-R.
           02  JT-KEY.                                                   ＫＥＹ
             03  JT-KEYD.
               04  JT-BC      PIC  9(001).                              　　部
               04  JT-RC      PIC  9(002).                                  品
             03  JT-JC        PIC  9(003).
           02  JT-JCD  REDEFINES JT-KEY  PIC  9(006).                   材料ｺｰ
           02  JT-TSU         PIC S9(007)V9(02).
           02  JT-SSU         PIC S9(007)V9(02).                        入庫数
           02  JT-SIK         PIC S9(008).
           02  JT-HSU         PIC S9(007)V9(02).                        出庫数
           02  JT-ZKS         PIC S9(007)V9(02).                        繰越数
           02  JT-ZKK         PIC S9(008).
           02  JT-YC          PIC  9(001).                              用途区
           02  JT-ZC          PIC  9(001).                              在庫区
           02  JT-SC          PIC  9(001).                              製品区
           02  JT-CSU         PIC S9(007)V9(02).
           02  JT-BKC         PIC  9(002).
           02  JT-BKNO        PIC  9(002).
           02  F              PIC  X(011).
       77  F                  PIC  X(001).
