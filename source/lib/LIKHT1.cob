      *******************************************
      *****     工品　品名統計マスター      *****
      *****  (  KHTD : KEY=KHTM1  170/3  )  *****
      *******************************************
       01  KHT-M.
           02  KHT-M_PNAME1   PIC  X(005) VALUE "KHTM1".
           02  F              PIC  X(001).
           02  KHT-M_LNAME    PIC  X(005) VALUE "KHT-M".
           02  F              PIC  X(001).
           02  KHT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  KHT-M_SORT     PIC  X(100) VALUE SPACE.
           02  KHT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  KHT-M_RES      USAGE  POINTER.
       01  KHT-R.
           02  KHT-KEYD.                                                ｺｰﾄﾞ
             03  KHT-YC       PIC  9(002).                              ﾖｳﾄｸﾌﾞﾝ
             03  KHT-NC       PIC  9(001).
             03  KHT-KEY.                                               ｺｰﾄﾞ
               04  KHT-KEY1   PIC  X(002).
               04  KHT-KEY2   PIC  9(003).
      *    [   ﾄ ｳ ｹﾞ ﾂ   ｼﾞ ｯ ｾ ｷ   ]
           02  KHT-KSU        PIC S9(006)V9(02).                        ｶﾘｭｳｽｳ
           02  KHT-HSU        PIC S9(006)V9(02).                        ﾊｲｷｬｸｽｳ
           02  KHT-ISU        PIC S9(006)V9(02).
           02  KHT-KKIN       PIC S9(008).                              ｾｲｻﾝｶﾞｸ
           02  KHT-SSU        PIC S9(006)V9(02).                        ｼｭｯｶｽｳ
           02  KHT-UKIN       PIC S9(008).                              ｳﾘｱｹﾞｶﾞｸ
           02  KHT-NKIN       PIC S9(007).                              ﾈﾋﾞｷｶﾞｸ
           02  KHT-GKIN       PIC S9(008).
           02  KHT-ZSU        PIC S9(006)V9(02).                        ｾﾞﾝｸﾘｽｳ
           02  KHT-ZKIN       PIC S9(008).                              ｾﾞﾝｸﾘｶﾞｸ
      *    [   ｱｽﾞｶﾘ ﾄｳｹｲ   ]
           02  KHT-AZS        PIC S9(006).                              ｾﾞﾝｸﾘｽｳ
           02  KHT-AAS        PIC S9(006).                              ｱｽﾞｶﾘｽｳ
           02  KHT-AUS        PIC S9(006).                              ｳﾘｱｹﾞｽｳ
           02  KHT-ASS        PIC S9(006).                              ｼｭｯｶｽｳ
           02  KHT-AC         PIC  9(001).                              ｸﾌﾞﾝ
           02  F              PIC  X(012).
      *    [   ｸﾌﾞﾝ･ｺｰﾄﾞ  ]
           02  KHT-KIS        PIC  9(001).                              ｷｼｭｸﾌﾞﾝ
           02  KHT-KCO        PIC  X(005).                              ｶﾘｭｳｺｰﾄﾞ
      *    [   ﾀﾅｵﾛｼ   ]
           02  KHT-JTS        PIC S9(006)V9(02).                        ｼﾞﾂﾀﾅ
           02  KHT-TTS        PIC S9(006)V9(02).                        ﾁｮｳﾎﾞﾀﾅ
           02  KHT-HKS        PIC S9(006)V9(02).
      *
           02  F              PIC  X(016).
       77  F                  PIC  X(001).
