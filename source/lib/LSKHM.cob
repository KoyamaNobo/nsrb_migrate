      ********************************************
      *****       工品　品名マスター　　　   *****
      *****     (  WK0256***  256/1  )       *****
      ********************************************
       01  KH-M.
           02  KH-M_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KH-M_LNAME     PIC  X(004) VALUE "KH-M".
           02  F              PIC  X(001).
           02  KH-M_KEY1      PIC  X(100) VALUE SPACE.
           02  KH-M_SORT      PIC  X(100) VALUE SPACE.
           02  KH-M_IDLST     PIC  X(100) VALUE SPACE.
           02  KH-M_RES       USAGE  POINTER.
       01  KH-R.
           02  KH-KEY.                                                  ｺｰﾄﾞ
             03  KH-KEY1      PIC  X(002).
             03  KH-KEY2      PIC  9(003).
           02  KH-HCD  REDEFINES KH-KEY  PIC  X(005).
           02  KH-NAME        PIC  X(020).                              ﾋﾝﾒｲ
           02  KH-YC          PIC  9(002).                              ﾖｳﾄｸﾌﾞﾝ
      *    [   ﾀ  ﾝ  ｶ   ]
           02  KH-TGM         PIC  9(004)V9(02).                        ｺﾞﾑﾀﾝｶ
           02  KH-TKN         PIC  9(004)V9(02).                        ｶﾅｸﾞﾀﾝｶ
           02  KH-TSZ         PIC  9(002)V9(02).                        ｾｯﾁｬｸﾀﾝｶ
      *    [   M   M   ]
           02  KH-YGT1        PIC  9(006)V9(02).
           02  KH-YGT2        PIC  9(006)V9(02).
           02  KH-MKR         PIC  9(002)V9(03).                        ｶﾘｭｳMM
      *
           02  KH-KKH         PIC  9(004)V9(02).                        ｶｺｳﾋ
      *
      *    [   ｼ ｱ ｹﾞ ﾀﾞ ｲ   ]
           02  KH-SBB         PIC  9(002)V9(02).                        ﾊﾞﾘﾊﾞﾌ
           02  KH-STS         PIC  9(002)V9(02).                        ﾄｿｳ
           02  KH-SNE         PIC  9(002)V9(02).                        ﾈｼﾞ
           02  KH-SKP         PIC  9(002)V9(02).                        ｺﾝﾎﾟｳ
           02  KH-SKY         PIC  9(002)V9(02).                        ｷｬｯﾌﾟ
           02  KH-SMK         PIC  9(002)V9(02).                        ﾏｰｸ
           02  KH-SPK         PIC  9(002)V9(02).                        ﾊﾟｰｶｰ
           02  KH-SKG         PIC  9(002)V9(02).                        ｹﾞｰｼﾞ
           02  KH-SAN         PIC  9(002)V9(02).                        ｱﾂﾆｭｳ
           02  KH-SET         PIC  9(002)V9(02).                        ------
           02  KH-SST         PIC  9(003)V9(02).                        ｿﾉﾀ
      *
           02  KH-DRH         PIC  9(003)V9(02).                        ﾄﾞｳﾘｮｸﾋ
      *
           02  KH-KPS         PIC  9(003)V9(02).                        ｶﾝﾊﾟﾝｿﾉﾀ
      *
           02  KH-SKH         PIC  9(002)V9(02).                        ｼｮｳｷｬｸﾋ
      *
           02  KH-SHY         PIC  9(002)V9(02).                        ﾎﾖｳﾋﾝ
      *    [   ﾊﾞ  ｲ  ｶ   ]
           02  KH-T1          PIC  9(006)V9(02).                        ﾊﾞｲｶA
           02  KH-T2          PIC  9(006)V9(02).                        ﾊﾞｲｶB
      *
           02  KH-KIS         PIC  9(001).                              ｷｼｭｸﾌﾞﾝ
           02  KH-SYS         PIC  9(003).                              ｼｮｯﾄｽｳ
           02  KH-TRS         PIC  9(002).                              ﾄﾘｺｽｳ
           02  KH-MS          PIC  9(001).                              ﾒﾝｽｳ
           02  KH-KCO         PIC  X(005).                              ｶﾘｭｳｺｰﾄﾞ
           02  KH-USG         PIC  9(004)V9(02).
           02  KH-NC          PIC  9(001).
           02  KH-GT1         PIC  9(006)V9(02).
           02  KH-GT2         PIC  9(006)V9(02).
           02  KH-KNA         PIC  N(024).
      *    [   日　付   ]
           02  KH-DNG         PIC  9(004).
           02  F              PIC  X(006).
           02  KH-ENG         PIC  9(004).
           02  KH-ADD         PIC  9(004).                              登録年月
           02  KH-COD         PIC  9(006).                              修正日
       77  F                  PIC  X(001).
