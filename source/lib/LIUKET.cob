      **************************************************
      *****     受　取　手　形　マ　ス　タ　ー     *****
      *****         ( U K E T M )    170/3         *****
      **************************************************
       01  UKET-M.
           02  UKET-M_PNAME1  PIC  X(005) VALUE "UKETM".
           02  F              PIC  X(001).
           02  UKET-M_LNAME   PIC  X(006) VALUE "UKET-M".
           02  F              PIC  X(001).
           02  UKET-M_KEY1    PIC  X(100) VALUE SPACE.
           02  UKET-M_SORT    PIC  X(100) VALUE SPACE.
           02  UKET-M_IDLST   PIC  X(100) VALUE SPACE.
           02  UKET-M_RES     USAGE  POINTER.
       01  UKET-R.
           02  UT-KEY         PIC  X(004).                              ｳｹﾃNO
           02  UT-TSC         PIC  9(002).                              ﾃｶﾞﾀｼｭﾙｲ
           02  UT-TSCD  REDEFINES UT-TSC.                               ﾃｶﾞﾀｼｭﾙｲ
             03  UT-TC1       PIC  9(001).
             03  UT-TC2       PIC  9(001).
           02  UT-SKC         PIC  9(002).                              ｼｮﾘｸﾌﾞﾝ
           02  UT-BCD         PIC  9(004).                              BKｺｰﾄﾞ
           02  UT-TTC         PIC  9(002).                              ﾀﾝﾄｳｺｰﾄﾞ
           02  UT-BC          PIC  9(001).                              ﾌﾞﾓﾝｺｰﾄﾞ
           02  UT-FKC         PIC  9(002).                              ﾌｹﾝｺｰﾄﾞ
           02  UT-TCD         PIC  9(004).                              ﾄﾘﾋｷｺｰﾄﾞ
           02  UT-KIN         PIC  9(010).                              ｷﾝｶﾞｸ
           02  UT-UTD         PIC  9(006).
           02  UT-UTDD  REDEFINES UT-UTD.                               ｳｹﾄﾘﾋﾞ
             03  UT-UNG.
               04  UT-UTN     PIC  9(002).
               04  UT-UTG     PIC  9(002).
             03  UT-UTP       PIC  9(002).
           02  UT-FDD         PIC  9(006).
           02  UT-FDDD  REDEFINES UT-FDD.                               ﾌﾘﾀﾞｼﾋﾞ
             03  UT-FDN       PIC  9(002).
             03  UT-FDG       PIC  9(002).
             03  UT-FDP       PIC  9(002).
           02  UT-HKD         PIC  9(006).
           02  UT-HKDD  REDEFINES UT-HKD.                               ﾋｷｳｹﾋﾞ
             03  UT-HKN       PIC  9(002).
             03  UT-HKG       PIC  9(002).
             03  UT-HKP       PIC  9(002).
           02  UT-MKD         PIC  9(006).
           02  UT-MKDD  REDEFINES UT-MKD.                               ﾏﾝｷﾋﾞ
             03  UT-MNG.
               04  UT-MKN     PIC  9(002).
               04  UT-MKG     PIC  9(002).
             03  UT-MKP       PIC  9(002).
           02  UT-IDD         PIC  9(006).
           02  UT-IDDD  REDEFINES UT-IDD.                               ｲﾄﾞｳﾋﾞ
             03  UT-ING.
               04  UT-IDN     PIC  9(002).
               04  UT-IDG     PIC  9(002).
             03  UT-IDP       PIC  9(002).
           02  UT-SBC         PIC  9(004).                              ｼｮﾘBK
           02  UT-FDM         PIC  N(024).                              ﾌﾘﾀﾞｼﾆﾝ
           02  F              PIC  X(039).
           02  UT-OKD         PIC  9(006).                              ｵﾁｺﾐﾋﾞ
           02  UT-OKDD  REDEFINES UT-OKD.
             03  UT-ONG.
               04  UT-OKN     PIC  9(002).
               04  UT-OKG     PIC  9(002).
             03  UT-OKP       PIC  9(002).
           02  UT-SND.                                                  ｾｲﾚｷ ﾈﾝ
             03  UT-SNU       PIC  9(004).
             03  UT-SNM       PIC  9(004).
             03  UT-SNI       PIC  9(004).
       77  F                  PIC  X(001).
