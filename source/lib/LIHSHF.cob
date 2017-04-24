      ****************************************
      *****     履物製品発注ファイル     *****
      *****   (  DATA  HSHFD  768/1  )   *****
      *****   (  KEY1  HSHF1  11-10  )   *****
      *****   (  KEY2  HSHF2   4-16  )   *****
      *****   (  KEY3  HSHF3   1-20  )   *****
      ****************************************
       01  HSHF.
           02  HSHF_PNAME1      PIC  X(005) VALUE "HSHF1".
           02  F                PIC  X(001).
           02  HSHF_PNAME2      PIC  X(005) VALUE "HSHF2".
           02  F                PIC  X(001).
           02  HSHF_PNAME3      PIC  X(005) VALUE "HSHF3".
           02  F                PIC  X(001).
           02  HSHF_LNAME       PIC  X(004) VALUE "HSHF".
           02  F                PIC  X(001).
           02  HSHF_KEY1        PIC  X(100) VALUE SPACE.
           02  HSHF_KEY2        PIC  X(100) VALUE SPACE.
           02  HSHF_KEY3        PIC  X(100) VALUE SPACE.
           02  HSHF_SORT        PIC  X(100) VALUE SPACE.
           02  HSHF_IDLST       PIC  X(100) VALUE SPACE.
           02  HSHF_RES         USAGE  POINTER.
       01  HSH-R.
           02  HSH-KEY3.
             03  HSH-SCD        PIC  9(004).
             03  HSH-KEY2.
               04  HSH-HCD      PIC  9(006).
               04  HSH-KEY      PIC  X(008).
               04  HSH-RNO   REDEFINES HSH-KEY.
                 05  HSH-RSN    PIC  9(002).
                 05  HSH-RNG    PIC  9(004).
                 05  HSH-RND    PIC  9(002).
           02  HSH-HDD          PIC  9(008).
           02  HSH-HDDD  REDEFINES HSH-HDD.
             03  HSH-HNG.
               04  HSH-HNEN     PIC  9(004).
               04  HSH-HNENL REDEFINES HSH-HNEN.
                 05  HSH-HNEN1  PIC  9(002).
                 05  HSH-HNEN2  PIC  9(002).
               04  HSH-HGET     PIC  9(002).
             03  HSH-HPEY       PIC  9(002).
           02  HSH-HDDL  REDEFINES HSH-HDD.
             03  F              PIC  9(002).
             03  HSH-HNGPS      PIC  9(006).
             03  HSH-HNGPL REDEFINES HSH-HNGPS.
               04  F            PIC  9(002).
               04  HSH-HGP      PIC  9(004).
           02  HSH-AHSUD.
             03  HSH-HSUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSH-AHSU  OCCURS  10.
                 05  HSH-HSU    PIC S9(004).                            数量
           02  HSH-ANSUD.
             03  HSH-NSUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSH-ANSU  OCCURS  10.
                 05  HSH-NSU    PIC S9(004).                            数量
           02  HSH-AISUD.
             03  HSH-ISUD  OCCURS   4.                                  ｻｲｽﾞ
               04  HSH-AISU  OCCURS  10.
                 05  HSH-ISU    PIC S9(004).                            数量
           02  HSH-T            PIC  9(005).
           02  HSH-NDD          PIC  9(008).
           02  HSH-NDDD  REDEFINES HSH-NDD.
             03  HSH-NNG.
               04  HSH-NNEN     PIC  9(004).
               04  HSH-NNENL REDEFINES HSH-NNEN.
                 05  HSH-NNEN1  PIC  9(002).
                 05  HSH-NNEN2  PIC  9(002).
               04  HSH-NGET     PIC  9(002).
             03  HSH-NPEY       PIC  9(002).
           02  HSH-NDDL  REDEFINES HSH-NDD.
             03  F              PIC  9(002).
             03  HSH-NNGPS      PIC  9(006).
           02  HSH-ENGP         PIC  9(006).
           02  HSH-ICHK         PIC  9(001).
           02  F                PIC  X(242).
       77  F                    PIC  X(001).
