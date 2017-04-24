      ***********************************************
      *****　　生　協　発　注　デ　ー　タ       *****
      *****         ( SK-HAT )   256/1          *****
      ***********************************************
       01  SK-HAT.
           02  SK-HAT_PNAME1  PIC  X(006) VALUE "HAT-DB".
           02  F              PIC  X(001).
           02  SK-HAT_LNAME   PIC  X(006) VALUE "SK-HAT".
           02  F              PIC  X(001).
           02  SK-HAT_KEY1    PIC  X(100) VALUE SPACE.
           02  SK-HAT_KEY2    PIC  X(100) VALUE SPACE.
           02  SK-HAT_SORT    PIC  X(100) VALUE SPACE.
           02  SK-HAT_IDLST   PIC  X(100) VALUE SPACE.
           02  SK-HAT_RES     USAGE  POINTER.
       01  HAT-R.
           02  HAT-KEY.
             03  HAT-01       PIC  9(06).
             03  HAT-02       PIC  9(01).
           02  HAT-03.
             03  HAT-031      PIC  9(04).
             03  HAT-032      PIC  9(02).
             03  HAT-033      PIC  9(02).
           02  HAT-04.
             03  HAT-041      PIC  9(04).
             03  HAT-042      PIC  9(03).
           02  HAT-05         PIC  9(01).
           02  HAT-06         PIC  9(09).
           02  HAT-07         PIC  9(06).
           02  HAT-08         PIC  9(03).
           02  HAT-09.
             03  HAT-091      PIC  N(14).
             03  HAT-092      PIC  N(10).
           02  HAT-10.
             03  HAT-101      PIC  9(04).
             03  HAT-102      PIC  9(02).
             03  HAT-103      PIC  9(02).
           02  HAT-11         PIC S9(04).
           02  HAT-13         PIC  9(05).
           02  HAT-14         PIC  9(05).
           02  HAT-15         PIC  X(13).
           02  HAT-16         PIC  9(01).
           02  HAT-17         PIC  N(09).
           02  HAT-18.
             03  HAT-181      PIC  9(02).
             03  HAT-182      PIC  9(02).
             03  HAT-183      PIC  9(03).
           02  HAT-19         PIC  N(23).
           02  HAT-23         PIC  9(10).
           02  HAT-25.
             03  HAT-251      PIC  9(04).
             03  HAT-252      PIC  9(02).
             03  HAT-253      PIC  9(02).
           02  HAT-26         PIC  X(09).
           02  F              PIC  X(18).
           02  HAT-27         PIC  9(01).
           02  HAT-24         PIC  9(01).
           02  HAT-20         PIC  9(01).
           02  HAT-21.
             03  HAT-211      PIC  9(04).
             03  HAT-212      PIC  9(02).
             03  HAT-213      PIC  9(02).
           02  HAT-22         PIC  9(01).
           02  HAT-97         PIC  9(01).
           02  HAT-98         PIC  9(01).
           02  HAT-99         PIC  9(01).
       77  F                  PIC  X(01).
