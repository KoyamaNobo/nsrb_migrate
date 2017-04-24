      ***********************************************
      *****     ê∂ã¶Å@î[ïièëÉèÅ[ÉNÇPÅ`ÇR        *****
      *****     ( SNOWK1, 2, 3 )     256/1      *****
      ***********************************************
       01  SNOWK.
           02  SNOWK_PNAME1   PIC  X(006) VALUE SPACE.
           02  F              PIC  X(001).
           02  SNOWK_LNAME    PIC  X(005) VALUE "SNOWK".
           02  F              PIC  X(001).
           02  SNOWK_KEY1     PIC  X(100) VALUE SPACE.
           02  SNOWK_SORT     PIC  X(100) VALUE SPACE.
           02  SNOWK_IDLST    PIC  X(100) VALUE SPACE.
           02  SNOWK_RES      USAGE  POINTER.
       01  SNO-R.
           02  SNO-01.
             03  SNO-011      PIC  9(04).
             03  SNO-012      PIC  9(03).
           02  SNO-02.
             03  SNO-021      PIC  9(02).
             03  SNO-022      PIC  9(02).
             03  SNO-023      PIC  9(03).
           02  SNO-03         PIC  9(06).
           02  SNO-04         PIC  9(01).
           02  SNO-05.
             03  SNO-051      PIC  9(10).
             03  SNO-052      PIC  9(01).
           02  SNO-06.
             03  SNO-061      PIC  9(04).
             03  SNO-062      PIC  9(02).
             03  SNO-063      PIC  9(02).
           02  SNO-07.
             03  SNO-071      PIC  9(04).
             03  SNO-072      PIC  9(02).
             03  SNO-073      PIC  9(02).
           02  SNO-08.
             03  SNO-081      PIC  9(04).
             03  SNO-082      PIC  9(05).
           02  SNO-09.
             03  SNO-091      PIC  N(14).
             03  SNO-092      PIC  N(10).
           02  SNO-10         PIC  N(04).
           02  SNO-11         PIC  X(13).
           02  SNO-12         PIC S9(04).
           02  SNO-13         PIC  9(05).
           02  SNO-14         PIC S9(09).
           02  SNO-15         PIC  9(05).
           02  SNO-16         PIC S9(09).
           02  SNO-17         PIC  N(23).
           02  SNO-18         PIC  9(01).
           02  SNO-19         PIC  X(09).
           02  F              PIC  X(42).
       77  F                  PIC  X(01).
