       01  NJZAIW.                                                      ¸×ÍÞÂ
           02  NJZAIW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  NJZAIW_LNAME     PIC  X(006) VALUE "NJZAIW".
           02  F                PIC  X(001).
           02  NJZAIW_KEY1      PIC  X(100) VALUE SPACE.
           02  NJZAIW_SORT      PIC  X(100) VALUE SPACE.
           02  NJZAIW_IDLST     PIC  X(100) VALUE SPACE.
           02  NJZAIW_RES       USAGE  POINTER.
      *
       01  NJZAIW-R.
           02  NJZAIW-KEY.                                              KEY
                03  NJZAIW-01   PIC 9(1).                               ¸× º°ÄÞ
                03  NJZAIW-02   PIC 9(6).                               ËÝº°ÄÞ
                03  NJZAIW-03   PIC 9(1).                               »²½Þ¸ÌÞÝ
           02  NJZAIW-04.
                03  NJZAI-041     OCCURS  10.                           »²½ÞÍÞÂ
                    04  NJZAIW-0411   PIC S9(6)     COMP-3.
           02  NJZAIW-05.
                03  NJZAIW-051     OCCURS  10.                          »²½ÞÍÞÂ
                    04  NJZAIW-0511   PIC S9(6)     COMP-3.
           02  NJZAIW-06.
                03  NJZAIW-061     OCCURS  10.                          »²½ÞÍÞÂ
                    04  NJZAIW-0611   PIC S9(6)     COMP-3.
           02  NJZAIW-07.
                03  NJZAIW-071     OCCURS  10.                          »²½ÞÍÞÂ
                    04  NJZAIW-0711   PIC S9(6)     COMP-3.
           02  NJZAIW-08.
                03  NJZAIW-081     OCCURS  10.                          »²½ÞÍÞÂ
                    04  NJZAIW-0811   PIC S9(6)     COMP-3.
           02  NJZAIW-09.
                03  NJZAIW-091     OCCURS  10.
                    04  NJZAIW-0911   PIC S9(6)     COMP-3.
           02  NJZAIW-10.
                03  NJZAIW-101     OCCURS  10.
                    04  NJZAIW-1011   PIC S9(6)     COMP-3.
           02  NJZAIW-11.
                03  NJZAIW-111     OCCURS  10.
                    04  NJZAIW-1111   PIC S9(6)     COMP-3.
           02   FILLER                PIC X(184).
       77  F                          PIC  X(001).
