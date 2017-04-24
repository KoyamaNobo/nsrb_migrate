      ***********************************************
      *****     社　店　マ　ス　タ              *****
      *****     ( STENM-D,K1,K2 )    170/3      *****
      ***********************************************
       01  STENM.
           02  STENM_PNAME1   PIC  X(008) VALUE "STENM-K1".
           02  F              PIC  X(001).
           02  STENM_LNAME    PIC  X(005) VALUE "STENM".
           02  F              PIC  X(001).
           02  STENM_KEY1     PIC  X(100) VALUE SPACE.
           02  STENM_KEY2     PIC  X(100) VALUE SPACE.
           02  STENM_SORT     PIC  X(100) VALUE SPACE.
           02  STENM_IDLST    PIC  X(100) VALUE SPACE.
           02  STENM_RES      USAGE  POINTER.
       01  STE-R.
           02  STE-KEY2.
             03  STE-011      PIC  9(004).
             03  STE-012      PIC  9(003).
             03  STE-KEY1.
               04  STE-02     PIC  9(009).
           02  STE-03         PIC  N(015).
           02  STE-04         PIC  N(015).
           02  STE-05         PIC  9(006).
           02  STE-06         PIC  X(004).
           02  STE-07         PIC  9(002).
           02  STE-08.
             03  STE-081      PIC  9(010).
             03  STE-082      PIC  9(010).
             03  STE-083      PIC  9(010).
           02  STE-09         PIC  9(001).
           02  STE-10         PIC  9(001).
           02  STE-11         PIC  9(001).
           02  STE-12         PIC  9(001).
           02  F              PIC  X(048).
       77  F                  PIC  X(001).
