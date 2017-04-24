       01  WTNAF.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_KEY1     PIC  X(100) VALUE SPACE.
           02  WTNAF_SORT     PIC  X(100) VALUE SPACE.
           02  WTNAF_IDLST    PIC  X(100) VALUE SPACE.
           02  WTNAF_RES      USAGE  POINTER.
       01  WTNA-R.
           02  WTNA-KEY.
             03  WTNA-TEN     PIC  9(004).
           02  WTNA-NAME      PIC  N(026).
           02  WTNA-OSN       PIC  9(001).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
