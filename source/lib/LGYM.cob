      ***  åªóaã‡É}ÉXÉ^     (64/4)
       01  GYM.
           02  GYM_PNAME1              PIC  X(009) VALUE "GEYOKIN-M".
           02  F                       PIC  X(001).
           02  GYM_LNAME               PIC  X(003) VALUE "GYM".
           02  F                       PIC  X(001).
           02  GYM_KEY1                PIC  X(100) VALUE SPACE.
           02  GYM_KEY2                PIC  X(100) VALUE SPACE.
           02  GYM_SORT                PIC  X(100) VALUE SPACE.
           02  GYM_IDLST               PIC  X(100) VALUE SPACE.
           02  GYM_RES                 USAGE  POINTER.
       01  GYM-R.
           02  GYM-KEY.
               03  GYM-01.
                   04  GYM-011         PIC 9(04).                       â»ñ⁄ÇbÇc
                   04  GYM-012         PIC 9(04).
           02  GYM-03                  PIC S9(11).
           02  GYM-04.                                                  ìñì˙î≠ê∂
               03  GYM-041             PIC S9(10).                      éÿï˚
               03  GYM-042             PIC S9(10).                      ë›ï˚
           02  FILLER                  PIC X(25).
       77  F                           PIC X(1).
