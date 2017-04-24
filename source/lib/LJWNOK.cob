       01  JWNOK.                                                       É³·ÍÞÂ¼­¯¶ÖÃ²Ü°¸
           02  JWNOK_PNAME1          PIC  X(009) VALUE SPACE.
           02  F                     PIC  X(001).
           02  JWNOK_LNAME           PIC  X(005) VALUE "JWNOK".
           02  F                     PIC  X(001).
           02  JWNOK_KEY1            PIC  X(100) VALUE SPACE.
           02  JWNOK_SORT            PIC  X(100) VALUE SPACE.
           02  JWNOK_IDLST           PIC  X(100) VALUE SPACE.
           02  JWNOK_RES             USAGE  POINTER.
      *
       01  JWNOK-R.
           02   JWNOK-KEY.                                              ¿°Ä  KEY
                03    JWNOK-01.                                         É³·
                      04  JWNOK-011  PIC 9(4).
                      04  JWNOK-012  PIC 9(2).                          Â·
                      04  JWNOK-013  PIC 9(2).                          Ë
                03  JWNOK-02.                                           Á®¿³»·C
                    04  JWNOK-021    PIC 9(4).                          Ä¸²º°ÄÞ
                    04  JWNOK-022    PIC 9(3).                          Á®¸¿³NO
                03  JWNOK-03         PIC 9(6).                          ËÝº°ÄÞ
                03  JWNOK-04         PIC 9(6).                          ¼Þ­Á­³NO
                03  JWNOK-05         PIC 9(1).                          »²½Þ¸ÌÞÝ
           02   JWNOK-06             PIC 9(1).                          ±½Þ¶Ø¸ÌÞ
           02   JWNOK-07.                                               ¼Þ­Á­³
                03  JWNOK-071    OCCURS  10.                            »²½ÞÍÞÂ
                    04  JWNOK-0711   PIC S9(6).
           02   JWNOK-08             PIC 9(4).
           02   JWNOK-09             PIC 9(1).
           02   FILLER               PIC X(162).
       77  F                         PIC X(001).
