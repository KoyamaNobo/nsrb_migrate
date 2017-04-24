       01  JNIF.                                                        ÆÌÀÞÄ×Ý
           02  JNIF_PNAME1        PIC  X(008) VALUE "JNIF-RYO".
           02  F                  PIC  X(001).
           02  JNIF_LNAME         PIC  X(004) VALUE "JNIF".
           02  F                  PIC  X(001).
           02  JNIF_KEY1          PIC  X(100) VALUE SPACE.
           02  JNIF_KEY2          PIC  X(100) VALUE SPACE.
           02  JNIF_SORT          PIC  X(100) VALUE SPACE.
           02  JNIF_IDLST         PIC  X(100) VALUE SPACE.
           02  JNIF_RES           USAGE  POINTER.
      *
       01  JNIF-R.
           02  JNIF1-R.
               03   JNIF1-KEY.                                              KEY
                    04    JNIF1-01    PIC 9(6).                             ÃÞÝËß®³N
                    04    JNIF1-02    PIC 9(1).                             ·Þ®³
               03   JNIF1-03          PIC 9(6).                             ËÝº°ÄÞ
               03   JNIF1-04.                                               Ê¯¿³ËÞ
                    04  JNIF1-041     PIC 9(2).                             ÈÝ
                    04  JNIF1-042     PIC 9(2).                             Â·
                    04  JNIF1-043     PIC 9(2).                             Ë
               03   JNIF1-05.                                               Á®¸º°ÄÞ
                    04  JNIF1-051     PIC 9(4).                             Ä¸²º°ÄÞ
                    04  JNIF1-052     PIC 9(3).                             Á®¸ NO
               03   JNIF1-06          PIC 9(1).                             ³Ý¿³
               03   JNIF1-07          PIC 9(1).                             ¿³º°ÄÞ
               03   JNIF1-08          PIC S9(3).                            º½³
               03   JNIF1-09    OCCURS  27.                                 ¼¯¶½³
                    04  JNIF1-091     PIC S9(3).                            »²½ÞÍÞÂ
               03   JNIF1-10          PIC 9(1).                             ²Ý¼Þ»²Ý
               03   JNIF1-11          PIC 9(1).                             Æ­³Ø®¸
               03   JNIF1-12          PIC 9(1).                             ¼­¯¶»²Ý
               03   JNIF1-13          PIC S9(3).                            Ï²½³
               03   JNIF1-13A         PIC 9(1).                             ˆê”Ê‹³ˆç
               03   JNIF1-15          PIC 9(2).
               03   JNIF1-14          PIC 9(6).                             µ¸Ø¼Þ®³
               03   FILLER            PIC X(1).
           02  JNIF2-R    REDEFINES  JNIF1-R.
               03   JNIF2-KEY.                                              KEY
                    04  JNIF2-01      PIC 9(6).                             ÃÞÝËß®³
                    04  JNIF2-02      PIC 9(1).                             ·Þ®³
               03   JNIF2-02A         PIC N(9).                             Ê²ÀÂ
               03   JNIF2-03          PIC N(23).                            Ã·Ö³
               03   FILLER            PIC X(41).
               03   JNIF2-04          PIC 9(1).                             ²Ý¼Þ»²Ý
               03   JNIF2-05          PIC 9(1).                             Æ®³Ø®¸
               03   JNIF2-06          PIC 9(1).                             ¼­¯¶»²Ý
               03   JNIF2-07          PIC S9(3).                            Ï²½³
               03   JNIF2-07A         PIC 9(1).                             ˆê”Ê‹³ˆç
               03   FILLER            PIC X(2).
               03   JNIF2-08          PIC 9(6).                             µ¸Ø¼Þ®³
               03   FILLER            PIC X(1).
       77  F                      PIC  X(001).
