       01  JT-WK04.
           02  JT-WK04_PNAME1            PIC  X(009) VALUE SPACE.
           02  F                         PIC  X(001).
           02  JT-WK04_LNAME             PIC  X(007) VALUE "JT-WK04".
           02  F                         PIC  X(001).
           02  JT-WK04_KEY1              PIC  X(100) VALUE SPACE.
           02  JT-WK04_SORT              PIC  X(100) VALUE SPACE.
           02  JT-WK04_IDLST             PIC  X(100) VALUE SPACE.
           02  JT-WK04_RES               USAGE  POINTER.
      *
       01  WK04-R.
           02   WK04-01                  PIC 9(1).                      ±½Þ¶Ø
           02   WK04-02.                                                ¼Þ­Á­³ËÞ
                03   WK04-021            PIC 9(4).
                03   WK04-022            PIC 9(2).                      Â·
                03   WK04-023            PIC 9(2).                      Ë
           02   WK04-KEY3.
                03   WK04-03             PIC 9(6).                      ËÝº°ÄÞ
                03   WK04-KEY2.
                    04   WK04-04         PIC 9(4).                      Ä¸²º°ÄÞ
                    04   WK04-05         PIC 9(6).                      ËÝº°ÄÞ
                    04   WK04-06.                                       É³·
                        05   WK04-061    PIC 9(4).
                        05   WK04-062    PIC 9(2).                      Â·
                        05   WK04-063    PIC 9(2).                      Ë
                    04   WK04-KEY1.
                        05   WK04-07     PIC 9(6).                      ¼Þ­Á­³NO
                        05   WK04-08     PIC 9(1).                      ·Þ®³ NO
           02   WK04-09                  PIC 9(1).                      »²½Þ¸ÌÞÝ
           02   WK04-10                  PIC 9(3).                      Á®¸¿³ NO
           02   WK04-11.                                                ¼Þ­Á­³½³
                03   WK04-111            OCCURS  10.                    »²½ÞÍÞÂ
                    04   WK04-1111       PIC S9(6)   COMP-3.
           02   WK04-12.                                                ¼­¯º½³
                03   WK04-121            OCCURS  10.                    »²½ÞÍÞÂ
                    04   WK04-1211       PIC S9(6)   COMP-3.
           02   WK04-14.                                                ¾ÞÝ¹ÞÂÏÂ
                03   WK04-141  OCCURS 10 PIC S9(06)  COMP-3.            »²½ÞÍÞÂ
           02   WK04-15.                                                ¼­¯¶»¼½Þ
                03   WK04-151  OCCURS 10 PIC S9(06)  COMP-3.            »²½ÞÍÞÂ
           02   WK04-16                  PIC S9(03).                    ¾¯Ä½³
           02   WK04-801                 PIC N(06).
           02   WK04-20                  PIC 9(03).                     ÍÝº³NO.
           02   WK04-17                  PIC 9(05).
           02   WK04-22                  PIC X(10).
           02   WK04-802                 PIC N(04).
           02   WK04-802D   REDEFINES  WK04-802.
                03   WK04-803            PIC N(02).
                03   WK04-23             PIC 9(04).
           02   WK04-88                  PIC 9(01).
           02   WK04-89                  PIC 9(01).
           02   WK04-90                  PIC 9(01).
           02   WK04-91                  PIC 9(02).                     ÀÝÄ³
           02   WK04-99.                                                ”NŒŽ“x
               03    WK04-991            PIC 9(04).
               03    WK04-992            PIC 9(02).                     ŒŽ
       77  F                             PIC X(01).
