       01  JT-WK03.                                                     º≠Ø∂ªºΩﬁƒ◊›
           02  JT-WK03_PNAME1        PIC  X(009) VALUE SPACE.
           02  F                     PIC  X(001).
           02  JT-WK03_LNAME         PIC  X(007) VALUE "JT-WK03".
           02  F                     PIC  X(001).
           02  JT-WK03_KEY1          PIC  X(100) VALUE SPACE.
           02  JT-WK03_SORT          PIC  X(100) VALUE SPACE.
           02  JT-WK03_IDLST         PIC  X(100) VALUE SPACE.
           02  JT-WK03_RES           USAGE  POINTER.
      *
       01  W03-R.
           02   W03-KEY.                                                KEY
                03   W03-01          PIC 9(6).                          º≠Ø∂ºΩﬁ
                03   W03-02          PIC 9(1).                          ∑ﬁÆ≥
           02   W03-03               PIC 9(1).                          √ﬁ›∏
           02   W03-04.                                                 º≠Ø∂Àﬁ ª
                03  W03-041          PIC 9(4).
                03  W03-041L  REDEFINES  W03-041.
                    04  W03-0411     PIC 9(2).
                    04  W03-0412     PIC 9(2).
                03  W03-042          PIC 9(2).                          ¬∑
                03  W03-043          PIC 9(2).                          À
           02   W03-05.                                                 º≠Ø∂Àﬁºﬁ
                03  W03-051          PIC 9(4).
                03  W03-051L  REDEFINES  W03-051.
                    04  W03-0511     PIC 9(2).
                    04  W03-0512     PIC 9(2).
                03  W03-052          PIC 9(2).                          ¬∑
                03  W03-053          PIC 9(2).                          À
           02   W03-06.                                                 ¡Æ∏ø≥ CD
                03  W03-061          PIC 9(4).                          ƒ∏≤∫∞ƒﬁ
                03  W03-062          PIC 9(3).                          ¡Æ∏ NO
           02   W03-07               PIC 9(1).                          ∏◊ ∫∞ƒﬁ
           02   W03-08.                                                 ºﬁ≠¡≠≥
                03  W03-081          PIC 9(6).                          ºﬁ≠¡≠≥NO
                03  W03-082          PIC 9(1).                          ∑ﬁÆ≥
           02   W03-09               PIC 9(6).                          À›∫∞ƒﬁ
           02   W03-10               PIC 9(1).                          ª≤Ωﬁ∏Ãﬁ›
           02   W03-11.                                                 º≠Ø∂ªºΩﬁ
                03  W03-111    OCCURS  10.                              ª≤ΩﬁÕﬁ¬
                    04  W03-1111     PIC S9(4).
                03  W03-112          PIC S9(5).
           02   W03-12.                                                 º≠Ø∂ºﬁ¬
                03  W03-121    OCCURS  10.                              ª≤ΩﬁÕﬁ¬
                    04  W03-1211     PIC S9(4).
                03  W03-122          PIC S9(5).
           02  W03-13                PIC 9(1).                          ±Ωﬁ∂ÿ KB
           02  W03-14                PIC 9(1).                          â^ëóÇbÇc
           02  W03-14A               PIC 9(3).                          ÉZÉbÉgêî
           02  W03-14B               PIC 9(6).                          ëóÇËèÛáÇ
           02  W03-14C               PIC 9(2).                          é}î‘
           02  W03-14D               PIC N(9).                          îzíB
           02  W03-15                PIC N(23).                         ìEóv
           02  W03-20                PIC X(10).
           02  W03-15A               PIC S9(03).                        å¬êî
           02  FILLER                PIC X(27).
           02  W03-158               PIC 9(01).
           02  W03-16                PIC 9(01).                         àÍî ã≥àÁ
           02  W03-17                PIC 9(01).                         çXêVª≤›
       77  F                         PIC X(01).
