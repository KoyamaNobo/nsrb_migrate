       01  JCON.                                                        ∫›ƒ€∞ŸF
           02  JCON_PNAME1             PIC  X(004) VALUE "JCON".
           02  F                       PIC  X(001).
           02  JCON_LNAME              PIC  X(004) VALUE "JCON".
           02  F                       PIC  X(001).
           02  JCON_KEY1               PIC  X(100) VALUE SPACE.
           02  JCON_KEY2               PIC  X(100) VALUE SPACE.
           02  JCON_SORT               PIC  X(100) VALUE SPACE.
           02  JCON_IDLST              PIC  X(100) VALUE SPACE.
           02  JCON_RES                USAGE  POINTER.
      *
       01  JCON-R.
           02  JCON1-R.
               03   JCON1-KEY.                                          KEY
                    04    JCON1-01         PIC 9(1).                    ID
                    04    JCON1-02         PIC 9(1).                    √ﬁ›∏ NO
               03   JCON1-03               PIC 9(6).                    ∑Æ≥≤∏
               03   JCON1-04               PIC 9(6).                    ‹∞∏
               03   JCON1-05               PIC 9(6).
               03   JCON1-06               PIC 9(6).
               03   FILLER                 PIC X(6).
           02  JCON2-R    REDEFINES  JCON1-R.
               03   JCON2-KEY.                                          KEY
                    04    JCON2-01         PIC 9(1).                    ID
                    04    JCON2-02         PIC 9(1).                    ≥›ø≥∫∞ƒﬁ
               03   JCON2-03               PIC N(6).                    ≥›ø≥“≤
               03   FILLER                 PIC X(18).
           02  JCON3-R    REDEFINES  JCON1-R.
               03   JCON3-KEY.                                          KEY
                    04    JCON3-01         PIC 9(1).                    ID
                    04    JCON3-02         PIC 9(1).                    ø≥∫∏Ãﬁ›
               03   JCON3-03               PIC N(6).                    ø≥∫“≤
               03   JCON3-04               PIC 9(1).                    ∂›ÿø≥∫KB
               03   FILLER                 PIC X(17).
      *    
           02  JCON4-R    REDEFINES  JCON1-R.
               03   JCON4-KEY.                                          KEY
                    04    JCON4-01         PIC 9(1).                    ID "4"
                    04    JCON4-02         PIC 9(1).                    √ﬁ›ø≥ª∑K
               03   JCON4-03               PIC N(6).                    √ﬁ›ø≥ª∑N
               03   JCON4-04               PIC X(13).                   TEL.NO
               03   FILLER                 PIC X(05).
      *    
           02  JCON5-R    REDEFINES  JCON1-R.
               03   JCON5-KEY.                                          KEY
                    04    JCON5-01         PIC 9(1).                    ID "5"
                    04    JCON5-02         PIC 9(1).                    √ﬁ›ÀﬂÆ≥K
               03   JCON5-03               PIC 9(6).
               03   FILLER                 PIC X(24).
      *    
           02  JCON6-R    REDEFINES  JCON1-R.
               03   JCON6-KEY.                                          KEY
                    04    JCON6-01         PIC 9(1).                    ID "6"
                    04    JCON6-02         PIC X(1).                    Å¢
               03   JCON6-03               PIC 9(6).                    ºÆÿÀ¬ﬁπ
               03   JCON6-03D  REDEFINES  JCON6-03.
                    04   JCON6-031         PIC 9(4).                    »›
                    04   JCON6-031D  REDEFINES  JCON6-031.
                         05   JCON6-0311   PIC 9(2).
                         05   JCON6-0312   PIC 9(2).
                    04   JCON6-032         PIC 9(2).                    ¬∑
               03   JCON6-05               PIC 9(1).
               03   JCON6-05.
                    04   JCON6-051         PIC 9(1).
                    04   JCON6-052         PIC 9(1).
                    04   JCON6-053         PIC 9(1).
                    04   JCON6-054         PIC 9(1).
               03   JCON6-06               PIC 9(1).
               03   FILLER                 PIC X(09).
               03   JCON6-08               PIC 9(1).
               03   JCON6-09               PIC 9(8).                    ∆ØŒﬂ≥À¬ﬁ
               03   JCON6-09D  REDEFINES  JCON6-09.
                    04   JCON6-091         PIC 9(4).                    »›
                    04   JCON6-091D  REDEFINES  JCON6-091.
                         05   JCON6-0911   PIC 9(2).
                         05   JCON6-0912   PIC 9(2).
                    04   JCON6-092         PIC 9(2).                    ¬∑
                    04   JCON6-093         PIC 9(2).                    À
           02  JCON7-R    REDEFINES  JCON1-R.
               03   JCON7-KEY.                                          KEY
                    04    JCON7-01         PIC 9(1).                    ID
                    04    JCON7-02         PIC 9(1).                    √ﬁ›∏ NO
               03   JCON7-05               PIC 9(6).
               03   JCON7-06               PIC 9(1).
               03   JCON7-07               PIC 9(6).
               03   JCON7-08               PIC 9(1).
               03   JCON7-09               PIC 9(6).
               03   JCON7-10               PIC 9(1).
               03   JCON7-11               PIC 9(6).
               03   JCON7-12               PIC 9(1).
               03   FILLER                 PIC X(02).
      *    
           02  JCON8-R    REDEFINES  JCON1-R.
               03   JCON8-KEY.                                          KEY
                    04    JCON8-01         PIC 9(1).                    ID "8"
                    04    JCON8-02         PIC X(1).                    Å¢
               03   JCON8-04               PIC 9(1).
               03   JCON8-05.
                    04   JCON8-051         PIC 9(1).
                    04   JCON8-052         PIC 9(1).
                    04   JCON8-053         PIC 9(1).
                    04   JCON8-054         PIC 9(1).
               03   JCON8-06               PIC 9(1).
               03   FILLER                 PIC X(24).
       77  F                           PIC  X(001).
