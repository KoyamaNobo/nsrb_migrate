      ***********************************************
      *****                                     *****
      **      oΧΐΡ t@C@@@@@@@@@@**
      *****         ( JSJD    )  256/1          *****
      ***********************************************
       01  JSJD.
           02  JSJD_PNAME1        PIC  X(005) VALUE "JSJD1".
           02  F                  PIC  X(001).
           02  JSJD_PNAME2        PIC  X(005) VALUE "JSJD2".
           02  F                  PIC  X(001).
           02  JSJD_LNAME         PIC  X(004) VALUE "JSJD".
           02  F                  PIC  X(001).
           02  JSJD_KEY1          PIC  X(100) VALUE SPACE.
           02  JSJD_KEY2          PIC  X(100) VALUE SPACE.
           02  JSJD_KEY3          PIC  X(100) VALUE SPACE.
           02  JSJD_KEY4          PIC  X(100) VALUE SPACE.
           02  JSJD_SORT          PIC  X(100) VALUE SPACE.
           02  JSJD_IDLST         PIC  X(100) VALUE SPACE.
           02  JSJD_RES           USAGE  POINTER.
      *
      *
       01  JSJD-REC.
           02  JSJD-KEY.
               03  JSJD-01        PIC 9(01).                            qΊ°Δή
               03  JSJD-02.                                             Όζ
                   04  JSJD-021   PIC 9(04).                            ΎΣζCD
                   04  JSJD-022   PIC 9(03).                            ΌζCD
               03  JSJD-KEY2.
                   04  JSJD-03    PIC 9(06).                            w}
                   04  JSJD-04    PIC 9(01).                            s
           02  JSJD-05            PIC 9(01).                            `ζ
           02  JSJD-06.                                                 w}ϊ
               03  JSJD-061       PIC 9(04).
               03  JSJD-061L  REDEFINES  JSJD-061.
                   04  JSJD-0611  PIC 9(02).
                   04  JSJD-0612  PIC 9(02).
               03  JSJD-062       PIC 9(02).                            @@
               03  JSJD-063       PIC 9(02).                            @@ϊ
           02  JSJD-06L   REDEFINES  JSJD-06.
               03  F              PIC 9(02).
               03  JSJD-06S       PIC 9(06).
           02  JSJD-07.                                                 oΧϊ
               03  JSJD-071       PIC 9(04).
               03  JSJD-071L  REDEFINES  JSJD-071.
                   04  JSJD-0711  PIC 9(02).
                   04  JSJD-0712  PIC 9(02).
               03  JSJD-072       PIC 9(02).                            @@
               03  JSJD-073       PIC 9(02).                            @@ϊ
           02  JSJD-07L   REDEFINES  JSJD-07.
               03  F              PIC 9(02).
               03  JSJD-07S       PIC 9(06).
           02  JSJD-08.                                                 σ
               03  JSJD-081       PIC 9(06).                            σ
               03  JSJD-082       PIC 9(01).                            s
           02  JSJD-09            PIC 9(06).                            iΌΊ°Δή
           02  JSJD-10            PIC 9(01).                            »²½ήζͺ
           02  JSJD-11.                                                 oΧw}
               03  JSJD-111   OCCURS  10.                               »²½ήΚ
                   04  JSJD-1111      PIC S9(04).                       oΧ
               03  JSJD-112       PIC S9(05).
           02  JSJD-12.                                                 oΧΐΡ
               03  JSJD-121   OCCURS  10.                               »²½ήΚ
                   04  JSJD-1211      PIC S9(04).                       ΐΡ
               03  JSJD-122       PIC S9(05).
           02  JSJD-13            PIC 9(01).                            aθζͺ
           02  JSJD-14            PIC 9(01).                            ^Ί°Δή
           02  JSJD-14A           PIC 9(03).                            Zbg
           02  JSJD-14B           PIC 9(06).                            θσ
           02  JSJD-14C           PIC 9(02).                            }Τ
           02  JSJD-14D           PIC N(09).                            zB
           02  JSJD-15            PIC N(23).                            Ev
           02  JSJD-20            PIC X(10).
           02  JSJD-15A           PIC S9(03).                           Β
           02  FILLER             PIC X(26).
           02  JSJD-19            PIC X(01).                            
           02  JSJD-158           PIC 9(01).                            σ»²έ
           02  JSJD-16            PIC 9(01).                            κ^Κζ
           02  JSJD-17            PIC 9(01).                            XV»²έ
       77  F                      PIC  X(001).
