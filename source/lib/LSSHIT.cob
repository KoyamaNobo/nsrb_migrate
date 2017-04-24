      *******************************************
      *****     ¼ Ê × ² Ã ¶Þ À  Ì § ² Ù     *****
      *******************************************
       01  SHIT-F.
           02  SHIT-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHIT-F_LNAME   PIC  X(006) VALUE "SHIT-F".
           02  F              PIC  X(001).
           02  SHIT-F_KEY1    PIC  X(100) VALUE SPACE.
           02  SHIT-F_SORT    PIC  X(100) VALUE SPACE.
           02  SHIT-F_IDLST   PIC  X(100) VALUE SPACE.
           02  SHIT-F_RES     USAGE  POINTER.
       01  SHIT-R.
           02  ST-KEY         PIC  9(004).                              ³¹ÃNO
           02  ST-TSC         PIC  9(002).                              Ã¶ÞÀ¼­Ù²
           02  ST-TSCD REDEFINES ST-TSC.                                Ã¶ÞÀ¼­Ù²
             03  ST-TC1       PIC  9(001).
             03  ST-TC2       PIC  9(001).
           02  ST-SKC         PIC  9(002).                              ¼®Ø¸ÌÞÝ
           02  ST-BCD         PIC  9(004).                              BKº°ÄÞ
           02  ST-FKC         PIC  9(002).                              Ì¹Ýº°ÄÞ
           02  ST-TCD         PIC  9(004).                              ÄØË·º°ÄÞ
           02  ST-KIN         PIC  9(010).                              ·Ý¶Þ¸
           02  ST-FDD         PIC  9(006).
           02  ST-FDDD REDEFINES ST-FDD.                                ÌØÀÞ¼ËÞ
             03  ST-FNG.
               04  ST-FDN     PIC  9(002).
               04  ST-FDG     PIC  9(002).
             03  ST-FDP       PIC  9(002).
           02  ST-MKD         PIC  9(006).
           02  ST-MKDD REDEFINES ST-MKD.                                ÏÝ·ËÞ
             03  ST-MNG.
               04  ST-MKN     PIC  9(002).
               04  ST-MKG     PIC  9(002).
             03  ST-MKP       PIC  9(002).
      *    [   ±²Ã¶Ó¸  ³ÁÜ¹ ·Ý¶Þ¸   ]
           02  ST-UKD.
             03  ST-UK    OCCURS  7  PIC  9(008).
           02  ST-AUK   REDEFINES ST-UKD.
             03  ST-ZR        PIC  9(008).                              »Þ²Ø®³
             03  ST-SS        PIC  9(008).                              ¼²Ú¼®³ËÝ
             03  ST-SB        PIC  9(008).                              ¾ÂËÞ
             03  ST-GC        PIC  9(008).                              ¶Þ²Á­³
             03  ST-SZ        PIC  9(008).                              ¾²¿Þ³¹²Ë
             03  ST-EG        PIC  9(008).                              ´²·Þ®³¹²
             03  ST-ST        PIC  9(008).                              ¿ÉÀ
           02  F              PIC  X(024).
           02  ST-SNF         PIC  9(004).
           02  ST-SNM         PIC  9(004).
       77  F                  PIC  X(001).
