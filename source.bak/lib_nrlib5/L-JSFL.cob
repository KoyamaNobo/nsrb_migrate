000010 FD  JSFL                                                         出荷情報
000020     BLOCK    8     RECORDS                                       問合せ
000030     LABEL    RECORD   STANDARD                                   ﾌｧｲﾙ
000040     VALUE    OF  IDENTIFICATION  "JSFL".
000050*
000060 01  JSFL-R.
000070     02   JSFL-KEY.                                               KEY
000080          03   JSFL-01         PIC 9(04).                         得意ｺｰﾄﾞ
000090          03   JSFL-02         PIC 9(03).                         直送ｺｰﾄﾞ
000100          03   JSFL-03         PIC 9(06).                         品名ｺｰﾄﾞ
000110          03   JSFL-04         PIC 9(06).                         受注ＮＯ
000120     02   JSFL-05              PIC S9(06).                        出荷計　
000130     02   FILLER                PIC X(07).
