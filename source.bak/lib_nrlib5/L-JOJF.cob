000010***********************************************
000020*****                                     *****
000030**   　　Ｏ／Ｌ状況　ファイル　　　　　　　　**
000040*****         ( J O J F )  512/1          *****
000050***********************************************
000060 FD  JOJF
000070     BLOCK 1 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "JOJF".
000100 01  JOJF-REC.
000110     02  JOJF-KEY.                                                KEY
000120         03  JOJF-01       PIC 9(04).                             SEQ.NO
000130     02  JOJF-02           PIC 9(04).                             日付
000140     02  JOJF-02R          REDEFINES  JOJF-02.
000150         03  JOJF-021      PIC 9(02).                               年
000160         03  JOJF-022      PIC 9(02).                               月
000170     02  JOJF-03           PIC 9(04).                             開始時間
000180     02  JOJF-03R          REDEFINES  JOJF-03.
000190         03  JOJF-031      PIC 9(02).                               時
000200         03  JOJF-032      PIC 9(02).                               分
000210     02  JOJF-04           PIC 9(04).                             終了時間
000220     02  JOJF-04R          REDEFINES  JOJF-04.
000230         03  JOJF-041      PIC 9(02).                               時
000240         03  JOJF-042      PIC 9(02).                               分
000250     02  JOJF-05           PIC 9(01).                             処理区分
000260     02  JOJF-06.                                                 終了状況
000270         03  JOJF-061      PIC 9(01).                               区分
000280         03  JOJF-062      PIC X(01).                               STS 1
000290         03  JOJF-063      PIC 9(02).                               STS 2
000300     02  JOJF-07           PIC 9(03).                             相手先
000310     02  JOJF-TBL.
000320*****    03  JOJF-TBL1     OCCURS  10.                            D.060922
000330         03  JOJF-TBL1     OCCURS  12.                            I.060922
000340             04  JOJF-08   PIC 9(02).                             ﾃﾞｰﾀ区分
000350             04  JOJF-09   PIC 9(06).                             ﾃﾞｰﾀ件数
000360             04  JOJF-10   PIC 9(06).                             送受件数
000370             04  JOJF-11   PIC X(10).                             ｽﾀｰﾄ KEY
000380             04  JOJF-12   PIC X(10).                             END  KEY
000390             04  F         PIC X(06).
000400*****02  F                 PIC X(84).                             D.060922
000410     02  F                 PIC X(04).                             I.060922
000420     02  JOJF-90           PIC 9(04).                             NEXT NO
