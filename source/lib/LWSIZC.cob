      *******************************************************************
      *    ÉTÉCÉYÉRÅ[ÉhÉeÅ[ÉuÉã                                         *
      *******************************************************************
       01  SIZE-CD-TBL.
           02  F  PIC X(30) VALUE "001002010011012013014280290300".
           02  F  PIC X(30) VALUE "125130135140150160170180190200".
           02  F  PIC X(30) VALUE "210215220225230235240245250000".
           02  F  PIC X(30) VALUE "240245250255260265270275000000".
       01  F   REDEFINES SIZE-CD-TBL.
           02  SIZE-CD-T1     OCCURS  04.
               03  SIZE-CD    PIC  9(03)  OCCURS  10.
      *******************************************************************
      *    ÉTÉCÉYñºèÃÉeÅ[ÉuÉã                                           *
      *******************************************************************
       01  SIZE-NM-TBL.
           02  F  PIC N(20) VALUE
               "Å@Å@Å@Å@Å@Å@Å@Å@ÇrÇrÅ@Å@ÇrÅ@Å@Å@ÇlÅ@Å@Å@".
           02  F  PIC N(20) VALUE
               "ÇkÅ@Å@Å@ÇkÇkÅ@Å@ÇwÇkÅ@Å@ÇwÇwÇkÅ@ÇRÇOÅDÇO".
           02  F  PIC N(20) VALUE
               "ÇPÇQÅDÇTÇPÇRÅDÇOÇPÇRÅDÇTÇPÇSÅDÇOÇPÇTÅDÇO".
           02  F  PIC N(20) VALUE
               "ÇPÇUÅDÇOÇPÇVÅDÇOÇPÇWÅDÇOÇPÇXÅDÇOÇQÇOÅDÇO".
           02  F  PIC N(20) VALUE
               "ÇQÇPÅDÇOÇQÇPÅDÇTÇQÇQÅDÇOÇQÇQÅDÇTÇQÇRÅDÇO".
           02  F  PIC N(20) VALUE
               "ÇQÇRÅDÇTÇQÇSÅDÇOÇQÇSÅDÇTÇQÇTÅDÇOÅ@Å@Å@Å@".
           02  F  PIC N(20) VALUE
               "ÇQÇSÅDÇOÇQÇSÅDÇTÇQÇTÅDÇOÇQÇTÅDÇTÇQÇUÅDÇO".
           02  F  PIC N(20) VALUE
               "ÇQÇUÅDÇTÇQÇVÅDÇOÇQÇVÅDÇTÅ@Å@Å@Å@óaÇËÅ@Å@".
           02  F  PIC N(20) VALUE
               "ÇRçÜÅ@Å@ÇQçÜÅ@Å@ÇPçÜÅ@Å@ÇOçÜÅ@Å@íÜÅ@Å@Å@".
           02  F  PIC N(40) VALUE
               "ëÂÅ@Å@Å@ì¡ëÂÅ@Å@ÇQÇWÅDÇOÇQÇXÅDÇOÇRÇOÅDÇO".
       01  F   REDEFINES SIZE-NM-TBL.
           02  SIZE-NM-T1     OCCURS  05.
               03  SIZE-NM    PIC  N(04)  OCCURS  10.
      *******************************************************************
      *    ÉTÉCÉYÉèÅ[ÉN                                                 *
      *******************************************************************
       01  SIZE-WK.
           02  SIZE-WK-HIN    PIC  9(06).                               ïiñº∫∞ƒﬁ
           02  SIZE-WK-CD     PIC  9(03).                               ª≤Ωﬁ∫∞ƒﬁ
           02  SIZE-WK-KB     PIC  9(01).                               ª≤ΩﬁãÊï™
           02  SIZE-WK-NM     PIC  N(04).                               ª≤ΩﬁñºèÃ
           02  SIZE-WK-II     PIC  9(01).
           02  SIZE-WK-JJ     PIC  9(02).
           02  SIZE-WK-SW     PIC  9(01).
