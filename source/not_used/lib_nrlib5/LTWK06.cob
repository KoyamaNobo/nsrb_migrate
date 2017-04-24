000010 FD  JT-WK06
000020*****BLOCK 03 RECORDS                                             D.970530
000030     BLOCK  2 RECORDS                                             I.970530
000040     LABEL RECORD IS STANDARD
000050     VALUE OF IDENTIFICATION  WK0128ID.                           I.970530
000060*****VALUE OF IDENTIFICATION  JT-OWS85ID.                         D.960209
000070*****VALUE OF IDENTIFICATION  WK0085ID.                           D.970530
000080 01  WK06-R.
000090     02  WK06-KEY.
000100       03  WK06-01          PIC 9(04).                            îNåéìx
000110       03  WK06-01R  REDEFINES  WK06-01.
000120           04  WK06-011     PIC 9(02).                            îN
000130           04  WK06-012     PIC 9(02).                            åé
000140       03  WK06-02          PIC 9(06).                            ïiñº∫∞ƒﬁ
000150       03  WK06-03          PIC 9(01).                            ª≤ΩﬁãÊï™
000160     02  WK06-04.                                                 éÛíçêî
000170       03  WK06-041  OCCURS 10  PIC S9(06)   COMP-3.
000180     02  WK06-05            PIC 9(01).                            ª≤ΩﬁãÊï™
000190     02  FILLER             PIC X(12).                            FILLER
000200     02  FILLER             PIC X(13).                            FILLER
000210     02  WK06-99.                                                 éÛíçîNåé
000220       03  WK06-991         PIC 9(04).                            ÇeÇqÇnÇl
000230       03  WK06-992         PIC 9(04).                            ÇsÇn
000240     02  FILLER             PIC X(43).                            I.970530
