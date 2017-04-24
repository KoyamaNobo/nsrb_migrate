000010***************************************
000020*    月数計算　ワーク　               *                           90/11/20
000030***************************************
000040 01  NISU-WORK-AREA.
000050     02  NISU-FROM.
000060         03  NISU-FROMY       PIC 9(04).
000070         03  NISU-FROMM       PIC 9(02).
000080         03  NISU-FROMD       PIC 9(02).
000090     02  NISU-FROMR REDEFINES NISU-FROM     PIC 9(08).
000100**
000110     02  NISU-TO.
000120         03  NISU-TOY         PIC 9(04).
000130         03  NISU-TOM         PIC 9(02).
000140         03  NISU-TOD         PIC 9(02).
000150     02  NISU-TOR REDEFINES NISU-TO     PIC 9(08).
000160**
000170     02  NISU-NET.
000180         03  NISU-NEN             PIC 9(02).
000190         03  NISU-TUK             PIC 9(02).
