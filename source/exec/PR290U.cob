       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR290U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
      *
       COPY    SIWAID.
       COPY    SIWAKE.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       PROCEDURE       DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       ST.
           CALL "DB_F_Open" USING
            "OUTPUT" SDI_PNAME1 " " BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" SSD_PNAME1 " " BY REFERENCE SSD_IDLST "0".
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_Close".
           STOP       RUN.
