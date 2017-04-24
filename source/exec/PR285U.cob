       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR285U.
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM-100.
       OBJECT-COMPUTER.            SYSTEM-100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
      *
       COPY  LWMSG_PR.
       COPY  LGYM.
       COPY  ACCUNT.
      *
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
      *
           COPY  LSMSG_PR.
      *****
       PROCEDURE               DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           CALL "DB_F_Open" USING
            "I-O" GYM_PNAME1 " " BY REFERENCE GYM_IDLST "1"
            "GYM-KEY" BY REFERENCE GYM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 " " BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
       MAIN00.
      *           READ        GYM         AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" GYM_PNAME1 BY REFERENCE GYM-R " " RETURNING RET.
           IF  RET = 1
               PERFORM CLSE-ENT THRU CLSE-EXT
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE        GYM-011     TO      AM-KEY.
      *           READ        AM          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO      MAIN00
           END-IF.
           IF  DR-CR       =       1
               COMPUTE GYM-03  =   GYM-03  +   GYM-041 -   GYM-042
           ELSE
               COMPUTE GYM-03  =   GYM-03  +   GYM-042 -   GYM-041
           END-IF.
           MOVE        ZERO        TO      GYM-041  GYM-042.
           MOVE        GYM-KEY     TO      ERR-K.
      *           REWRITE     GYM-R       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            GYM_PNAME1 GYM_LNAME GYM-R RETURNING RET.
           IF  RET = 1
               MOVE  "GYM" TO      ERR-F
               MOVE  "R"   TO      ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO          TO          MAIN00.
       MAIN-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE GYM_IDLST GYM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
       CLSE-EXT.
           EXIT.
      *****
       COPY  LPMSG_PR.
