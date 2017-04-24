           CALL "DB_F_Open" USING
            "INPUT" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE  "01" TO DATE-KEY.
       AAAA.
      *           READ  M-DATE   WITH UNLOCK  INVALID
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID" M-DATE_PNAME1 BY REFERENCE DATE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-DATE" ERR-DATE "0" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-BUZ" ERR-BUZ "0" RETURNING RESU
               GO TO  AAAA
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
