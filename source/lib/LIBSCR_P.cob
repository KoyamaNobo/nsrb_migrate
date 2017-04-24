      *****************************************
      * WORKING-STORAGE SECTION  :  LIBSCR    *
      * PROCEDURE DIVISION       :  LIBSCR_P  *
      *****************************************
       CALL "SD_Init" USING
           "C-DERR" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DERR" " " "24" "0" "32" " " "C-DERR" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-BUZ" "X" "24" "75" "5" " " "01C-DERR" RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-DATE" "X" "24" "15" "27" "ERR-BUZ" " " RETURNING RESU.
