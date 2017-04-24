      ******************************************
      *    MESSEGE  AREA                       *
      *                                        *
      * WORKING-STORAGE SECTION  :  LSMSG_PR   *
      * PROCEDURE DIVISION       :  LSMSG_PR_P *
      ******************************************
       CALL "SD_Init" USING
           "DISP-ERR-AREA" " " "24" "0" "961" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-MSG-01" " " "24" "0" "50" " " "DISP-ERR-AREA"
           RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-MSG-01" "X" "24" "2" "50" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING
           "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-MSG-SPACE" " " "24" "0" "50" "DISP-MSG-01" " "
           RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-MSG-SPACE" "X" "24" "2" "50" " " "DISP-MSG-SPACE"
           RETURNING RESU.
       CALL "SD_From" USING
           "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
           RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-BUZ-B" "X" "24" "80" "5" " " "DISP-BUZ-B"
           RETURNING RESU.
       CALL "SD_Init" USING
           "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " "
           RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZ-J"
           RETURNING RESU.
       CALL "SD_Init" USING
           "NOR-M01" " " "24" "0" "72" "DISP-BUZ-J" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01NOR-M01" "X" "24" "2" "50" " " "NOR-M01" RETURNING RESU.
       CALL "SD_From" USING
           "01NOR-M01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02NOR-M01" "N" "24" "2" "22" "01NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "NOR-D01" " " "24" "0" "72" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01NOR-D01" "X" "24" "2" "50" " " "NOR-D01" RETURNING RESU.
       CALL "SD_From" USING
           "01NOR-D01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02NOR-D01" "N" "24" "2" "22" "01NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "INV-M01" " " "24" "0" "72" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01INV-M01" "X" "24" "2" "50" " " "INV-M01" RETURNING RESU.
       CALL "SD_From" USING
           "01INV-M01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02INV-M01" "N" "24" "2" "22" "01INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "INV-D01" " " "24" "0" "72" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01INV-D01" "X" "24" "2" "50" " " "INV-D01" RETURNING RESU.
       CALL "SD_From" USING
           "01INV-D01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02INV-D01" "N" "24" "2" "22" "01INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "OK-01" " " "24" "0" "64" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01OK-01" "X" "24" "2" "50" " " "OK-01" RETURNING RESU.
       CALL "SD_From" USING
           "01OK-01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02OK-01" "N" "24" "2" "14" "01OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "CAN-01" " " "24" "0" "68" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CAN-01" "X" "24" "2" "50" " " "CAN-01" RETURNING RESU.
       CALL "SD_From" USING
           "01CAN-01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02CAN-01" "N" "24" "2" "18" "01CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-01" " " "24" "0" "68" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-01" "X" "24" "2" "50" " " "ERR-01" RETURNING RESU.
       CALL "SD_From" USING
           "01ERR-01" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02ERR-01" "N" "24" "2" "18" "01ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "INV-MCT" " " "24" "0" "78" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01INV-MCT" "X" "24" "2" "50" " " "INV-MCT" RETURNING RESU.
       CALL "SD_From" USING
           "01INV-MCT" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02INV-MCT" "N" "24" "2" "28" "01INV-MCT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "INV-CON" " " "24" "0" "92" "INV-MCT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01INV-CON" "X" "24" "2" "50" " " "INV-CON" RETURNING RESU.
       CALL "SD_From" USING
           "01INV-CON" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02INV-CON" "N" "24" "2" "42" "01INV-CON" " " RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-YMD" " " "24" "0" "72" "INV-CON" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-YMD" "X" "24" "2" "50" " " "ERR-YMD" RETURNING RESU.
       CALL "SD_From" USING
           "01ERR-YMD" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02ERR-YMD" "N" "24" "2" "22" "01ERR-YMD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "ERR-DIS" " " "24" "0" "121" "ERR-YMD" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01ERR-DIS" "X" "24" "2" "50" " " "ERR-DIS" RETURNING RESU.
       CALL "SD_From" USING
           "01ERR-DIS" BY REFERENCE ERR-SPACE "60" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02ERR-DIS" "X" "24" "2" "5" "01ERR-DIS" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03ERR-DIS" "X" "24" "7" "12" "02ERR-DIS" " " RETURNING RESU.
       CALL "SD_From" USING
           "03ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04ERR-DIS" "X" "24" "19" "1" "03ERR-DIS" " " RETURNING RESU.
       CALL "SD_From" USING
           "04ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05ERR-DIS" "X" "24" "20" "11" "04ERR-DIS" " "
           RETURNING RESU.
       CALL "SD_Init" USING
           "06ERR-DIS" "X" "24" "31" "2" "05ERR-DIS" " " RETURNING RESU.
       CALL "SD_From" USING
           "06ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "07ERR-DIS" "X" "24" "33" "5" "06ERR-DIS" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08ERR-DIS" "X" "24" "38" "5" "07ERR-DIS" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09ERR-DIS" "X" "24" "43" "30" "08ERR-DIS" " "
           RETURNING RESU.
       CALL "SD_From" USING
           "09ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
