      ****************************************
      * WORKING-STORAGE SECTION  :  LSSEM    *
      * PROCEDURE DIVISION       :  LSSEM_P  *
      ****************************************
       CALL "SD_Init" USING
           "SCR-STN-ERR-MSG" " " "24" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "SCR-STN-ERR-MSG"
            RETURNING RESU.
       CALL "SD_From" USING
        "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME71" " " "24" "0" "63" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING
        "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME71" "N" "24" "15" "50" "01E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
