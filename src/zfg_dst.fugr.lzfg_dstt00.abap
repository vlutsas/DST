*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCFT_DST_FIELDS.................................*
DATA:  BEGIN OF STATUS_ZCFT_DST_FIELDS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCFT_DST_FIELDS               .
CONTROLS: TCTRL_ZCFT_DST_FIELDS
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZCFT_DST_KEYS...................................*
DATA:  BEGIN OF STATUS_ZCFT_DST_KEYS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCFT_DST_KEYS                 .
CONTROLS: TCTRL_ZCFT_DST_KEYS
            TYPE TABLEVIEW USING SCREEN '0005'.
*.........table declarations:.................................*
TABLES: *ZCFT_DST_FIELDS               .
TABLES: *ZCFT_DST_KEYS                 .
TABLES: ZCFT_DST_FIELDS                .
TABLES: ZCFT_DST_KEYS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
