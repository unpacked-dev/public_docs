# Transport RFC
is a tool for exporting and importing transports from one system to another by sending the cofiles via RFC.

`EXPORT`
```abap
FUNCTION z_transport_rfc_export.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IV_TRANSPORT) TYPE  STRING
*"----------------------------------------------------------------------

  " Schritte:
  " 1. Transportnummer (importparameter) lesen
  DATA: lv_tmp TYPE string,
        lv_system TYPE string,
        lv_transport_no TYPE string.

  lv_system = syst-sysid.

  SPLIT iv_transport AT lv_system INTO lv_tmp lv_transport_no.
  SPLIT lv_transport_no AT 'K' INTO lv_tmp lv_transport_no.

  IF lv_transport_no IS INITIAL.
    MESSAGE 'ERROR WHILE PARSING TRANSPORT NUMBER' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " 2. Path lesen
  DATA: lv_trans_path TYPE char128.

  CALL FUNCTION 'RSAN_SYSTEM_PARAMETER_READ'
    EXPORTING
      i_name  = 'DIR_TRANS'
    IMPORTING
      e_value = lv_trans_path.

  " 3. Dateinamen bauen
  DATA: lv_cofiles_name TYPE string,
        lv_data_name  TYPE string.

  CONCATENATE 'K' lv_transport_no '.' lv_system INTO lv_cofiles_name.
  CONCATENATE 'R' lv_transport_no '.' lv_system INTO lv_data_name.

  " 4. Dateien aus Path in XSTRING lesen
  DATA: lv_cofiles_path TYPE string,
        lv_data_path TYPE string,
        lv_export_cofiles TYPE xstring,
        lv_export_data  TYPE xstring,
        lx_conversion_codepage TYPE REF TO cx_sy_conversion_codepage.

  CONCATENATE lv_trans_path '\cofiles\' lv_cofiles_name INTO lv_cofiles_path.
  CONCATENATE lv_trans_path '\data\' lv_data_name INTO lv_data_path.

  IF lv_cofiles_path IS INITIAL OR lv_data_path IS INITIAL.
    MESSAGE 'ERROR WHILE PARSING FILENAMES' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " 4.1 Read Cofiles
  CLOSE DATASET lv_cofiles_path.
  OPEN DATASET lv_cofiles_path FOR INPUT IN BINARY MODE.

  TRY.
      READ DATASET lv_cofiles_path INTO lv_export_cofiles.
    CATCH cx_sy_conversion_codepage INTO lx_conversion_codepage. " Error handling here!
      MESSAGE 'ERROR WHILE READING COFILES' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.

  IF sy-subrc NE 0.
    MESSAGE 'ERROR WHILE READING COFILES' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  CLOSE DATASET lv_cofiles_path.

  " 4.2 Read Data
  CLOSE DATASET lv_data_path.
  OPEN DATASET lv_data_path FOR INPUT IN BINARY MODE.

  TRY.
      READ DATASET lv_data_path INTO lv_export_data.
    CATCH cx_sy_conversion_codepage INTO lx_conversion_codepage. " Error handling here!
      MESSAGE 'ERROR WHILE READING TRANSPORT DATA' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.

  IF sy-subrc NE 0.
    MESSAGE 'ERROR WHILE READING TRANSPORT DATA' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  CLOSE DATASET lv_data_path.

  " 5. Remote FuBa (IMPORT) aufrufen und COFILES + DATA übergeben
  CALL FUNCTION 'Z_TRANSPORT_RFC_IMPORT'
    DESTINATION 'YOUR_RFC'
    EXPORTING
      iv_transport         = iv_transport
      iv_transport_cofiles = lv_export_cofiles
      iv_transport_data    = lv_export_data
      iv_name_cofiles      = lv_cofiles_name
      iv_name_data         = lv_data_name.

ENDFUNCTION.
```

`IMPORT`
```abap
FUNCTION Z_TRANSPORT_RFC_IMPORT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_TRANSPORT) TYPE  STRING
*"     VALUE(IV_TRANSPORT_COFILES) TYPE  XSTRING
*"     VALUE(IV_TRANSPORT_DATA) TYPE  XSTRING
*"     VALUE(IV_NAME_COFILES) TYPE  STRING
*"     VALUE(IV_NAME_DATA) TYPE  STRING
*"----------------------------------------------------------------------

DATA: lv_system TYPE string.
lv_system = syst-sysid.

" 1. Daten angekommen?
IF IV_TRANSPORT IS INITIAL OR IV_TRANSPORT_COFILES IS INITIAL OR IV_TRANSPORT_DATA IS INITIAL.
  MESSAGE 'INVALID IMPORTPARAMETERS' TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.

" 2. Path zum speichern lesen
DATA: lv_trans_path TYPE char128.

CALL FUNCTION 'RSAN_SYSTEM_PARAMETER_READ'
    EXPORTING
      i_name  = 'DIR_TRANS'
    IMPORTING
      e_value = lv_trans_path.

" 3. Paths erstellen
DATA: lv_cofiles_path TYPE string,
      lv_data_path TYPE string,
      lx_conversion_codepage TYPE REF TO cx_sy_conversion_codepage.

  CONCATENATE lv_trans_path '/cofiles/' IV_NAME_COFILES INTO lv_cofiles_path.
  CONCATENATE lv_trans_path '/data/' IV_NAME_DATA INTO lv_data_path.

" 4. Daten schreiben
TRY.
      OPEN DATASET lv_cofiles_path FOR OUTPUT IN BINARY MODE.
      TRANSFER IV_TRANSPORT_COFILES TO lv_cofiles_path.
      CLOSE DATASET lv_cofiles_path.
    CATCH cx_sy_conversion_codepage INTO lx_conversion_codepage. " Error handling here!
      MESSAGE 'ERROR WHILE WRITING COFILES' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
ENDTRY.

TRY.
      OPEN DATASET lv_data_path FOR OUTPUT IN BINARY MODE.
      TRANSFER IV_TRANSPORT_DATA TO lv_data_path.
      CLOSE DATASET lv_data_path.
    CATCH cx_sy_conversion_codepage INTO lx_conversion_codepage. " Error handling here!
      MESSAGE 'ERROR WHILE WRITING TRANSPORT DATA' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
ENDTRY.

" 5.0 (Zwischenschritt)
DATA: lv_import_sys TYPE TMSCSYS-SYSNAM,
      lv_import_transport TYPE TMSBUFFER-TRKORR.

lv_import_sys = LV_SYSTEM.
LV_IMPORT_TRANSPORT = IV_TRANSPORT.

" 5. Transportauftrag zu STMS importiere
CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
  EXPORTING
    IV_REQUEST                       = lv_import_transport
    IV_TARGET                        = lv_import_sys
*   IV_TARDOM                        =
*   IV_TARCLI                        =
*   IV_SOURCE                        =
*   IV_SRCDOM                        =
*   IV_NO_DELIVERY                   =
*   IV_WRONG_POS                     =
*   IV_IMPORT_AGAIN                  =
*   IV_MONITOR                       = 'X'
*   IV_VERBOSE                       =
*   IT_REQUESTS                      =
* IMPORTING
*   EV_DIFFERENT_GROUPS              =
*   EV_TP_RET_CODE                   =
*   EV_TP_ALOG                       =
*   EV_TP_SLOG                       =
*   EV_TP_PID                        =
*   ES_EXCEPTION                     =
*   ET_TP_FORWARDS                   =
* TABLES
*   TT_STDOUT                        =
* EXCEPTIONS
*   READ_CONFIG_FAILED               = 1
*   TABLE_OF_REQUESTS_IS_EMPTY       = 2
*   OTHERS                           = 3
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
  MESSAGE 'ERROR WHILE IMPORTING TRANSPORT INTO STMS' TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.

" 6. Transportauftrag von STMS automatisch importieren
CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
  EXPORTING
    IV_SYSTEM                        = LV_IMPORT_SYS
*   IV_DOMAIN                        =
    IV_REQUEST                       = LV_IMPORT_TRANSPORT
*   IV_CLIENT                        =
*   IV_CTC_ACTIVE                    =
*   IV_OVERTAKE                      =
*   IV_IMPORT_AGAIN                  =
*   IV_IGNORE_ORIGINALITY            =
*   IV_IGNORE_REPAIRS                =
*   IV_IGNORE_TRANSTYPE              =
*   IV_IGNORE_TABLETYPE              =
*   IV_IGNORE_QAFLAG                 =
*   IV_IGNORE_PREDEC                 =
   IV_IGNORE_CVERS                  = 'X'
*   IV_IGNORE_SPAM                   =
*   IV_TEST_IMPORT                   =
*   IV_CMD_IMPORT                    =
*   IV_NO_DELIVERY                   =
*   IV_SUBSET                        =
*   IV_OFFLINE                       =
*   IV_FEEDBACK                      =
*   IV_MONITOR                       = 'X'
*   IV_FORCE                         =
*   IV_VERBOSE                       =
*   IS_BATCH                         =
*   IT_REQUESTS                      =
*   IT_CLIENTS                       =
* IMPORTING
*   EV_TP_RET_CODE                   =
*   EV_TP_ALOG                       =
*   EV_TP_SLOG                       =
*   EV_TP_PID                        =
*   EV_TPSTAT_KEY                    =
*   ES_EXCEPTION                     =
*   ET_TP_IMPORTS                    =
* TABLES
*   TT_LOGPTR                        =
*   TT_STDOUT                        =
* EXCEPTIONS
*   READ_CONFIG_FAILED               = 1
*   TABLE_OF_REQUESTS_IS_EMPTY       = 2
*   OTHERS                           = 3
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
   MESSAGE 'ERROR WHILE IMPORTING TRANSPORT' TYPE 'S' DISPLAY LIKE 'E'.
  EXIT.
ENDIF.

ENDFUNCTION.
```