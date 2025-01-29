Download xstring to file using abap gui_download.

```abap
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(FIV_XSTRING) TYPE  XSTRING
*"     REFERENCE(FIV_FILENAME) TYPE  STRING
*"     REFERENCE(FIV_FILE_EXTENSION) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: lv_filename TYPE string,
        lv_filepath     TYPE string,
        lv_fullpath     TYPE string,
        lv_action       TYPE i.

  lv_filename = fiv_filename.
  DATA(lt_raw_data) = cl_bcs_convert=>xstring_to_solix( fiv_xstring ).

  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_file_name = lv_filename
      default_extension = fiv_file_extension
      file_filter = cl_gui_frontend_services=>filetype_all
    CHANGING
      filename          = lv_filename
      path              = lv_filepath
      fullpath          = lv_fullpath
      user_action       = lv_action
  ).
  IF lv_action EQ cl_gui_frontend_services=>action_ok.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename     = lv_fullpath
        filetype     = 'BIN'
        bin_filesize = xstrlen( fiv_xstring )
      CHANGING
        data_tab     = lt_raw_data
    ).
  ENDIF.
```

© unpacked - [licence](../../LICENSE)