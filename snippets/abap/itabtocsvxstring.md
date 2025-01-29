Takes any table as import and converts it to csv in xsting format for easy download.

```abap
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(FIT_TABLE) TYPE  ANY
*"     REFERENCE(FIV_SEPERATOR) TYPE  STRING DEFAULT ';'
*"  EXPORTING
*"     REFERENCE(FEV_TABLE_XSTRING) TYPE  XSTRING
*"----------------------------------------------------------------------

  DATA: lo_type_descr   TYPE REF TO cl_abap_typedescr,
        lo_table_descr  TYPE REF TO cl_abap_tabledescr,
        lo_struct_descr TYPE REF TO cl_abap_structdescr,
        lt_components   TYPE cl_abap_structdescr=>component_table,
        ls_component    TYPE cl_abap_structdescr=>component,
        lv_csv_table    TYPE string.

  "Check if IT_TABLE is a table
  lo_type_descr = cl_abap_typedescr=>describe_by_data( fit_table ).
  IF lo_type_descr IS NOT INSTANCE OF cl_abap_tabledescr.
    MESSAGE 'FIT_TABLE is not a table' TYPE 'E' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "Get Header Fields of Table
  lo_table_descr ?= lo_type_descr.
  lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
  lt_components = lo_struct_descr->get_components( ).

  "Add Header field to CSV
  LOOP AT lt_components INTO ls_component.
    IF sy-tabix EQ 1.
      CONCATENATE lv_csv_table ls_component-name INTO lv_csv_table.
    ELSE.
      CONCATENATE lv_csv_table ls_component-name INTO lv_csv_table SEPARATED BY fiv_seperator.
    ENDIF.
  ENDLOOP.
  CONCATENATE lv_csv_table cl_abap_char_utilities=>cr_lf INTO lv_csv_table.

  "Add Content to csv
  FIELD-SYMBOLS: <fs_row>   TYPE any,
                 <fs_field> TYPE any,
                 <lt_itab>  TYPE STANDARD TABLE.

  DATA: lt_itab  TYPE REF TO data,
        lv_index TYPE i VALUE 0.

  CREATE DATA lt_itab LIKE fit_table.
  ASSIGN lt_itab->* TO <lt_itab>.
  <lt_itab> = fit_table.

  LOOP AT <lt_itab> ASSIGNING <fs_row>.
    LOOP AT lt_components INTO ls_component.
      ASSIGN COMPONENT ls_component-name OF STRUCTURE <fs_row> TO <fs_field>.
      IF sy-tabix EQ 1.
        CONCATENATE lv_csv_table <fs_field> INTO lv_csv_table.
      ELSE.
        CONCATENATE lv_csv_table <fs_field> INTO lv_csv_table SEPARATED BY fiv_seperator.
      ENDIF.
    ENDLOOP.
    CONCATENATE lv_csv_table cl_abap_char_utilities=>cr_lf INTO lv_csv_table.
  ENDLOOP.

  "CSV to XSTRING
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_csv_table
    IMPORTING
      buffer = ev_table_xstring.
```

© unpacked - [licence](../../LICENSE)