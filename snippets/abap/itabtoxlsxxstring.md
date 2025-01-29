Takes any table as import and converts it to xlsx in xsting format for easy download.

```abap
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(FIT_TABLE) TYPE  ANY
*"  EXPORTING
*"     REFERENCE(FEV_TABLE_XSTRING) TYPE  XSTRING
*"----------------------------------------------------------------------

  "--------------------------- Troubleshooting: ------------------------------------------
  "1. Falls Spalten im Excel zu lang sind, dann die Länge des Feldes prüfen, (z.B. mit strlen)
  "manchmal sind im String unsichtbare Zeichen enthalten
  "--
  "2. Führende Leerzeichen werden im Excel entfernt.
  "Stattdessen kann ein führendes Leerzeichen auch mit "cl_abap_conv_in_ce=>uccp( '200B' )" ersetz werden
  "----------------------------------------------------------------------

  FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.
  DATA: lt_itab  TYPE REF TO data.
  CREATE DATA lt_itab LIKE fit_table.
  ASSIGN lt_itab->* TO <fs_table>.
  <fs_table> = fit_table.

  DATA: lo_salv TYPE REF TO cl_salv_table.
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = lo_salv
    CHANGING
      t_table        = <fs_table>
  ).

  "Cols
  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column_table,
        lt_cols    TYPE salv_t_column_ref,
        ls_col     TYPE salv_s_column_ref,
        ls_color   TYPE lvc_s_colo.

  lr_columns = lo_salv->get_columns( ).
  lt_cols = lr_columns->get( ).

  LOOP AT lt_cols INTO ls_col.
    CONDENSE ls_col-columnname.
    lr_column ?= lr_columns->get_column( ls_col-columnname ).
    lr_column->set_long_text( ls_col-columnname && '' ).
  ENDLOOP.

  ev_table_xstring = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
```

© unpacked - [licence](../../LICENSE)