# ABAP Report Blueprints 

## Radio Blueprint
ABAP report blueprint with **RADIO BUTTONS** and text inputfields which switch visibility and content according to selection.
```ABAP
REPORT XXX.

*&---------------------------------------------------------------------*
*& Parameter Definitions
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-100.

PARAMETERS r_one RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND r_mode.
PARAMETERS r_two RADIOBUTTON GROUP mode.

SELECTION-SCREEN SKIP.

PARAMETERS pa_one TYPE string MODIF ID one.
PARAMETERS pa_two TYPE string MODIF ID two.

SELECTION-SCREEN END OF BLOCK block1.

*&---------------------------------------------------------------------*
*& Globals
*&---------------------------------------------------------------------*
DATA gv_selected_mode TYPE string.
DATA gc_mode_one TYPE I VALUE 1.
DATA gc_mode_two TYPE I VALUE 2.

*&---------------------------------------------------------------------*
*& Screens
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'ONE'.
        screen-active = COND #( WHEN r_one = 'X' THEN 1 ELSE 0 ).
      WHEN 'TWO'.
        screen-active = COND #( WHEN r_two = 'X' THEN 1 ELSE 0 ).
    ENDCASE.
    PERFORM set_selected_mode.
    MODIFY SCREEN.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& Start Program
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  CASE gv_selected_mode.
  ENDCASE.
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Forms
*&---------------------------------------------------------------------*
FORM set_selected_mode.
  IF r_one EQ abap_true.
    gv_selected_mode = gc_mode_one.
  ELSEIF r_two EQ abap_true.
    gv_selected_mode = gc_mode_two.
  ELSE.
    CLEAR: gv_selected_mode.
  ENDIF.
ENDFORM.
```

## Button Blueprint
ABAP report blueprint with custom **BUTTONS** and click handlers.
```ABAP
REPORT XXX.

*&---------------------------------------------------------------------*
*& Parameter Definitions
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-100.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /1(20) btn_act USER-COMMAND BTN_ACT.

SELECTION-SCREEN END OF BLOCK block1.

*&---------------------------------------------------------------------*
*& Initialization
*&---------------------------------------------------------------------*
INITIALIZATION.
  btn_act = text-ba1. "Define Button Text

*&---------------------------------------------------------------------*
*& Screen Handling
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BTN_ACT'.
      "...
  ENDCASE.
```
