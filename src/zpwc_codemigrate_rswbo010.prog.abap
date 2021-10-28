REPORT zpwc_codemigrate_rswbo010 NO STANDARD PAGE HEADING.

TABLES:     tadir.
TYPE-POOLS: strwb.

DATA: gt_tadir             LIKE tadir OCCURS 0,
      gt_object_texts      LIKE ko100 OCCURS 0,
      gv_ucomm             TYPE sy-ucomm,
      gv_detail            TYPE c,
      gv_devclass_visible  TYPE strwb_field,
      gv_srcsystem_visible TYPE strwb_field,
      gv_number_of_objects TYPE i.

* makro definition
DEFINE scr_line.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS &1 AS CHECKBOX MODIF ID mor.
  PARAMETERS &2 LIKE tadir-pgmid  DEFAULT 'R3TR' MODIF ID out.
  PARAMETERS &3 LIKE tadir-object DEFAULT &4     MODIF ID out.
  SELECTION-SCREEN: COMMENT 13(19) &5 MODIF ID mor, POSITION 33.
  PARAMETERS &6 LIKE &7 MODIF ID mor.
  SELECTION-SCREEN END OF LINE.
END-OF-DEFINITION.

* standard selections
SELECTION-SCREEN BEGIN OF BLOCK general WITH FRAME TITLE TEXT-t01.
  SELECT-OPTIONS: devclass FOR tadir-devclass  MODIF ID dev,
                  author   FOR tadir-author,
                  srcsystm FOR tadir-srcsystem MODIF ID sys.
SELECTION-SCREEN END OF BLOCK general.

* selections by type and name
SELECTION-SCREEN BEGIN OF BLOCK objects1 WITH FRAME TITLE TEXT-t02.


  PARAMETERS: p_all  RADIOBUTTON GROUP radi     USER-COMMAND choi,
              p_part RADIOBUTTON GROUP radi.

  SELECTION-SCREEN BEGIN OF BLOCK objects2 WITH FRAME TITLE TEXT-t03.

    scr_line  check1 pgm1 obj1 'PROG' TEXT-p01 objname1 rseux-cp_value.
    scr_line  check2 pgm2 obj2 'FUGR' TEXT-p02 objname2 rseux-cf_value.
    scr_line  check7 pgm7 obj7 'CLAS' TEXT-p07 objname7 seoclass-clsname.
    scr_line  check3 pgm3 obj3 'TABL' TEXT-p03 objname3 dd02v-tabname.
    scr_line  check4 pgm4 obj4 'VIEW' TEXT-p04 objname4 dd25l-viewname.
    scr_line  check6 pgm6 obj6 'TTYP' TEXT-p06 objname6 dd40l-typename.
    scr_line  check5 pgm5 obj5 'DTEL' TEXT-p05 objname5 dd04v-rollname.

    SELECTION-SCREEN BEGIN OF BLOCK b01.
      SELECTION-SCREEN BEGIN OF LINE.
        PARAMETERS checka   LIKE sctsobject-checkbox   MODIF ID mor.
        SELECTION-SCREEN POSITION 3.
        PARAMETERS pgmida   LIKE sctsobject-pgmid      MODIF ID out.
        PARAMETERS objecta  LIKE sctsobject-object     MODIF ID mor.
        SELECTION-SCREEN POSITION 13.
        PARAMETERS objtexta LIKE sctsobject-text VISIBLE LENGTH 19 LOWER CASE
                                                       MODIF ID 2d.
        SELECTION-SCREEN POSITION 33.
        PARAMETERS objnamea LIKE tadir-obj_name        MODIF ID mor.
        PARAMETERS only_ca  LIKE sctsobject-only_compl NO-DISPLAY DEFAULT 'X'.
      SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN END OF BLOCK b01.

    SELECTION-SCREEN BEGIN OF BLOCK b02.
      SELECTION-SCREEN BEGIN OF LINE.
        PARAMETERS: checkb  LIKE sctsobject-checkbox   MODIF ID mor.
        SELECTION-SCREEN POSITION 3.
        PARAMETERS pgmidb   LIKE sctsobject-pgmid      MODIF ID out.
        PARAMETERS objectb  LIKE sctsobject-object     MODIF ID mor.
        PARAMETERS objtextb LIKE sctsobject-text VISIBLE LENGTH 19 LOWER CASE
                                                       MODIF ID 2d.
        SELECTION-SCREEN POSITION 33.
        PARAMETERS objnameb LIKE tadir-obj_name        MODIF ID mor.
        PARAMETERS only_cb  LIKE sctsobject-only_compl NO-DISPLAY DEFAULT 'X'.
      SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN END OF BLOCK b02.

    SELECTION-SCREEN BEGIN OF BLOCK b03.
      SELECTION-SCREEN BEGIN OF LINE.
        PARAMETERS: checkc  LIKE sctsobject-checkbox   MODIF ID mor.
        SELECTION-SCREEN POSITION 3.
        PARAMETERS pgmidc   LIKE sctsobject-pgmid      MODIF ID out.
        PARAMETERS objectc  LIKE sctsobject-object     MODIF ID mor.
        PARAMETERS objtextc LIKE sctsobject-text VISIBLE LENGTH 19 LOWER CASE
                                                       MODIF ID 2d.
        SELECTION-SCREEN POSITION 33.
        PARAMETERS objnamec LIKE tadir-obj_name        MODIF ID mor.
        PARAMETERS only_cc  LIKE sctsobject-only_compl NO-DISPLAY DEFAULT 'X'.
      SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN END OF BLOCK b03.

  SELECTION-SCREEN END OF BLOCK objects2.
SELECTION-SCREEN END OF BLOCK objects1.

SELECTION-SCREEN BEGIN OF BLOCK max_hits WITH FRAME TITLE TEXT-t04.
  PARAMETERS max_hits  TYPE i       DEFAULT 1000.
  SELECTION-SCREEN: BEGIN OF LINE, COMMENT 1(32) TEXT-t05, POSITION 33.
  PARAMETERS show_all  AS CHECKBOX.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN: BEGIN OF LINE, COMMENT 1(32) TEXT-t06, POSITION 33.
  PARAMETERS only_exi  AS CHECKBOX  DEFAULT 'X'.
  SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK max_hits.

*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization     TABLES   gt_object_texts
                             CHANGING gv_devclass_visible
                                      gv_srcsystem_visible.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output  USING  gv_devclass_visible
                                             gv_srcsystem_visible.

AT SELECTION-SCREEN ON objecta.
  PERFORM at_selection_screen_on_field    TABLES gt_object_texts
                                          USING 'OBJECTA'.

AT SELECTION-SCREEN ON objectb.
  PERFORM at_selection_screen_on_field    TABLES gt_object_texts
                                          USING 'OBJECTB'.

AT SELECTION-SCREEN ON objectc.
  PERFORM at_selection_screen_on_field    TABLES gt_object_texts
                                          USING 'OBJECTC'.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname1.
  PERFORM value_request_for_field          USING 'OBJNAME1'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname2.
  PERFORM value_request_for_field          USING 'OBJNAME2'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname3.
  PERFORM value_request_for_field          USING 'OBJNAME3'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname4.
  PERFORM value_request_for_field          USING 'OBJNAME4'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname5.
  PERFORM value_request_for_field          USING 'OBJNAME5'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname6.
  PERFORM value_request_for_field          USING 'OBJNAME6'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR objname7.
  PERFORM value_request_for_field          USING 'OBJNAME7'.

AT USER-COMMAND.
  PERFORM user_command.

*°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°*
START-OF-SELECTION.

  PERFORM select_objects            TABLES   gt_tadir
                                    USING    max_hits
                                             show_all
                                             only_exi
                                    CHANGING gv_number_of_objects.

  PERFORM export_objects_to_memory  TABLES   gt_tadir
                                    USING    gv_number_of_objects.

*°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°*

*&---------------------------------------------------------------------*
*&      Form  CHECK_BEFORE_EXECUTION
*&---------------------------------------------------------------------*
FORM check_before_execution.
  DATA: lv_lines TYPE i,
        lv_total TYPE i.

*  check only before execution or execution&printing
  IF gv_ucomm = 'ONLI' OR gv_ucomm = 'PRIN'.

    DESCRIBE TABLE devclass LINES lv_total.
    DESCRIBE TABLE author   LINES lv_lines.
    ADD lv_lines TO lv_total.
    DESCRIBE TABLE srcsystm LINES lv_lines.
    ADD lv_lines TO lv_total.
    IF lv_total < 1.
      IF check1 = ' ' AND check2 = ' ' AND check3 = ' ' AND
         check4 = ' ' AND check5 = ' ' AND check6 = ' ' AND
         check7 = ' ' AND checka = ' ' AND checkb = ' ' AND
         checkc = ' '.
        MESSAGE e871(tk).
*       restrict object choice
      ENDIF.
    ENDIF.

    IF gv_detail = 'X' AND
       check1 = ' ' AND check2 = ' ' AND check3 = ' ' AND
       check4 = ' ' AND check5 = ' ' AND check6 = ' ' AND
       check7 = ' ' AND checka = ' ' AND checkb = ' ' AND
       checkc = ' '.
      SET CURSOR FIELD 'objname1'.
      MESSAGE e844(tk).
*     restrict object choice
    ENDIF.

    IF check1 = 'X' AND objname1 CA '''#@!'.
      MESSAGE e879(tk) WITH objname1.
    ENDIF.
    IF check2 = 'X' AND objname2 CA '''#@!'.
      MESSAGE e879(tk) WITH objname2.
    ENDIF.
    IF check3 = 'X' AND objname3 CA '''#@!'.
      MESSAGE e879(tk) WITH objname3.
    ENDIF.
    IF check4 = 'X' AND objname4 CA '''#@!'.
      MESSAGE e879(tk) WITH objname4.
    ENDIF.
    IF check5 = 'X' AND objname5 CA '''#@!'.
      MESSAGE e879(tk) WITH objname5.
    ENDIF.
    IF check6 = 'X' AND objname6 CA '''#@!'.
      MESSAGE e879(tk) WITH objname6.
    ENDIF.
    IF check7 = 'X' AND objname7 CA '''#@!'.
      MESSAGE e879(tk) WITH objname7.
    ENDIF.
    IF checka = 'X' AND objnamea CA '''#@!'.
      MESSAGE e879(tk) WITH objnamea.
    ENDIF.
    IF checkb = 'X' AND objnameb CA '''#@!'.
      MESSAGE e879(tk) WITH objnameb.
    ENDIF.
    IF checkc = 'X' AND objnamec CA '''#@!'.
      MESSAGE e879(tk) WITH objnamec.
    ENDIF.
  ENDIF.
ENDFORM.                               " CHECK_BEFORE_EXECUTION


*&---------------------------------------------------------------------*
*&      Form  FILL_OBJECTTABLE
*&---------------------------------------------------------------------*
FORM fill_objecttable TABLES   pt_object_texts STRUCTURE ko100.

  IF pt_object_texts[] IS INITIAL.
    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = pt_object_texts.

    DELETE pt_object_texts
                       WHERE NOT pgmid = 'R3TR'
                         AND NOT pgmid = 'R3OB'
                         AND NOT ( pgmid = 'LIMU' AND object = 'COMM' ).
    DELETE pt_object_texts
                       WHERE pgmid = 'R3TR' AND object = 'TABU'.
    SORT pt_object_texts BY object.
  ENDIF.
ENDFORM.                               " FILL_OBJECTTABLE

*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJECTS
*&---------------------------------------------------------------------*
FORM select_objects  TABLES   pt_tadir           STRUCTURE tadir
                     USING    VALUE(pv_max_hits) TYPE i
                              VALUE(pv_show_all) TYPE c
                              VALUE(pv_only_exi) TYPE c
                     CHANGING pv_hits            TYPE i.

  DATA: BEGIN OF lt_where OCCURS 0,
          line(72),
        END OF lt_where.

  DATA: lt_tadir        LIKE tadir         OCCURS 0,
        ls_tadir        LIKE tadir,
        or(2)           TYPE c   VALUE space,
        lv_max_hits     TYPE i,
        lv_package_size TYPE i,
        lv_flag         TYPE c,
        lv_lines        TYPE i,
        lv_pgmid        LIKE tadir-pgmid,
        lv_ok           TYPE c,
        c               TYPE cursor.

*--build select statement dynamically
  lt_where-line = '('. APPEND lt_where.

  IF NOT check1 IS INITIAL.
    PERFORM prepare_variables      USING  objname1 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm1 obj1 objname1 lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT check2 IS INITIAL.
    PERFORM prepare_variables      USING  objname2 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm2 obj2 objname2 lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT check3 IS INITIAL.
    PERFORM prepare_variables      USING  objname3 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm3 obj3 objname3 lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT check4 IS INITIAL.
    PERFORM prepare_variables      USING  objname4 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm4 obj4 objname4 lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT check5 IS INITIAL.
    PERFORM prepare_variables      USING  objname5 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm5 obj5 objname5 lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT check6 IS INITIAL.
    PERFORM prepare_variables      USING  objname6 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm6 obj6 objname6 lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT check7 IS INITIAL.
    PERFORM prepare_variables      USING  objname7 lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  pgm7 obj7 objname7 lv_flag
                                   CHANGING or.
  ENDIF.

  IF NOT checka IS INITIAL.
    PERFORM prepare_variables2     USING  lv_pgmid
                                          objecta objnamea lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  lv_pgmid
                                          objecta objnamea lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT checkb IS INITIAL.
    PERFORM prepare_variables2     USING  lv_pgmid
                                          objectb objnameb lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  lv_pgmid
                                          objectb objnameb lv_flag
                                   CHANGING or.
  ENDIF.
  IF NOT checkc IS INITIAL.
    PERFORM prepare_variables2     USING  lv_pgmid
                                          objectc objnamec lv_flag.
    PERFORM build_select_statement TABLES lt_where
                                   USING  lv_pgmid
                                          objectc objnamec lv_flag
                                   CHANGING or.
  ENDIF.

*--if no entry except '(' has been made yet, clear table
  DESCRIBE TABLE lt_where LINES lv_lines.
  IF lv_lines > 1.
    lt_where-line = ')'. APPEND lt_where.
  ELSE.
    CLEAR: lt_where[].
  ENDIF.

*--define the number of rows to be selected
  IF pv_show_all = 'X'.

    SELECT        * FROM  tadir INTO TABLE pt_tadir
    WHERE  srcsystem  IN srcsystm
    AND    author     IN author
    AND    devclass   IN devclass
    AND    (lt_where).

    LOOP AT pt_tadir.
      PERFORM check_tadir       USING    pt_tadir
                                         pv_only_exi
                                CHANGING lv_ok.
      IF lv_ok <> 'X'.
        DELETE pt_tadir.
      ENDIF.
    ENDLOOP.

  ELSE.
    IF pv_max_hits < 1.
      lv_max_hits = 1000.
    ELSE.
      lv_max_hits = pv_max_hits.
    ENDIF.
    lv_package_size = pv_max_hits.

    OPEN CURSOR c FOR  SELECT * FROM  tadir
                              WHERE  srcsystem  IN srcsystm
                              AND    author     IN author
                              AND    devclass   IN devclass
                              AND    (lt_where).

    DO.
      FETCH NEXT CURSOR c INTO TABLE lt_tadir
                          PACKAGE SIZE lv_package_size.
      IF sy-subrc <> 0.
        CLOSE CURSOR c. EXIT.
      ENDIF.
      LOOP AT lt_tadir INTO ls_tadir.
        PERFORM check_tadir       USING    ls_tadir
                                           pv_only_exi
                                  CHANGING lv_ok.
        IF lv_ok = 'X'.
          APPEND ls_tadir TO pt_tadir.
          DESCRIBE TABLE pt_tadir LINES lv_lines.
          IF lv_lines >= lv_max_hits. EXIT. ENDIF.
        ENDIF.
      ENDLOOP.

      DESCRIBE TABLE pt_tadir LINES lv_lines.
      IF lv_lines >= lv_max_hits. CLOSE CURSOR c. EXIT. ENDIF.
      CLEAR lt_tadir[].
    ENDDO.
  ENDIF.

  DESCRIBE TABLE pt_tadir LINES pv_hits.

ENDFORM.                               " SELECT_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.

  CASE sy-ucomm.
    WHEN 'BAC2'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'ENTR'.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.

ENDFORM.                               " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization  TABLES   pt_object_texts          STRUCTURE ko100
                     CHANGING pv_devclass_visible      TYPE strwb_field
                              pv_srcsystem_visible     TYPE strwb_field.

  DATA: et_objects               TYPE strwb_objects. "table for export
  DATA: iv_srcsystem             LIKE tadir-srcsystem,
        iv_srcsystem_visible     TYPE strwb_field,
        iv_devclass              LIKE tadir-devclass,
        iv_devclass_visible      TYPE strwb_field,
        iv_only_existing_objects TYPE strwb_field,
        iv_title                 LIKE rseu1-tit_text,
        lv_flag                  TYPE c            VALUE space,
        ev_number_of_objects     TYPE i            VALUE 0.

** initialization
*  EXPORT et_objects
*         lv_flag
*         ev_number_of_objects TO MEMORY ID 'OBJ_5957'.
*
*  IMPORT iv_srcsystem
*         iv_srcsystem_visible
*         iv_devclass
*         iv_devclass_visible
*         iv_only_existing_objects
*         iv_title             FROM MEMORY ID 'PARAMS_5957'.

  IF iv_title <> space.
    SET TITLEBAR 'GEN' WITH iv_title.
  ENDIF.

  pv_srcsystem_visible     = iv_srcsystem_visible.
*  pv_devclass_visible      = iv_devclass_visible.
  pv_devclass_visible      = abap_true.

  only_exi = iv_only_existing_objects.

*  IF NOT iv_devclass IS INITIAL.
*    devclass-low = iv_devclass.
*    devclass-option = 'EQ'.
*    devclass-sign   = 'I'.
*    APPEND devclass.
*  ENDIF.
*
*  IF NOT iv_srcsystem IS INITIAL.
*    srcsystm-low = iv_srcsystem.
*    srcsystm-option = 'EQ'.
*    srcsystm-sign = 'I'.
*    APPEND srcsystm.
*  ENDIF.

*--fill table with texts of objects
  PERFORM fill_objecttable   TABLES pt_object_texts.

ENDFORM.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen.

  gv_ucomm = sy-ucomm.

  IF gv_ucomm = 'CHOI'
  AND p_all   = 'X'.
    IF check1 = 'X' OR check2 = 'X' OR check3 = 'X' OR
       check4 = 'X' OR check5 = 'X' OR check6 = 'X' OR
       check7 = 'X' OR checka = 'X' OR checkb = 'X' OR
       objname1 <> space OR objname2 <> space OR
       objname3 <> space OR objname4 <> space OR
       objname5 <> space OR objname6 <> space OR
       objname7 <> space OR objnamea <> space OR
       objnameb <> space OR objnamec <> space.
      MESSAGE w872(tk).
*       Your selection will be lost
    ENDIF.
  ENDIF.
  PERFORM check_before_execution.

ENDFORM.                               " AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN_ON_FIELD
*&---------------------------------------------------------------------*
FORM at_selection_screen_on_field TABLES pt_object_texts STRUCTURE ko100
                                  USING pv_field.

  DATA: ls_object_text       LIKE ko100.

  CASE pv_field.
    WHEN 'OBJECTA'.
      IF objecta <> space.
        READ TABLE pt_object_texts INTO ls_object_text
                                   WITH KEY object = objecta.
        IF sy-subrc <> 0.
          MESSAGE e870(tk).
*         Bitte wählen Sie einen gültigen Objekttyp aus
        ELSE.
          pgmida   = ls_object_text-pgmid.
          objtexta = ls_object_text-text.
        ENDIF.
      ELSE.
        CLEAR: checka, pgmida, objtexta, objnamea.
      ENDIF.

    WHEN 'OBJECTB'.
      IF objectb <> space.
        READ TABLE pt_object_texts INTO ls_object_text
                                   WITH KEY object = objectb.
        IF sy-subrc <> 0.
          MESSAGE e870(tk).
*         Bitte wählen Sie einen gültigen Objekttyp aus
        ELSE.
          pgmidb   = ls_object_text-pgmid.
          objtextb = ls_object_text-text.
        ENDIF.
      ELSE.
        CLEAR: checkb, pgmidb, objtextb, objnameb.
      ENDIF.

    WHEN 'OBJECTC'.
      IF objectc <> space.
        READ TABLE pt_object_texts INTO ls_object_text
                                   WITH KEY object = objectc.
        IF sy-subrc <> 0.
          MESSAGE e870(tk).
*         Bitte wählen Sie einen gültigen Objekttyp aus
        ELSE.
          pgmidc   = ls_object_text-pgmid.
          objtextc = ls_object_text-text.
        ENDIF.
      ELSE.
        CLEAR: checkc, pgmidc, objtextc, objnamec.
      ENDIF.
  ENDCASE.
ENDFORM.                               " AT_SELECTION_SCREEN_ON_FIELD

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM at_selection_screen_output   USING  pv_devclass_visible
                                         pv_srcsystem_visible.

  DATA: BEGIN OF lt_excl_okcode OCCURS 0,
          okcode LIKE sy-ucomm,
        END OF lt_excl_okcode.

* exclude some functions in the menu
  lt_excl_okcode-okcode = 'PRIN'.
  APPEND lt_excl_okcode.
  lt_excl_okcode-okcode = 'SJOB'.
  APPEND lt_excl_okcode.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_excl_okcode.


  IF gv_ucomm = 'CHOI'.
    IF p_all = 'X'.
      gv_detail = ' '.
      CLEAR: check1, check2, check3, check4, check5, check6,
             check7, checka, checkb, checkc,
             pgmida,  pgmidb,  pgmidc,
             objecta, objectb, objectc,
             objname1, objname2, objname3, objname4, objname5,
             objname6, objname7, objnamea, objnameb, objnamec.
    ELSEIF p_part = 'X'.
      gv_detail = 'X'.
    ENDIF.
  ENDIF.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN  'MOR'.
        IF gv_detail = 'X'.
          screen-active = '1'. MODIFY SCREEN.
        ELSE.
          screen-active = '0'. MODIFY SCREEN.
        ENDIF.
      WHEN 'OUT'.
        screen-input = '0'.
        IF gv_detail = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
        MODIFY SCREEN.
      WHEN '2D'.
        screen-input = '0'. screen-display_3d = 0.
        IF gv_detail = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'DEV'.
        IF pv_devclass_visible = ' '.
          screen-active = '0'. MODIFY SCREEN.
        ENDIF.
      WHEN 'SYS'.
        IF pv_srcsystem_visible = ' '.
          screen-active = '0'. MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                               " AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXPORT_OBJECTS_TO_MEMORY
*&---------------------------------------------------------------------*
FORM export_objects_to_memory TABLES pt_tadir STRUCTURE tadir
                              USING  ev_number_of_objects TYPE i.

*  DATA: et_objects TYPE strwb_objects. "table to be exported
*  DATA: ls_object  TYPE strwb_object.
*  DATA: ls_tadir   LIKE tadir.
  DATA: lv_flag    TYPE c       VALUE 'X'.
*
*  IF pt_tadir[] IS INITIAL.
*    MESSAGE s850(tk).
**    no objects selected
*  ELSE.
*    LOOP AT pt_tadir INTO ls_tadir.
*      MOVE-CORRESPONDING ls_tadir TO ls_object.
*      APPEND ls_object TO et_objects.
*    ENDLOOP.
*    FREE: pt_tadir.

*    EXPORT et_objects
*           lv_flag
*           ev_number_of_objects    TO MEMORY ID 'OBJ_5957'.
    EXPORT gt_tadir from pt_tadir TO MEMORY ID 'PWC_CM_MEM1'.

    SET SCREEN 0. LEAVE SCREEN.
*  ENDIF.
ENDFORM.                               " EXPORT_OBJECTS_TO_MEMORY

*&---------------------------------------------------------------------*
*&      Form  BUILD_SELECT_STATEMENT
*&---------------------------------------------------------------------*
FORM build_select_statement TABLES   pt_where
                            USING VALUE(pv_pgmid)    LIKE tadir-pgmid
                                  VALUE(pv_object)   LIKE tadir-object
                                  VALUE(pv_obj_name)
                                  VALUE(pv_flag)
                            CHANGING or.

  CASE pv_flag.
    WHEN space.
      CONCATENATE or  ' ( PGMID = '''    pv_pgmid  ''''
                      ' AND OBJECT = '''  pv_object ''''
                      ' AND OBJ_NAME = ' INTO pt_where.
      APPEND pt_where.
      CONCATENATE '''' pv_obj_name '''' ' ) ' INTO pt_where.
      APPEND pt_where.
      or = 'OR'.
    WHEN 'L'.
      CONCATENATE or  ' ( PGMID = '''    pv_pgmid  ''''
                      ' AND OBJECT = '''  pv_object ''''
                      ' AND OBJ_NAME LIKE ' INTO pt_where.
      APPEND pt_where.
      CONCATENATE '''' pv_obj_name '''' ' ) ' INTO pt_where.
      APPEND pt_where.
      or = 'OR'.
  ENDCASE.

ENDFORM.                               " BUILD_SELECT_STATEMENT

*&---------------------------------------------------------------------*
*&      Form  PREPARE_VARIABLES
*&---------------------------------------------------------------------*
FORM prepare_variables USING    pv_obj_name
                                pv_flag.

  IF pv_obj_name IS INITIAL.
    pv_obj_name = '%'.
  ELSEIF pv_obj_name CA '*'.
    TRANSLATE pv_obj_name USING '*%'.
  ENDIF.

  IF pv_obj_name CA '%'.
    pv_flag = 'L'.
  ELSE.
    pv_flag = ' '.
  ENDIF.

ENDFORM.                               " PREPARE_VARIABLES

*&---------------------------------------------------------------------*
*&      Form  PREPARE_VARIABLES2
*&---------------------------------------------------------------------*
FORM prepare_variables2 USING    pv_pgmid     LIKE tadir-pgmid
                                 pv_object    LIKE tadir-object
                                 pv_obj_name  LIKE tadir-obj_name
                                 pv_flag.
  DATA: ls_ko100 LIKE ko100.

  IF pv_obj_name IS INITIAL.
    pv_obj_name = '%'.
  ELSEIF pv_obj_name CA '*'.
    TRANSLATE pv_obj_name USING '*%'.
  ENDIF.

  IF pv_obj_name CA '%'.
    pv_flag = 'L'.
  ELSE.
    pv_flag = ' '.
  ENDIF.

  READ TABLE gt_object_texts INTO ls_ko100
                             WITH KEY object = pv_object.

  IF sy-subrc = 0.
    pv_pgmid = ls_ko100-pgmid.
  ELSE.
    pv_flag = 'F'.                     "faulty entry
  ENDIF.

ENDFORM.                               " PREPARE_VARIABLES2

*&---------------------------------------------------------------------*
*&      Form  CHECK_TADIR
*&---------------------------------------------------------------------*
FORM check_tadir USING    pt_tadir                 LIKE tadir
                          pv_only_existing_objects TYPE c
                 CHANGING pv_ok                    TYPE c.

  DATA: ls_e071   LIKE e071,
        lv_result LIKE trpari-s_checked,
        lv_exist  TYPE strl_pari-flag.

  pv_ok = 'X'.

  MOVE-CORRESPONDING pt_tadir TO ls_e071.

  CALL FUNCTION 'TR_CHECK_TYPE'
    EXPORTING
      wi_e071   = ls_e071
    IMPORTING
      pe_result = lv_result.

  IF lv_result NA 'LT'.
    pv_ok = ' '.
  ELSE.
*   Check if object is part of DDL source => no transport allowed
*   See note 1965044
    IF (   ls_e071-object = 'VIEW'
        OR ls_e071-object = 'STOB'
        OR ls_e071-object = 'DDLS' ).
      CALL METHOD cl_dd_ddl_utilities=>transport_is_allowed
        EXPORTING
          objname = ls_e071-obj_name
          objtype = ls_e071-object
        RECEIVING
          allowed = pv_ok.
    ENDIF.

    IF pv_only_existing_objects = 'X' AND pv_ok = 'X'.
      CALL FUNCTION 'CHECK_EXIST'
        EXPORTING
          i_tadir              = pt_tadir
        IMPORTING
          e_exist              = lv_exist
        EXCEPTIONS
          tr_no_check_function = 1.
      IF sy-subrc = 0 AND lv_exist = ' '.
        pv_ok = ' '.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                               " CHECK_TADIR
*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST_FOR_FIELD
*&---------------------------------------------------------------------*
FORM value_request_for_field USING    pv_field TYPE c.

  DATA: lt_dynpread LIKE dynpread   OCCURS 0 WITH HEADER LINE,
        lv_program  LIKE d020s-prog,
        lv_dynpro   LIKE d020s-dnum.

  lv_program     = sy-repid.
  lv_dynpro      = sy-dynnr.

  CASE pv_field.
*---F4 for programs
    WHEN 'OBJNAME1'.
      lt_dynpread-fieldname  = 'OBJNAME1'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname1 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_PROGRAM'
        EXPORTING
          object = objname1
        IMPORTING
          result = objname1.

*---F4 for function groups
    WHEN 'OBJNAME2'.
      lt_dynpread-fieldname  = 'OBJNAME2'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname2 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_FUNCTIONGROUP'
        EXPORTING
          object = objname2
        IMPORTING
          result = objname2.

*---F4 for classes
    WHEN 'OBJNAME7'.
      lt_dynpread-fieldname  = 'OBJNAME7'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname7 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_CLASS'
        EXPORTING
          object = objname7
        IMPORTING
          result = objname7.

*---F4 for tables
    WHEN 'OBJNAME3'.
      lt_dynpread-fieldname  = 'OBJNAME3'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname3 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_DD_TABLES'
        EXPORTING
          object = objname3
        IMPORTING
          result = objname3.

*---F4 for views
    WHEN 'OBJNAME4'.
      lt_dynpread-fieldname  = 'OBJNAME4'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname4 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_DD_VIEW'
        EXPORTING
          object = objname4
        IMPORTING
          result = objname4.

*---F4 for table type
    WHEN 'OBJNAME6'.
      lt_dynpread-fieldname  = 'OBJNAME6'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname6 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_DD_TABLE_TYPE'
        EXPORTING
          object = objname6
        IMPORTING
          result = objname6.

*---F4 for data element
    WHEN 'OBJNAME5'.
      lt_dynpread-fieldname  = 'OBJNAME5'.
      APPEND lt_dynpread.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = lv_program
          dynumb     = lv_dynpro
        TABLES
          dynpfields = lt_dynpread
        EXCEPTIONS
          OTHERS     = 1.
      IF sy-subrc = 0.
        READ TABLE lt_dynpread INDEX 1.
        IF sy-subrc = 0.
          objname5 = lt_dynpread-fieldvalue.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'F4_DD_DATA_ELEMENT'
        EXPORTING
          object = objname5
        IMPORTING
          result = objname5.


  ENDCASE.
ENDFORM.                               " VALUE_REQUEST_FOR_FIELD
