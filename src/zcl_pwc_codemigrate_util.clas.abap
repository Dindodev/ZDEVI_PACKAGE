class ZCL_PWC_CODEMIGRATE_UTIL definition
  public
  final
  create public .

public section.

  types:
    tty_devclass TYPE SORTED TABLE OF tdevc WITH UNIQUE KEY devclass .

  class-data T_TADIR type ZTT_ZTADIR .

  class-methods GET_ALL_OBJECTS
    returning
      value(RT_TADIR) type ZTT_ZTADIR .
  class-methods GET_ALL_DEV_PACKAGES
    returning
      value(RT_DEVCLASS) type TTY_DEVCLASS .
protected section.
private section.

  class-data T_DEVCLASS type TTY_DEVCLASS .
ENDCLASS.



CLASS ZCL_PWC_CODEMIGRATE_UTIL IMPLEMENTATION.


  METHOD get_all_dev_packages.

    TYPES devclass_range TYPE RANGE OF tdevc-devclass.

    DATA(devclass_range) = VALUE devclass_range(
      ( sign = 'I' option = 'CP' low = 'Z*') ).

    SELECT * FROM tdevc INTO TABLE rt_devclass WHERE devclass IN devclass_range.

  ENDMETHOD.


  METHOD get_all_objects.

    DATA: lt_rspar TYPE TABLE OF rsparams,
          gt_tadir TYPE TABLE OF tadir.

    DATA(zdevclass) = get_all_dev_packages( ).

    LOOP AT zdevclass ASSIGNING FIELD-SYMBOL(<fs_zdevclasS>).
      APPEND INITIAL LINE TO lt_rspar ASSIGNING FIELD-SYMBOL(<fs_rspar>).
      <fs_rspar>-selname = 'DEVCLASS'.
      <fs_rspar>-kind = 'S'.
      <fs_rspar>-option = 'EQ'.
      <fs_rspar>-sign = 'I'.
      <fs_rspar>-low = <fs_zdevclass>-devclass.
    ENDLOOP.

    APPEND INITIAL LINE TO lt_rspar ASSIGNING <fs_rspar>.
    <fs_rspar>-selname = 'SHOW_ALL'.
    <fs_rspar>-kind = 'P'.
    <fs_rspar>-low = 'X'.

    APPEND INITIAL LINE TO lt_rspar ASSIGNING <fs_rspar>.
    <fs_rspar>-selname = 'ONLY_EXI'.
    <fs_rspar>-kind = 'P'.
    <fs_rspar>-low = 'X'.

    SUBMIT zpwc_codemigrate_rswbo010 WITH SELECTION-TABLE lt_rspar
                AND RETURN.

    IMPORT gt_tadir TO gt_tadir FROM MEMORY ID 'PWC_CM_MEM1'.

    LOOP AT gt_tadir ASSIGNING FIELD-SYMBOL(<fs_tadir>).
      APPEND INITIAL LINE TO rt_tadir ASSIGNING FIELD-SYMBOL(<fs_ztadir>).
      MOVE-CORRESPONDING <fs_tadir> TO <fs_ztadir>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
