class ZCL_Z_CODE_MIGRATION_DPC_EXT definition
  public
  inheriting from ZCL_Z_CODE_MIGRATION_DPC
  create public .

public section.
protected section.

  methods ZTADIRSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_Z_CODE_MIGRATION_DPC_EXT IMPLEMENTATION.


  METHOD ztadirset_get_entityset.

    TRY.
        CALL METHOD zcl_pwc_codemigrate_util=>get_all_objects
          RECEIVING
            rt_tadir = et_entityset.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
