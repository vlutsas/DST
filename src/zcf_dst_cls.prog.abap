*&---------------------------------------------------------------------*
*&  Include  zcf_data_scramble_tool_cls
*&---------------------------------------------------------------------*
FIELD-SYMBOLS:
  <gfs_tab>    TYPE ANY TABLE
  ,<gfs_tab_db>  TYPE ANY TABLE.

DATA: fgbg_listbox TYPE char1 VALUE '1'.

CLASS lcl_dst_app DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    METHODS:
      constructor
      .

    CLASS-METHODS:
                       class_constructor
                      ,run
                      ,pbo
                      ,pai IMPORTING iv_code TYPE sy-ucomm
                      .

  PRIVATE SECTION.

    TYPES:
            BEGIN OF gty_alv
             ,tabname TYPE tabname
             ,fieldname TYPE fieldname
             ,tab_ddtext TYPE as4text
             ,field_ddtext TYPE as4text
             ,key_exist TYPE char4
             ,cell_color TYPE lvc_t_scol
           ,END OF gty_alv.

    TYPES:
           gty_dst_fields TYPE STANDARD TABLE OF zcft_dst_fields WITH DEFAULT KEY
          ,gty_percentage(3) TYPE p DECIMALS 3
          .


    CONSTANTS:
                mc_title                TYPE string               VALUE 'DATA SCRAMBLING TOOL' ##NO_TEXT
               ,mc_balobj_d_zdst        TYPE balobj_d             VALUE 'ZDST' ##NO_TEXT
               ,mc_batchjob_event       TYPE btceventid           VALUE 'DST_START' ##NO_TEXT
               ,mc_key_exist            TYPE lvc_s_sort-fieldname VALUE 'KEY_EXIST' ##NO_TEXT
               ,mc_zcft_dst_fields      TYPE e071k-objname        VALUE 'ZCFT_DST_FIELDS' ##NO_TEXT
               ,mc_zcft_dst_keys        TYPE e071k-objname        VALUE 'ZCFT_DST_KEYS' ##NO_TEXT
               ,mc_fieldname            TYPE lvc_fname            VALUE 'FIELDNAME' ##NO_TEXT
               ,mc_tabname              TYPE lvc_fname            VALUE 'TABNAME' ##NO_TEXT
               ,mc_tab_ddtext           TYPE lvc_s_sort-fieldname VALUE 'TAB_DDTEXT' ##NO_TEXT
               ,mc_field_ddtext         TYPE lvc_s_fcat-fieldname VALUE 'FIELD_DDTEXT' ##NO_TEXT
               ,mc_as4text              TYPE lvc_s_fcat-domname   VALUE 'AS4TEXT' ##NO_TEXT
               ,mc_char                 TYPE lvc_s_fcat-datatype  VALUE 'CHAR' ##NO_TEXT
               ,mc_intlen               TYPE lvc_s_fcat-intlen    VALUE 60
               ,mc_pfstatus             TYPE string               VALUE 'ZDISPLAY' ##NO_TEXT
               ,mc_zcf_dst_config_view  TYPE vcl_name             VALUE 'ZCF_DST_CONFIG_VIEW' ##NO_TEXT
               ,mc_repid                TYPE indx_srtfd           VALUE sy-repid
               .

    CLASS-DATA:
                 mv_ok_code             TYPE sy-ucomm
                ,mt_fieldcat            TYPE lvc_t_fcat
                ,mo_app                 TYPE REF TO lcl_dst_app
                ,mo_container           TYPE REF TO cl_gui_custom_container
                ,mo_alv                 TYPE REF TO cl_gui_alv_grid
                ,mt_fields_db           TYPE STANDARD TABLE OF zcft_dst_fields
                ,mt_fields_int_alv      TYPE STANDARD TABLE OF gty_alv
                ,mt_keys_db             TYPE STANDARD TABLE OF zcft_dst_keys
                ,mt_keys_int            TYPE STANDARD TABLE OF zcft_dst_keys
                ,mv_screen_status       TYPE string
                ,mv_not_valid           TYPE xfeld
                ,mv_system_client_role  TYPE cccategory
                ,mv_log_handle          TYPE balloghndl
                ,mt_list                TYPE vrm_values
                ,mv_ts_start            TYPE tzonref-tstamps
                ,mv_bal_log_created     TYPE xfeld
                ,mv_percentage_step     TYPE gty_percentage
                ,mv_percentage          TYPE gty_percentage
                .

    METHODS:
             create_alv_data
            ,set_status
                        IMPORTING iv_status TYPE string
            ,on_data_changed
                        FOR EVENT data_changed OF cl_gui_alv_grid
                        IMPORTING er_data_changed
                                  e_onf4
                                  e_onf4_before
                                  e_onf4_after
                                  e_ucomm
                                  sender
            ,on_double_click
                        FOR EVENT double_click OF cl_gui_alv_grid
                        IMPORTING e_row
                                  e_column
            ,on_data_changed_finished
                        FOR EVENT data_changed_finished OF cl_gui_alv_grid
                        IMPORTING e_modified
            ,leave_screen
            .

    CLASS-METHODS:
                  build_screen_object
                 ,build_fieldcatalog
                 ,get_system_parameters
                 ,read_table_data   IMPORTING iv_tabname TYPE tabname
                 ,scramble_table_column
                                    IMPORTING iv_tabname TYPE tabname
                                              iv_fieldname TYPE fieldname
                                    CHANGING  cv_field_count TYPE i OPTIONAL
                 ,fetch_template_data
                 ,compare_tables
                                    IMPORTING
                                      it_itab_new TYPE table " itab with current data
                                      it_itab_old TYPE table " itab with old data
                                    EXPORTING
                                      et_insert TYPE table " itab with new data
                                      et_update TYPE table " itab with changed data
                                      et_delete TYPE table " itab with deleted data
                 ,create_transport_request
                 ,save_template_data
                 ,start_scrambling
                 ,read_dd_descriptions
                 ,format_alv_table
                                   RETURNING VALUE(rt_mt_fields_int) TYPE gty_dst_fields
                 ,update_key_column
                 ,edit_key
                                   IMPORTING e_row TYPE lvc_s_row
                 ,run_se16n
                                   IMPORTING e_row TYPE lvc_s_row
                 ,rsds_trange_to_range
                                   IMPORTING iv_tabname TYPE tabname
                                   RETURNING VALUE(rt_field_ranges) TYPE rsds_trange
                 ,create_where     IMPORTING iv_tabname TYPE tabname
                                   RETURNING VALUE(rt_where) TYPE rsdmd_t_where
                 ,bal_log_create
                 ,bal_log_msg_add IMPORTING is_msg TYPE bal_s_msg
                 ,bal_log_display
                 ,bal_log_save
                 ,bal_log_display_all
                 ,add_percentage_step
                 ,update_db_table
                                  IMPORTING iv_tabname TYPE tabname
                 ,check_template_on_keys
                                  RETURNING VALUE(rt_key_exist) TYPE xfeld
                 ,get_tables_list
                                  RETURNING VALUE(rt_tab_data) TYPE rsar_t_tabnames
                 .



ENDCLASS.

CLASS lcl_dst_app IMPLEMENTATION.

  METHOD constructor.

    mt_list = VALUE vrm_values( ( key = '1' text = 'FOREGROUND'(038) )      "listbox
                                ( key = '2' text = 'BACKGROUND'(039) )  ).

    IF sy-batch = abap_true. "background job
      fgbg_listbox = 2.
      start_scrambling(  ).
    ELSE.
      create_alv_data( ).
      CALL SCREEN 100.
    ENDIF.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA:

      lv_dbtabname TYPE tabname16,
      lv_fieldname TYPE name_feld.

    LOOP AT er_data_changed->mt_mod_cells INTO DATA(ls_mod_cel)
      GROUP BY ( row_id = ls_mod_cel-row_id
                 size = GROUP SIZE
                 index = GROUP INDEX )
                 ASCENDING
                 REFERENCE INTO DATA(group_ref).

      LOOP AT GROUP group_ref ASSIGNING FIELD-SYMBOL(<s_mod_cell>).

        er_data_changed->get_cell_value(
          EXPORTING
            i_row_id    = <s_mod_cell>-row_id
            i_fieldname = mc_tabname
          IMPORTING
            e_value     = lv_dbtabname ).


        SELECT SINGLE tabname       "check if table exists
          FROM dd02l
          INTO @DATA(lv_tabname)
          WHERE
              tabname = @lv_dbtabname.

        IF sy-subrc IS NOT INITIAL.

          er_data_changed->add_protocol_entry(
                                                EXPORTING
                                                  i_msgid     = mc_balobj_d_zdst
                                                  i_msgno     = '006'
                                                  i_msgty     = 'E'
                                                  i_msgv1     = 'incorrect table name'(025)
                                                  i_fieldname = <s_mod_cell>-fieldname
                                                  i_row_id    = <s_mod_cell>-row_id ).

          mv_not_valid = abap_true.

        ENDIF.

        er_data_changed->get_cell_value(
                                          EXPORTING
                                            i_row_id    = <s_mod_cell>-row_id
                                            i_fieldname = mc_fieldname
                                          IMPORTING
                                            e_value     = lv_fieldname ).

        SELECT SINGLE keyflag "Check if key exists
          FROM dd03l
          INTO @DATA(lv_keyflag)
          WHERE
              tabname = @lv_dbtabname
          AND fieldname = @lv_fieldname.

        IF sy-subrc IS NOT INITIAL.

          er_data_changed->add_protocol_entry(
                                                EXPORTING
                                                  i_msgid     = mc_balobj_d_zdst
                                                  i_msgno     = '006'
                                                  i_msgty     = 'E'
                                                  i_msgv1     = 'incorrect field name'(026)
                                                  i_fieldname = mc_fieldname
                                                  i_row_id    = <s_mod_cell>-row_id ).

          mv_not_valid = abap_true.

        ELSEIF lv_keyflag = abap_true.

          er_data_changed->add_protocol_entry(
                                                EXPORTING
                                                  i_msgid     = mc_balobj_d_zdst
                                                  i_msgno     = '006'
                                                  i_msgty     = 'W'
                                                  i_msgv1     = lv_fieldname && ` - ` && 'field is a table KEY'(027)
                                                  i_fieldname = mc_fieldname
                                                  i_row_id    = <s_mod_cell>-row_id ).

          CLEAR mv_not_valid.

        ELSE.
          CLEAR mv_not_valid.
        ENDIF.

        RETURN.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    CASE e_column-fieldname.

      WHEN mc_key_exist.
        edit_key( e_row = e_row ).

      WHEN mc_tabname.
        run_se16n( e_row = e_row ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD run_se16n.

    DATA:
          lt_se16n_seltab TYPE STANDARD TABLE OF se16n_seltab
         ,lt_output_fields_wkeys TYPE STANDARD TABLE OF se16n_output
         ,lt_output_fields TYPE STANDARD TABLE OF se16n_output
         ,lt_dd03p TYPE STANDARD TABLE OF dd03p
         .

    lt_se16n_seltab = VALUE #( FOR m1 IN mt_keys_int
                                   WHERE ( tabname = mt_fields_int_alv[ e_row-index ]-tabname )
                                             ( field = m1-field
                                               sign = m1-sign
                                               option = m1-zoption
                                               low = m1-low
                                               high = m1-high ) ).

    DATA(lv_fields_alv) = format_alv_table( ).

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = mt_fields_int_alv[ e_row-index ]-tabname    " Name of the Table to be Read
*       state         = 'A'    " Read Status of the Table
*       langu         = ' '    " Language in which Texts are Read
*      IMPORTING
*       gotstate      =     " Status in which Reading took Place
*       dd02v_wa      =     " Table Header
*       dd09l_wa      =     " Technical Settings of the Table
      TABLES
        dd03p_tab     = lt_dd03p   " Table Fields
*       dd05m_tab     =     " Foreign Key Fields of the Table
*       dd08v_tab     =     " Foreign Keys of the Table
*       dd12v_tab     =     " Table Indexes
*       dd17v_tab     =     " Index Fields of the Table
*       dd35v_tab     =     " Header of the Search Help Assignments of the Table
*       dd36m_tab     =     " Allocations of the Search Help Assignments of the Table
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lt_output_fields_wkeys = VALUE #( FOR m2 IN lt_dd03p
                                    WHERE ( keyflag = abap_true )
                                              ( field = m2-fieldname
                                                                  ) ).


    lt_output_fields = VALUE #( FOR m3 IN lv_fields_alv
                                    WHERE ( tabname = mt_fields_int_alv[ e_row-index ]-tabname )
                                              ( field = m3-fieldname
                                                                  ) ) .
    APPEND LINES OF lt_output_fields TO lt_output_fields_wkeys.

    CALL FUNCTION 'SE16N_INTERFACE'
      EXPORTING
        i_tab            = mt_fields_int_alv[ e_row-index ]-tabname  " Table
*       i_edit           = ' '    " Single-Character Indicator
*       i_sapedit        = ' '    " Single-Character Indicator
*       i_no_txt         = ' '    " Do not select any texts
        i_max_lines      = 500    " Maximum Number of Hits
*       i_line_det       = ' '    " Only determine number of entries found
*       i_display        = 'X'    " Display Results
*       i_clnt_spez      = ' '    " Client-dependent selection
*       i_clnt_dep       = ' '    " Table is client-dependent
*       i_variant        = ' '    " Layout
*       i_old_alv        = ' '    " Not ALV grid
*       i_checkkey       = ' '    " No foreign key check
*       i_tech_names     = ''    " Technical Name as Column Heading
*       i_cwidth_opt_off = ' '    " Deactivate Column Width Optimization
*       i_scroll         = ' '    " KeyColumnsScrolab.
*       i_no_convexit    = ' '    " Do not use conversion for output
*       i_layout_get     = ' '    " Get last layout automatically
*       i_add_field      =     " Additional Field for RETURN
*       i_add_fields_on  =     " Use Field RETURN
*       i_uname          =     " User Name
        i_hana_active    = abap_true "SPACE    " SAP HANA Mode Active
*       i_dbcon          = SPACE    " Logical Name for a Database Connection
*       i_ojkey          = SPACE    " Outer Join Definition
*       i_display_all    = ' '    " Selection of all Records
*       i_extract_read   = SPACE    " Display Extract
*       i_extract_write  = SPACE    " Save Extract
*       i_extract_name   = SPACE    " SE16N: Display Variant of Initial Screen
*       i_extract_uname  = SPACE    " User Name of Extract
*       i_temperature    =     " Data Filter Value for Data Aging
*       i_temperature_cold    = SPACE    " Read All Cold Data
*       i_session_control     =     " Session Control for Data Aging
*        IMPORTING
*       e_line_nr        =     " No. of entries found
*       e_dref           =     " Pointer to the Results List
*       et_fieldcat      =     " Output List Field Catalog
      TABLES
        it_selfields     = lt_se16n_seltab    " Table Display: Selection Criteria
        it_output_fields = lt_output_fields_wkeys     " Table Display: Output Fields
*       it_or_selfields  =     " Transfer Table for Multiple Selection Criteria
*       it_callback_events    =     " Event Table for SE16N
*       it_add_up_curr_fields =     " Table Display: CURR Fields to be Added by Row
*       it_add_up_quan_fields =     " Table Display: QUAN Fields to be Added by Row
*       it_sum_up_fields =     " Totaling Fields
*       it_group_by_fields    =     " Grouping Fields
*       it_order_by_fields    =     " Sort fields
*       it_aggregate_fields   =     " Aggregation Fields
*       it_toplow_fields =     " Top Bottom Fields
*       it_sortorder_fields   =     " Sort Sequence
*        CHANGING
*       it_and_selfields =     " SE16N: Table Type for AND Link
      EXCEPTIONS
        no_values        = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.

  METHOD edit_key.

    DATA:
               lv_selid TYPE  rsdynsel-selid
              ,lt_table_tab TYPE TABLE OF rsdstabs
              ,lt_cond_tab TYPE rsds_twhere
              ,lt_keys_int_new TYPE STANDARD TABLE OF zcft_dst_keys
              .

    TRY.

        DATA(lv_tabname) = mt_fields_int_alv[ e_row-index ]-tabname.  "table name of currently clicked line

      CATCH cx_sy_itab_line_not_found.
        LEAVE PROGRAM.
    ENDTRY.

    APPEND VALUE #( prim_tab = lv_tabname ) TO lt_table_tab. "Table name

    DATA(lt_field_ranges) = rsds_trange_to_range( iv_tabname = lv_tabname ).
    DATA(lt_field_tab) = VALUE rsdsfields_t( FOR <wa_fields> IN mt_keys_int
                                               ( tablename = <wa_fields>-tabname
                                                 fieldname = <wa_fields>-field ) ).

    DELETE ADJACENT DUPLICATES FROM lt_field_tab.  "rel2

    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'T'    " Type of field list
*       expressions              = lt_expressions    " Initial selections
        field_ranges_int         = lt_field_ranges   " Initial Selections in Form of Ranges Tables
*       field_groups_key         =     " Selection view key
*       restriction              =     " Option restrictions of individual selections
*       alv                      =     " Internal Use
*       curr_quan_prog           = SY-CPROG    " Program for fetching currency reference
*       curr_quan_relation       =     " Assignment quantity field -> currency key field.
      IMPORTING
        selection_id             = lv_selid    " Identification for FREE_SELECTIONS_DIALOG
*       where_clauses            =     " Selections in form of WHERE clauses
*       expressions              =     " Initial selections
*       field_ranges             =     " Selections in form of RANGES tables
*       number_of_active_fields  =     " Number of fields with selections
      TABLES
        tables_tab               = lt_table_tab    " Desired tables
*       tabfields_not_display    =     " Child T: Hide Fields
        fields_tab               = lt_field_tab    " Preselect fields
*       field_desc               =     " Description of non-Dictionary fields
*       field_texts              =     " Own selection texts
*       events                   =     " Events for which control is required
*       event_fields             =     " Fields for field-specific events
*       fields_not_selected      =     " Kind F: Fields not initially selected
*       no_int_check             =     " Fields for which the interval check is to be deactivated
*       alv_qinfo                =     " Internal Field
      EXCEPTIONS
        fields_incomplete        = 1
        fields_no_join           = 2
        field_not_found          = 3
        no_tables                = 4
        table_not_found          = 5
        expression_not_supported = 6
        incorrect_expression     = 7
        illegal_kind             = 8
        area_not_found           = 9
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13
        dup_field                = 14
        field_no_type            = 15
        field_ill_type           = 16
        dup_event_field          = 17
        node_not_in_ldb          = 18
        area_no_field            = 19
        OTHERS                   = 20.

    IF sy-subrc <> 0.
      MESSAGE 'keys selection initialization error'(024) TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    IF mo_alv->is_ready_for_input( ) <> '1'.           "display only
      DATA(iv_just_display) = abap_true.
      MESSAGE 'enter change mode to edit keys'(006) TYPE 'S'.
    ELSE.
      CLEAR iv_just_display.
    ENDIF.

    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = lv_selid    " The SELID returned by FREE_SELECTIONS_INIT
*       title           = SPACE     " Selection screen title
*       frame_text      = SPACE    " Frame text on selection screen
*       status          =     " Status number (see long text)
        as_window       = abap_true    " X: Selection screen as dialog box
*       start_row       =     " Dialog box: Initial row
*       start_col       =     " Dialog box: Initial column
*       no_intervals    = SPACE    " Thin selection screen (no HIGH field)
        just_display    = iv_just_display    " X: Display Mode
*       pfkey           =     " Individual GUI status (status + program).
*       alv             = SPACE    " Internal use
*       tree_visible    = abap_true    " Do you want to display the field selection tree?
*       diag_text_1     =     " Replacement text:"Deleting dynamic selections"
*       diag_text_2     =     " Replacement text:"Changing dynamic selections"
*       warning_title   =     " Warning dialob box title
*       as_subscreen    = SPACE    " (For internal use only) Integrate Dynamic Selections as Subs
*       no_frame        =
      IMPORTING
        where_clauses   = lt_cond_tab   " Selections in form of WHERE clauses
*       expressions     =     " Selections in form of logical expressions
        field_ranges    = lt_field_ranges    " Selections in form of RANGES tables
*       number_of_active_fields =     " Number of fields with selections
      TABLES
        fields_tab      = lt_field_tab    " Returns selected fields
*       fcode_tab       =     " Internal use
*       fields_not_selected     =     " Kind 'F': List of deselected fields.
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.

    IF sy-subrc <> 0 AND sy-subrc <> 2.
      MESSAGE 'keys selection creation error'(022) TYPE 'I'.
      LEAVE PROGRAM.
    ENDIF.

    DELETE mt_keys_int WHERE tabname = lv_tabname.

    IF lt_field_ranges IS NOT INITIAL.

      lt_keys_int_new = VALUE #(
                                  FOR <wa_tab> IN lt_field_ranges          "selected fields to DST table
                                    FOR <wa_field> IN <wa_tab>-frange_t
                                      FOR <wa_range> IN <wa_field>-selopt_t
                                          ( mandt = sy-mandt
                                            tabname = <wa_tab>-tablename
                                            field = <wa_field>-fieldname
                                            sign = <wa_range>-sign
                                            zoption = <wa_range>-option
                                            low = <wa_range>-low
                                            high = <wa_range>-high ) ).

*      SORT lt_keys_int_new.                               "rel2
*      DELETE ADJACENT DUPLICATES FROM lt_keys_int_new.    "rel2

      APPEND LINES OF lt_keys_int_new TO mt_keys_int.

    ENDIF.

    update_key_column(  ).
    mo_alv->refresh_table_display(
                                   EXPORTING i_soft_refresh = abap_true ).

  ENDMETHOD.                           "handle_double_click

  METHOD run.

    CREATE OBJECT mo_app.

  ENDMETHOD.

  METHOD class_constructor.

    get_system_parameters( ).
    fetch_template_data( ).

    IF sy-batch <> abap_true. "background job
      build_screen_object( ).
      build_fieldcatalog( ).
    ENDIF.

  ENDMETHOD.

  METHOD build_screen_object.

    CREATE OBJECT mo_container
      EXPORTING
*       PARENT                      =
        container_name              = 'CUSTOM'
*       STYLE                       =
*       LIFETIME                    = lifetime_default
*       REPID                       =
*       DYNNR                       =
*       NO_AUTODEF_PROGID_DYNNR     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT mo_alv
      EXPORTING
*       I_SHELLSTYLE      = 0
*       I_LIFETIME        =
        i_parent          = mo_container
*       I_APPL_EVENTS     = space
*       I_PARENTDBG       =
*       I_APPLOGPARENT    =
*       I_GRAPHICSPARENT  =
*       I_NAME            =
*       I_FCAT_COMPLETE   = SPACE
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.

  METHOD fetch_template_data.
    "method fetches the template for scrambling (tables/fields to be scrambles and what keys should be used to fetch this data)

    SELECT *                                    "Fields
      FROM zcft_dst_fields
      INTO TABLE @mt_fields_db
      .                                                 "#EC CI_NOWHERE

    mt_fields_int_alv = CORRESPONDING #( mt_fields_db ).  "filling the table will be used for ALV output/edit.

    SELECT *                                     "Keys
      FROM zcft_dst_keys
      INTO TABLE mt_keys_db
      .                                                 "#EC CI_NOWHERE

    mt_keys_int = mt_keys_db.                     "filling the table will be used for ALV output/edit in KEYS column.
    read_dd_descriptions(  ).                     "read table/fields text descriptions

  ENDMETHOD.

  METHOD create_alv_data.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Layout
    DATA:
          ls_layout TYPE lvc_s_layo.
    "handlers
    SET HANDLER mo_app->on_double_click FOR mo_alv.
    SET HANDLER mo_app->on_data_changed FOR mo_alv.
    SET HANDLER mo_app->on_data_changed_finished FOR mo_alv.

    mo_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).  "register ENTER to raise event DATA_CHANGED.
    mo_alv->set_ready_for_input( i_ready_for_input = '0' ).                     "ready for input

    "layout
    ls_layout-zebra = abap_true.
    ls_layout-grid_title = 'TEMPLATE  EDITOR'.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-ctab_fname = 'CELL_COLOR'.

    "sorting
    DATA(lt_sort) = VALUE lvc_t_sort(
                                       ( spos = 1 fieldname = mc_tabname up = abap_true )
                                       ( spos = 2 fieldname = mc_fieldname  up = abap_true )
                                       ( spos = 3 fieldname = mc_tab_ddtext up = abap_true )
                                       ( spos = 4 fieldname = mc_key_exist up = abap_true ) ).

    mo_alv->set_table_for_first_display(
                                          EXPORTING
*                                           I_BUFFER_ACTIVE    =
                                            i_bypassing_buffer = abap_true
*                                           I_CONSISTENCY_CHECK           =
*                                           I_STRUCTURE_NAME   =
*                                           IS_VARIANT         =
*                                           I_SAVE             =
*                                           I_DEFAULT          = ‘X’
                                            is_layout          = ls_layout
*                                           IS_PRINT           =
*                                           IT_SPECIAL_GROUPS  =
*                                           IT_TOOLBAR_EXCLUDING          =
*                                           IT_HYPERLINK       =
*                                           IT_ALV_GRAPHICS    =
*                                           IT_EXCEPT_QINFO    =
*                                           IR_SALV_ADAPTER    =
                                          CHANGING
                                            it_outtab          = mt_fields_int_alv
                                            it_fieldcatalog    = mt_fieldcat
                                            it_sort            = lt_sort
*                                           IT_FILTER          =
*                                         EXCEPTIONS
*                                           invalid_parameter_combination = 1
*                                           program_error      = 2
*                                           too_many_lines     = 3
*                                           OTHERS             = 4
                                          ).
    IF sy-subrc <> 0.
*                                             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                                                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.

  METHOD build_fieldcatalog.

    DATA:
          lt_fieldcat_temp TYPE lvc_t_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
*       i_buffer_active        =     " Buffer active
        i_structure_name       = mc_zcft_dst_fields   " Structure name (structure, table, view)
*       i_client_never_display = abap_true    " Hide client fields
*       i_bypassing_buffer     =     " Ignore buffer while reading
*       i_internal_tabname     =    " Table Name
      CHANGING
        ct_fieldcat            = lt_fieldcat_temp  " Field Catalog with Field Descriptions
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    mt_fieldcat = VALUE #( FOR <wa> IN lt_fieldcat_temp
                                 ( VALUE #( BASE CORRESPONDING #( <wa> ) edit  = abap_true ) ) ).     "Edit mode ON

    TRY.

        CLEAR mt_fieldcat[ fieldname = mc_tabname ]-key.
        CLEAR mt_fieldcat[ fieldname = mc_tabname ]-emphasize.
        mt_fieldcat[ fieldname = mc_tabname ]-style = alv_style_font_bold BIT-OR alv_style_color_total.

        CLEAR mt_fieldcat[ fieldname = mc_fieldname ]-key.
        mt_fieldcat[ fieldname = mc_fieldname ]-style = alv_style_font_italic BIT-OR alv_style_color_total.

        CLEAR mt_fieldcat[ fieldname = mc_key_exist ]-edit.    "Edit
        mt_fieldcat[ fieldname = mc_key_exist ]-reptext = 'Key'(028).    "Title for column 'Key'
        mt_fieldcat[ fieldname = mc_key_exist ]-col_pos = 6.

      CATCH cx_sy_itab_line_not_found.

    ENDTRY.

    "additional description fields
    INSERT VALUE lvc_s_fcat(
                                            fieldname = mc_tab_ddtext
                                            col_pos = 4
                                            datatype = mc_char
                                            inttype = 'C'
                                            intlen = mc_intlen
                                            reptext = 'Table description'(029)
                                            domname = mc_as4text
                                            dd_outlen = mc_intlen

                                                                                    ) INTO TABLE mt_fieldcat.
    INSERT VALUE lvc_s_fcat(
                                            fieldname = mc_field_ddtext
                                            col_pos = 5
                                            datatype = mc_char
                                            inttype = 'C'
                                            intlen = mc_intlen
                                            reptext = 'Field description'(034)
                                            domname = mc_as4text
                                            dd_outlen = mc_intlen

                                                                                    ) INTO TABLE mt_fieldcat.

  ENDMETHOD.

  METHOD pbo.

    IF mo_app IS INITIAL.
      RETURN.
    ENDIF.

    mo_app->set_status( mc_pfstatus ).

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'FGBG_LISTBOX'    " Name of Value Set
        values          = mt_list
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.     "PBO

  METHOD set_status.

    mv_screen_status = iv_status.
    SET PF-STATUS mv_screen_status.
    SET TITLEBAR 'TITLE' OF PROGRAM syst-cprog WITH mc_title.

  ENDMETHOD.

  METHOD pai.

    DATA:
          lv_answer TYPE char1.

    mo_app->mv_ok_code = iv_code.

    CASE mo_app->mv_ok_code.

      WHEN 'INFO'.

        DATA(lv_object) = CONV doku_obj( syst-cprog ).

        CALL FUNCTION 'DOCU_CALL'
          EXPORTING
*           cmod_entrance     = SPACE    " Access via transaction CMOD
            displ      = abap_true    " Display/maintenance mode
            displ_mode = '2'    " Display mode (1=Editor,2=Formatted)
*           dynpro_for_thlpf  = SPACE
*           fdname_for_thlpf  = SPACE
            id         = 'RE'   " Module class
            langu      = sy-langu   " Language
            object     = lv_object    " Documentation module name
*           program_for_thlpf = SPACE
*           shorttext  = SPACE
*           typ        = 'E'    " Documentation type
*           suppress_edit     = SPACE
*           use_sec_langu     = SPACE
*           force_editor      = SPACE
*           extension_mode    = SPACE    " Enhancement mode
*           template_id       = SPACE
*           template_object   = SPACE
*           template_typ      = SPACE
*           use_note_template = SPACE
*           display_shorttext = SPACE
*          IMPORTING
*           savetext   =
*           exit_code  =     " Document editor RETURN type
*          EXCEPTIONS
*           wrong_name = 1
*           others     = 2
          .
        IF sy-subrc <> 0.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN '&F03' OR '&F12' OR '&F15'.

        SORT: mt_fields_db, mt_keys_db, mt_keys_int.

        IF format_alv_table( ) <> mt_fields_db
        OR mt_keys_int <> mt_keys_db.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Leave transaction'(035) "SPACE    " Title of dialog box
*             diagnose_object       = SPACE    " Diagnosis text (maintain via SE61)
              text_question         = 'Template data changes will be not saved. Leave transaction ?'(036)   " Question text in dialog box
              text_button_1         = 'Leave'(037)    " Text on the first pushbutton
*             icon_button_1         = SPACE    " Icon on first pushbutton
*             text_button_2         = 'No'    " Text on the second pushbutton
              icon_button_2         = space    " Icon on second pushbutton
*             default_button        = '1'    " Cursor position
              display_cancel_button = space    " Button for displaying cancel pushbutton
*             userdefined_f1_help   = SPACE    " User-Defined F1 Help
*             start_column          =     " Column in which the POPUP begins
*             start_row             =     " Line in which the POPUP begins
*             popup_type            =     " Icon type
*             iv_quickinfo_button_1 = SPACE    " Quick Info on First Pushbutton
*             iv_quickinfo_button_2 = SPACE    " Quick Info on Second Pushbutton
            IMPORTING
              answer                = lv_answer  " Return values: '1', '2', 'A'
*           TABLES
*             parameter             =     " Text transfer table for parameter in text
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF lv_answer EQ 1.
            mo_app->leave_screen( ).
          ENDIF.

        ELSE.

          mo_app->leave_screen( ).

        ENDIF.

      WHEN 'CHAN'.

        IF mv_system_client_role <> 'C'.
          MESSAGE 'please use customizing client + transport request for edit data template'(008) TYPE 'E'.
          RETURN.
        ENDIF.

        AUTHORITY-CHECK OBJECT 'Z_S_DST' ID 'ACTVT' FIELD '02'.

        IF sy-subrc NE 0.
          MESSAGE 'no authorization for scrambling template change'(016) TYPE 'E'.
          RETURN.
        ENDIF.

        mo_alv->check_changed_data(
           IMPORTING
             e_valid   = DATA(lt_e_valid)    " Entries are Consistent
*           CHANGING
*            c_refresh = lv_refresh   " Character Field of Length 1
        ).

        IF mv_not_valid IS NOT INITIAL.
          MESSAGE 'check the data'(015) TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF mo_alv->is_ready_for_input( ) = '0'.

          CALL FUNCTION 'ENQUEUE_ESINDX'
            EXPORTING
*             mode_indx      = 'E'    " Lock mode for table INDX
*             mandt          = SY-MANDT    " 01th enqueue argument
              relid          = 'ZD'   " 02th enqueue argument
              srtfd          = mc_repid   " 03th enqueue argument
              srtf2          = '0'   " 04th enqueue argument
*             x_relid        = SPACE    " Fill argument 02 with initial value?
*             x_srtfd        = SPACE    " Fill argument 03 with initial value?
*             x_srtf2        = SPACE    " Fill argument 04 with initial value?
*             _scope         = '2'
*             _wait          = SPACE
*             _collect       = ' '    " Initially only collect lock
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            MESSAGE w007(zdst) WITH sy-msgv1 DISPLAY LIKE 'A'.  "Template is currently edited by &

          ELSE.
            mo_alv->set_ready_for_input( i_ready_for_input = '1' ).
            CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
              EXPORTING
                percentage = ''
                text       = 'change mode ON'(032).

          ENDIF.

        ELSE.

          CALL FUNCTION 'DEQUEUE_ESINDX'
            EXPORTING
*             mode_indx = 'E'    " Lock mode for table INDX
*             mandt = SY-MANDT    " 01th enqueue argument
              relid = 'ZD'   " 02th enqueue argument
              srtfd = mc_repid   " 03th enqueue argument
              srtf2 = '0'   " 04th enqueue argument
*             x_relid   = SPACE    " Fill argument 02 with initial value?
*             x_srtfd   = SPACE    " Fill argument 03 with initial value?
*             x_srtf2   = SPACE    " Fill argument 04 with initial value?
*             _scope    = '3'
*             _synchron = SPACE    " Synchonous unlock
*             _collect  = ' '    " Initially only collect lock
            .

          mo_alv->set_ready_for_input( i_ready_for_input = '0' ).
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = ''
              text       = 'change mode OFF'(033).

        ENDIF.

      WHEN 'SAVE'.

        IF mo_alv->is_ready_for_input( ) = '1'.

          IF format_alv_table( ) <> mt_fields_db OR mt_keys_int <> mt_keys_db.
            save_template_data( ).
          ELSEIF mv_not_valid = abap_true.
            MESSAGE 'data was not saved'(005) TYPE 'E'.
          ELSE.
            MESSAGE 'no changes made'(003) TYPE 'I'.
          ENDIF.

        ELSE.
          MESSAGE 'enter change mode to save data'(004) TYPE 'E'.      "Enter edit mode to save data
          RETURN.
        ENDIF.

      WHEN 'VCONFIG'.

        CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
          EXPORTING
            viewcluster_name             = mc_zcf_dst_config_view     " View Cluster Name
*           start_object                 = '          '    " Initial Object in View Cluster
            maintenance_action           = 'S'    " Action (Display/Change/Transport: S/U/T/C)
*           read_kind                    = ' '    " Read Type (Complete/Subtree:' '/T)
*           show_selection_popup         = ' '    " Flag: Display Selection Conditions Dialog Box
*           corr_number                  = ' '    " Correction Number for Changes Made
*           no_warning_for_clientindep   = ' '    " Flag: No Warning for Cross-Client Objects
*           rfc_destination              = ' '
*           suppress_wa_popup            = SPACE    " Flag: Check Whether Maintenance is Allowed
*       TABLES
*           dba_sellist                  =     " Selection Conditions Only for Initial Object
*           dba_sellist_cluster          =     " Selection Conditions for Any Objects
*           excl_cua_funct_all_objects   =     " GUI Functions to be Deactivated for All Objects
*           excl_cua_funct_cluster       =     " GUI Functions to be Deactivated for Any Objects
*           dpl_sellist_for_start_object =     " Initial Object Display Selections
          EXCEPTIONS
            client_reference             = 1
            foreign_lock                 = 2
            viewcluster_not_found        = 3
            viewcluster_is_inconsistent  = 4
            missing_generated_function   = 5
            no_upd_auth                  = 6
            no_show_auth                 = 7
            object_not_found             = 8
            no_tvdir_entry               = 9
            no_clientindep_auth          = 10
            invalid_action               = 11
            saving_correction_failed     = 12
            system_failure               = 13
            unknown_field_in_dba_sellist = 14
            missing_corr_number          = 15
            OTHERS                       = 16.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN 'TR'.

        IF mo_alv->is_ready_for_input( ) <> '1'.
          MESSAGE 'enter change mode to create transport request'(040) TYPE 'E'.
          RETURN.
        ENDIF.

        create_transport_request( ).

      WHEN 'START_DS'.

        GET TIME STAMP FIELD mv_ts_start. "scrambling start timestamp

        IF mv_system_client_role = 'P'.                      "system check

          bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'E'
                                                     msgno = '000'
                                                     probclass = 2 ) ).

          bal_log_save( ).

          MESSAGE e006(zdst) WITH text-018.  "scrambling PRD?

        ENDIF.

        AUTHORITY-CHECK OBJECT 'Z_S_DST' ID 'ACTVT' FIELD '16'.

        IF sy-subrc NE 0.

          bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'E'
                                                     msgno = '006'
                                                     msgv1 = 'no authorization to start scrambling'(017)
                                                     msgv2 = 'for user '
                                                     msgv3 = sy-uname
                                                     probclass = 2 ) ).

          bal_log_save( ).
          MESSAGE 'no authorization to start scrambling'(017) TYPE 'E'.
          RETURN.

        ENDIF.

        IF format_alv_table( ) <> mt_fields_db OR mt_keys_int <> mt_keys_db.
          MESSAGE 'Template data was changed, please save before start scrambling'(019) TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        DATA(lv_icon_modify) = CONV iconname( icon_modify ).
        DATA(lv_icon_cancel) = CONV iconname( icon_cancel ).
        DATA(lv_icon_dangerous_goods) = CONV iconname( icon_dangerous_goods ).
        DATA(lv_icon_check) = CONV iconname( icon_check ).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'DST (Data Scrambling Tool)'    " Title of dialog box
*           diagnose_object       = SPACE    " Diagnosis text (maintain via SE61)
            text_question         = | CONFIRM | && mt_list[ key = fgbg_listbox ]-text && | DATA SCRAMBLING |  " Question text in dialog box
            text_button_1         = 'start'(030)    " Text on the first pushbutton
            icon_button_1         = lv_icon_modify   " Icon on first pushbutton
            text_button_2         = 'cancel'(031)        " Text on the second pushbutton
            icon_button_2         = lv_icon_cancel         " Icon on second pushbutton
*           default_button        = '1'    " Cursor position
            display_cancel_button = ''    " Button for displaying cancel pushbutton
*           userdefined_f1_help   = SPACE    " User-Defined F1 Help
            start_column          = '1'    " Column in which the POPUP begins
            start_row             = '10'    " Line in which the POPUP begins
            popup_type            = lv_icon_dangerous_goods   " Icon type
*           iv_quickinfo_button_1 = SPACE    " Quick Info on First Pushbutton
*           iv_quickinfo_button_2 = SPACE    " Quick Info on Second Pushbutton
          IMPORTING
            answer                = lv_answer   " Return values: '1', '2', 'A'
*          TABLES
*           parameter             =     " Text transfer table for parameter in text
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF lv_answer = '2'.
          RETURN.
        ENDIF.

        IF check_template_on_keys( ) IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'DST (Data Scrambling Tool)'    " Title of dialog box
*             diagnose_object       = SPACE    " Diagnosis text (maintain via SE61)
              text_question         = | TEMPLATE CONTAINS DB TABLE KEY FIELDS FOR SCRAMBLING |  " Question text in dialog box
              text_button_1         = 'start'(030)    " Text on the first pushbutton
              icon_button_1         = lv_icon_modify   " Icon on first pushbutton
              text_button_2         = 'cancel'(031)        " Text on the second pushbutton
              icon_button_2         = lv_icon_cancel         " Icon on second pushbutton
*             default_button        = '1'    " Cursor position
              display_cancel_button = ''    " Button for displaying cancel pushbutton
*             userdefined_f1_help   = SPACE    " User-Defined F1 Help
              start_column          = '15'    " Column in which the POPUP begins
              start_row             = '11'    " Line in which the POPUP begins
              popup_type            = lv_icon_check  " Icon type
*             iv_quickinfo_button_1 = SPACE    " Quick Info on First Pushbutton
*             iv_quickinfo_button_2 = SPACE    " Quick Info on Second Pushbutton
            IMPORTING
              answer                = lv_answer   " Return values: '1', '2', 'A'
*          TABLES
*             parameter             =     " Text transfer table for parameter in text
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF lv_answer = '2'.
            RETURN.
          ENDIF.

        ENDIF.


        IF fgbg_listbox = 1.
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          start_scrambling( ).
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        ELSE.

          cl_batch_event=>raise(
            EXPORTING
              i_eventid                      =  mc_batchjob_event   " Background Processing Events
*              i_eventparm                    =     " Background Event Parameters (Such as, Jobname/Jobcount)
*              i_server                       =     " Application Server Name
*              i_ignore_incorrect_server      = 'X'    " Boolean Variable (X=true, -=false, space=unknown)
             EXCEPTIONS
              excpt_raise_failed             = 1
              excpt_server_accepts_no_events = 2
              excpt_raise_forbidden          = 3
              excpt_unknown_event            = 4
              excpt_no_authority             = 5
              OTHERS                         = 6
          ).
          IF sy-subrc <> 0.
            MESSAGE 'Background job error, please check Background Processing Event'(041) TYPE 'E'.
            RETURN.
          ENDIF.

          bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'I'
                                                     msgno = '005'
                                                     msgv1 = mt_list[ key = fgbg_listbox ]-text
                                                     msgv2 = 'Check application and job logs.'(002)
                                                     probclass = 2 ) )."Scrambling started

          bal_log_save(  ).
          bal_log_display(  ).

        ENDIF.

      WHEN 'START_APPL_LOG'.
        bal_log_display_all(  ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD save_template_data.

    DATA:
         lt_insert_fields TYPE TABLE OF zcft_dst_fields
        ,lt_update_fields TYPE TABLE OF zcft_dst_fields
        ,lt_delete_fields TYPE TABLE OF zcft_dst_fields
        ,lt_insert_keys TYPE TABLE OF zcft_dst_keys
        ,lt_update_keys TYPE TABLE OF zcft_dst_keys
        ,lt_delete_keys TYPE TABLE OF zcft_dst_keys
        ,lt_delete_keys_add TYPE TABLE OF zcft_dst_keys
        .

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "   save fields
    SORT mt_fields_db.

    IF mv_not_valid IS NOT INITIAL.
      MESSAGE 'check the data'(015) TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    compare_tables(
                    EXPORTING
                      it_itab_new = format_alv_table( )
                      it_itab_old = mt_fields_db
                    IMPORTING
                      et_insert  = lt_insert_fields
                      et_update  = lt_update_fields
                      et_delete  = lt_delete_fields ).

    IF lt_insert_fields IS NOT INITIAL.
      INSERT zcft_dst_fields FROM TABLE lt_insert_fields ACCEPTING DUPLICATE KEYS.
    ENDIF.

    IF lt_update_fields IS NOT INITIAL.
      UPDATE zcft_dst_fields FROM TABLE lt_update_fields.
    ENDIF.

    IF lt_delete_fields IS NOT INITIAL.
      DELETE zcft_dst_fields FROM TABLE lt_delete_fields.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "save keys
    SORT mt_keys_int.
    SORT mt_keys_db.

    compare_tables( EXPORTING
                      it_itab_new = mt_keys_int
                      it_itab_old = mt_keys_db
                    IMPORTING
                      et_insert  = lt_insert_keys
                      et_update  = lt_update_keys
                      et_delete  = lt_delete_keys ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "delete keys if group of all fields deleted
    LOOP AT lt_delete_fields INTO DATA(ls_delete_fields)
        GROUP BY ( tabname = ls_delete_fields-tabname
                  idx = GROUP INDEX )
                  ASCENDING
                  ASSIGNING FIELD-SYMBOL(<group>)
                  .

      DATA(lt_members) = VALUE gty_dst_fields( FOR m IN GROUP <group> ( tabname = m-tabname ) ).

      DATA(lt_fields_db) = VALUE gty_dst_fields( FOR m IN mt_fields_db
                                         WHERE ( tabname = lt_members[ 1 ]-tabname )
                                               ( m )  ).

      IF lines( lt_members )  "number of lines for tabname is been deleted
       = lines( lt_fields_db ). "number of lines for tabname in DB

        lt_delete_keys_add = VALUE #( FOR <wa1> IN mt_keys_int
          WHERE ( tabname = lt_members[ 1 ]-tabname ) ( <wa1> ) ).

        APPEND LINES OF lt_delete_keys_add TO lt_delete_keys.

      ENDIF.

    ENDLOOP.

    IF lt_insert_keys IS NOT INITIAL.
      INSERT zcft_dst_keys FROM TABLE lt_insert_keys ACCEPTING DUPLICATE KEYS.
    ENDIF.

    IF lt_update_keys IS NOT INITIAL.
      UPDATE zcft_dst_keys FROM TABLE lt_update_keys ##WARN_OK.
    ENDIF.

    IF lt_delete_keys IS NOT INITIAL.
      DELETE zcft_dst_keys FROM TABLE lt_delete_keys.
    ENDIF.

    MESSAGE i001(zdst).           "Configuration updated

    bal_log_msg_add( is_msg = VALUE bal_s_msg(
                                                msgty = 'I'
                                                msgno = '001'
                                                                        ) )."Template data updated.

    bal_log_save( ).
    fetch_template_data( ).
    mo_alv->refresh_table_display( EXPORTING i_soft_refresh = abap_true  ).

  ENDMETHOD.

  METHOD leave_screen.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDMETHOD.

  METHOD get_system_parameters.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
*       systemedit         =     " System Change Option
*       systemname         =     " System Name (e.g. C11)
*       systemtype         =     " System Type ('SAP' or 'CUSTOMER')
*       system_client_edit =     " Change Option for Client-Dependent Customizing Objects
*       sys_cliinddep_edit =     " Change Option for Repository Objects in Logon Client
        system_client_role = mv_system_client_role    " Client Role (see Documentation)
*       ev_sfw_bcset_rec   =     " Recording Client for Switch BC Sets
*       ev_c_system        =     " Obsolete: Always returned empty
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.                                                       "for testing only
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "mv_system_client_role

    "Client role specifies the role of the client. The following values are possible:
    " - Productive client    ("P")
    " - Test client          ("T")
    " - Customizing client   ("C")

    """"""""""""""""""""""""""""""
    " others..
    " - Demo client          ("D")
    " - Training client      ("E")
    " - SAP reference client ("R")


  ENDMETHOD.

  METHOD start_scrambling.

    DATA:
          lv_field_count TYPE i
         ,lv_time TYPE syuzeit
         .

    CALL FUNCTION 'ENQUEUE_ESINDX'                                    "locking report to prevent running in other sessions
      EXPORTING
*       mode_indx      = 'E'    " Lock mode for table INDX
*       mandt          = SY-MANDT    " 01th enqueue argument
        relid          = 'ZS'   " 02th enqueue argument
        srtfd          = mc_repid   " 03th enqueue argument
        srtf2          = '0'   " 04th enqueue argument
*       x_relid        = SPACE    " Fill argument 02 with initial value?
*       x_srtfd        = SPACE    " Fill argument 03 with initial value?
*       x_srtf2        = SPACE    " Fill argument 04 with initial value?
*       _scope         = '2'
*       _wait          = SPACE
*       _collect       = ' '    " Initially only collect lock
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'E'
                                                 msgno = '008'
                                                 msgv1 = sy-msgv1
                                                 probclass = 2 ) ).

      DATA(lv_user) = sy-msgv1.

      bal_log_save(  ).
      MESSAGE e008(zdst) WITH lv_user DISPLAY LIKE 'A'.  "Scrambling is currently executed by &

    ENDIF.

    GET TIME STAMP FIELD mv_ts_start.    "scrambling start timestamp

    bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'I'
                                               msgno = '005'
                                               msgv1 = mt_list[ key = fgbg_listbox ]-text
                                               probclass = 2 ) )."Scrambling started

    IF sy-batch = abap_true.               "check to cover accident run with background job

      IF mv_system_client_role = gc_role_prod.                      "system check

        bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'E'
                                                   msgno = '000'
                                                   probclass = 2 ) ).

        bal_log_save( ).
        MESSAGE e000(zdst).  "scrambling PRD?

      ENDIF.

      AUTHORITY-CHECK OBJECT 'Z_S_DST' ID 'ACTVT' FIELD '16'. "authorization check

      IF sy-subrc NE 0.

        bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'E'
                                                   msgno = '006'
                                                   msgv1 = 'no authorization to start scrambling'(017)
                                                   msgv2 = 'for user '
                                                   msgv3 = sy-uname
                                                   probclass = 2 ) ).

        bal_log_save( ).
        MESSAGE 'no authorization to start scrambling'(017) TYPE 'E'.
        RETURN.

      ENDIF.

    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "percentage init

    CLEAR: mv_percentage, mv_percentage_step.

    DATA(lv_lines) = lines( mt_fields_db ).                          "total fields number

    lv_lines = lv_lines + lines( get_tables_list( ) ).               "steps for scrambling and table update operations

    CHECK lv_lines IS NOT INITIAL.
    mv_percentage_step = ( 100 / lv_lines ).                         "percentega step for each line

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " scrambling starts

    LOOP AT mt_fields_db INTO DATA(ls_field_int)
      GROUP BY ( tabname = ls_field_int-tabname
                 size = GROUP SIZE
                 index = GROUP INDEX )
                 ASCENDING
                 REFERENCE INTO DATA(group_ref).

      LOOP AT GROUP group_ref ASSIGNING FIELD-SYMBOL(<s_field>).

        IF <gfs_tab> IS NOT ASSIGNED.
          read_table_data( iv_tabname =  <s_field>-tabname ).
        ENDIF.

        add_percentage_step( ).

        scramble_table_column( EXPORTING
                                           iv_tabname = <s_field>-tabname
                                           iv_fieldname = <s_field>-fieldname
                               CHANGING
                                           cv_field_count = lv_field_count
                                            ).

      ENDLOOP.

      update_db_table( iv_tabname = <s_field>-tabname ).

      bal_log_msg_add( is_msg = VALUE bal_s_msg(
                                                   msgty = SWITCH char1( sy-subrc
                                                                           WHEN 0 THEN SWITCH char1( lv_field_count
                                                                                                       WHEN '0' THEN 'W'
                                                                                                       ELSE 'I' )
                                                                           WHEN 99 THEN 'W'
                                                                           ELSE 'W'  )
                                                       msgno = '002'
                                                       msgv1 = <s_field>-tabname
                                                       msgv2 = lv_field_count
                                                       msgv3 = sy-subrc
                                                                           ) )."& table modification finished. SY-SUBRC = &

      UNASSIGN <gfs_tab>.
      CLEAR lv_field_count.

    ENDLOOP.

    " scrambling ends
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    GET TIME STAMP FIELD DATA(ts_finish). "finish time

    cl_abap_tstmp=>subtract(        "scrambling duration
      EXPORTING
        tstmp1                     =  ts_finish   " UTC Time Stamp
        tstmp2                     =  mv_ts_start   " UTC Time Stamp
      RECEIVING
        r_secs                     = DATA(lv_duration_secs)    " Time Interval in Seconds
    ).

    " This variable works better if declared explicitly. 
    DATA lv_duration_sec_int4 TYPE cx_point_in_time.

    lv_duration_sec_int4 = CONV int4( lv_duration_secs ).

    CALL FUNCTION 'POINT_IN_TIME_CONVERT'
      EXPORTING
*       kz_endtermin  = SPACE    " Indicator finish date
        point_in_time = lv_duration_sec_int4   " Time (seconds since reference date
      IMPORTING
*       date          =     " Date
        time          = lv_time.    " Time

    bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'I'
                                               msgno = '003'
                                               msgv1 = |{ ts_finish TIMESTAMP = SPACE }|
                                               msgv2 = |{ lv_time TIME = ISO }|
                                               probclass = 2 ) )."Scrambling completed

    bal_log_save(  ).
    bal_log_display(  ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 100
        text       = text-020. "DATA SCRAMBLING COMPLETE

    CALL FUNCTION 'DEQUEUE_ESINDX'
      EXPORTING
*       mode_indx = 'E'    " Lock mode for table INDX
*       mandt = SY-MANDT    " 01th enqueue argument
        relid = 'ZD'   " 02th enqueue argument
        srtfd = mc_repid   " 03th enqueue argument
        srtf2 = '0'   " 04th enqueue argument
*       x_relid   = SPACE    " Fill argument 02 with initial value?
*       x_srtfd   = SPACE    " Fill argument 03 with initial value?
*       x_srtf2   = SPACE    " Fill argument 04 with initial value?
*       _scope    = '3'
*       _synchron = SPACE    " Synchonous unlock
*       _collect  = ' '    " Initially only collect lock
      .

  ENDMETHOD.

  METHOD read_table_data.
    " Read data of the table to be scrambled
    DATA:
           lr_data        TYPE REF TO data
          ,lr_data_db        TYPE REF TO data
          .

    CREATE DATA lr_data TYPE TABLE OF (iv_tabname).
    ASSIGN lr_data->* TO <gfs_tab>.

    CREATE DATA lr_data_db TYPE TABLE OF (iv_tabname).
    ASSIGN lr_data_db->* TO <gfs_tab_db>.

    DATA(lt_where) = create_where( iv_tabname = iv_tabname ).

    SELECT *                                                    "dynamic select
    FROM (iv_tabname)
    BYPASSING BUFFER
    INTO CORRESPONDING FIELDS OF TABLE @<gfs_tab_db>
    WHERE (lt_where).

    <gfs_tab> = CORRESPONDING #( <gfs_tab_db> ).                       "original values

    DATA(lv_lines) = lines( <gfs_tab> ).

    bal_log_msg_add( is_msg = VALUE bal_s_msg(
                                              msgty = SWITCH char1( sy-subrc
                                                                                WHEN 0 THEN 'I'
                                                                                ELSE 'W' )
                                              msgno = '004'
                                              msgv1 = iv_tabname
                                              msgv2 = lv_lines
                                              msgv3 = sy-subrc
                                                                ) )."table content fetch finished

  ENDMETHOD.

  METHOD scramble_table_column.

    FIELD-SYMBOLS:
                         <lfs> TYPE any
                        .

    DATA:
               lv_string TYPE string
              ,lv_random_c TYPE c LENGTH 1000
              .

    CHECK iv_fieldname <> 'MANDT'. "rel2

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = mv_percentage
        text       = |{ 'scrambling'(009)  && ` ` && iv_tabname  && '-' && iv_fieldname }|.


    LOOP AT <gfs_tab> ASSIGNING FIELD-SYMBOL(<fs_line>).

      CLEAR lv_random_c.

      UNASSIGN <lfs>.
      ASSIGN COMPONENT iv_fieldname OF STRUCTURE <fs_line> TO <lfs>.
      CHECK sy-subrc IS INITIAL.

      DESCRIBE FIELD <lfs> TYPE DATA(lv_type).                    "data type.

      lv_string = <lfs>.

      IF lv_string NE ''.
        DATA(lv_field_len) = strlen( lv_string ).     "field lenght for non-numeric fields
      ELSE.
        CLEAR lv_field_len.
      ENDIF.

      "to avoid updating of numeric table fields with characters and vice versa
      "Numeric Data Type   ID
      "b                   b
      "s                   s
      "i                   I
      "int8                8
      "p                   P
      "decfloat16          a
      "decfloat34          e
      "f                   F

      IF
         lv_type =  'b'
      OR lv_type =  'D'       "rel2
      OR lv_type =  's'
      OR lv_type =  'I'
      OR lv_type =  '8'
      OR lv_type =  'P'
      OR lv_type =  'a'
      OR lv_type =  'e'
      OR lv_type =  'F'
      OR lv_type =  'N'.

*        CALL FUNCTION 'GENERAL_GET_RANDOM_INT'                              "rel2
*          EXPORTING
*            range  = CONV i( <lfs> )   " The random int will be <= range
*          IMPORTING
*            random = lv_random.   " Random value

        IF lv_field_len <> 0.

          DESCRIBE FIELD <lfs> LENGTH DATA(lv_len) IN BYTE MODE. "data lenght

          IF lv_len <> 1.
            lv_len = lv_len - 1. "dot
          ENDIF.

          DATA(lv_alphabet) = CONV char200( '0123456789' ).

          CALL 'RSEC_GEN_PASSWD'
            ID 'ALPHABET'    FIELD lv_alphabet
            ID 'ALPHABETLEN' FIELD 10
            ID 'OUTPUT'      FIELD lv_random_c
            ID 'OUTPUTLEN'   FIELD lv_len
            ID 'FORCE_INIT'  FIELD ''.                    "#EC CI_CCALL

          <lfs> = lv_random_c.

        ENDIF.

      ELSEIF lv_type =  'C'.

        IF lv_field_len <> 0.

*          CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'                      "rel2
*            EXPORTING
*              number_chars  = lv_field_len   " Specifies the number of generated chars
*            IMPORTING
*              random_string = lv_string.   " Generated string

          lv_alphabet = CONV char200( 'ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456789@$%&/\()=+-#~ []{}' ).

          CALL 'RSEC_GEN_PASSWD'
            ID 'ALPHABET'    FIELD lv_alphabet
            ID 'ALPHABETLEN' FIELD 77
            ID 'OUTPUT'      FIELD lv_random_c
            ID 'OUTPUTLEN'   FIELD lv_field_len
            ID 'FORCE_INIT'  FIELD ''.                    "#EC CI_CCALL

*          <lfs> = |{ lv_random_c CASE = (cl_abap_format=>c_upper) }|.
          <lfs> = lv_random_c.

        ENDIF.

      ELSE.

        IF lv_field_len <> 0.

          CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
            EXPORTING
              number_chars  = lv_field_len   " Specifies the number of generated chars
            IMPORTING
              random_string = lv_string.   " Generated string

*          <lfs> = |{ lv_string CASE = (cl_abap_format=>c_upper) }|.
          <lfs> = lv_string .

        ENDIF.

      ENDIF.

      cv_field_count = cv_field_count + 1.

    ENDLOOP.

  ENDMETHOD.

  METHOD compare_tables.

    FIELD-SYMBOLS:
      <ld_chind>  TYPE bu_chind,
      <ls_struc>  TYPE any,
      <ls_di>     TYPE any,
      <lt_old_di> TYPE table,
      <lt_new_di> TYPE table.

    TYPES: BEGIN OF ty_s_di.
    TYPES:   chind TYPE bu_chind.  "change indicator
    TYPES: END OF ty_s_di.

    DATA:
           lv_struc_old     TYPE string
          ,lv_struc_new     TYPE string
          ,lv_tabname       TYPE tabname
          ,lo_struc         TYPE REF TO data
          ,lo_di_struc      TYPE REF TO data
          ,lo_di_new        TYPE REF TO data
          ,lo_di_old        TYPE REF TO data
          ,ls_chind         TYPE ty_s_di
          ,lt_components    TYPE cl_abap_structdescr=>component_table
          ,lt_components_di TYPE cl_abap_structdescr=>component_table
          ,lo_tab_new       TYPE REF TO cl_abap_tabledescr
          ,lo_tab_old       TYPE REF TO cl_abap_tabledescr
          ,lo_tab_di        TYPE REF TO cl_abap_tabledescr
          ,lo_strucdescr    TYPE REF TO cl_abap_structdescr
          .

    "Get RTTI of new itab
    lo_tab_new ?= cl_abap_typedescr=>describe_by_data( it_itab_new ).
    lo_strucdescr ?= lo_tab_new->get_table_line_type( ).
    lv_struc_new = lo_strucdescr->get_relative_name( ).
    lv_tabname = lv_struc_new.  " type conversion for function module

    "Get RTTI of old itab
    lo_tab_old ?= cl_abap_typedescr=>describe_by_data( it_itab_old ).
    lo_strucdescr ?= lo_tab_old->get_table_line_type( ).
    lv_struc_old = lo_strucdescr->get_relative_name( ).

    IF ( lv_struc_old NE lv_struc_new ).
*          RAISE error.  " itab's have different line types. NO ACTION
    ENDIF.

    "Create variable having line type of new/old itab
    CREATE DATA lo_struc TYPE HANDLE lo_strucdescr.
    ASSIGN lo_struc->* TO <ls_struc>.  " line type of new/old itab

    "Get components of new/old itab and add component CHIND
    lt_components_di = lo_strucdescr->get_components( ).

    REFRESH: lt_components.
    CLEAR: ls_chind.

    lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( ls_chind ).
    lt_components  = lo_strucdescr->get_components( ).

    APPEND LINES OF lt_components TO lt_components_di.

    "Create variable having line type of new/old itab with additional change indicator field & corresponding itab
    lo_strucdescr = cl_abap_structdescr=>create( lt_components_di ).
    lo_tab_di     = cl_abap_tabledescr=>create( lo_strucdescr ).

    CREATE DATA lo_di_struc  TYPE HANDLE lo_strucdescr.
    CREATE DATA lo_di_new    TYPE HANDLE lo_tab_di.
    CREATE DATA lo_di_old    TYPE HANDLE lo_tab_di.

    ASSIGN lo_di_struc->*  TO <ls_di>.
    ASSIGN lo_di_new->*    TO <lt_new_di>.
    ASSIGN lo_di_old->*    TO <lt_old_di>.

    "Shuffle data from new itab into corresponding itab with change indicator (field CHIND)
    LOOP AT it_itab_new INTO <ls_struc>.
      MOVE-CORRESPONDING <ls_struc> TO <ls_di>.
      APPEND <ls_di> TO <lt_new_di>.
    ENDLOOP.

    LOOP AT it_itab_old INTO <ls_struc>. "Shuffle data from old itab into corresponding itab with change indicator (field CHIND)
      MOVE-CORRESPONDING <ls_struc> TO <ls_di>.
      APPEND <ls_di> TO <lt_old_di>.
    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " NOTE: If check_indicator = ' ' then the itab's are condensed meaning
    "       that identical entries are removed from both itab's.
    " Remaining entries in table_new have the following change indicators:
    " - 'I' = INSERT, i.e. a new entry
    " - 'U' = UPDATE, i.e. a modified entry
    "
    " Remaining entries in table_old have the following change indicators:
    " - 'D' = DELETE, i.e. a deleted entry
    " - ' ' = has a corresponding entry in table_new with CHIND = 'U'

    CALL FUNCTION 'CHANGEDOCUMENT_PREPARE_TABLES'
      EXPORTING
        check_indicator        = abap_false "SPACE    " Flag whether tables are to be changed
        tablename              = lv_tabname    " Name of the table structure of the internal tables
*       check_only_logflags    = SPACE    " Control Flag: Check Change Document-Relevant Fields Only
*      IMPORTING
*       result                 =     " Flag whether tables are identical
      TABLES
        table_new              = <lt_new_di>    " Table contains the changed data
        table_old              = <lt_old_di>    " Table contains the unchanged data
      EXCEPTIONS
        nametab_error          = 1
        wrong_structure_length = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          RAISING function_call_error.
    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Fill the output itab's depending on the change indicator
    " of the records
    LOOP AT <lt_new_di> INTO <ls_di>.  " new itab -> INSERT & UPDATE

      MOVE-CORRESPONDING <ls_di> TO <ls_struc>.
      ASSIGN COMPONENT 'CHIND' OF STRUCTURE <ls_di> TO <ld_chind>.

      CASE <ld_chind>.

        WHEN 'I'.   "insert
          APPEND <ls_struc> TO et_insert.

        WHEN 'U'.    "u
          APPEND <ls_struc> TO et_update.

        WHEN OTHERS.
          CONTINUE.

      ENDCASE.

    ENDLOOP.

    LOOP AT <lt_old_di> INTO <ls_di>.  " old itab -> DELETE

      MOVE-CORRESPONDING <ls_di> TO <ls_struc>.
      ASSIGN COMPONENT 'CHIND' OF STRUCTURE <ls_di> TO <ld_chind>.

      CASE <ld_chind>.

        WHEN 'D'.       "DELETE
          APPEND <ls_struc> TO et_delete.

          "Modified entry (old values)
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                    "compare_tables

  METHOD create_transport_request.

    DATA:
           lt_e071 TYPE STANDARD TABLE OF e071
          ,lt_e071k TYPE STANDARD TABLE OF e071k
          ,lv_msg_saved TYPE itex132
          ,lv_task TYPE trkorr
          .

    IF mv_system_client_role <> gc_role_reci.
      MESSAGE 'use customizing client for transport request creation'(014) TYPE 'E'.    "
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
*    EXPORTING
*      iv_category            = 'CUST'    " Required request category ('CUST', 'SYST')
*      iv_cli_dep             = 'X'    " Flag: 'X' = request client check also for SYST
*      iv_project_check       = ' '    " Check request for project assignment
      IMPORTING
*       ev_order               =     " Selected request
        ev_task                = lv_task   " Selected Task
      EXCEPTIONS
        invalid_category       = 1
        no_correction_selected = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lt_e071 = VALUE #( ( trkorr = lv_task
                          as4pos = '000001'
                          pgmid = gc_pgmid_r3tr
                          object = gc_object_cdat
                          obj_name = mc_zcf_dst_config_view
                          objfunc = trco )

                        ( trkorr = lv_task
                          as4pos = '000002'
                          pgmid = gc_pgmid_r3tr
                          object = gc_object_cdat
                          obj_name = mc_zcf_dst_config_view
                          objfunc = trco ) ) .

    lt_e071k = VALUE #( ( trkorr = lv_task
                          pgmid = gc_pgmid_r3tr
                          object = gc_object_tabu
                          objname = mc_zcft_dst_fields
                          as4pos = '000001'
                          mastertype = gc_object_cdat
                          mastername = mc_zcf_dst_config_view
                          tabkey = gc_logo_generic
                          sortflag = '2' )

                        ( trkorr = lv_task
                          pgmid = gc_pgmid_r3tr
                          object = gc_object_tabu
                          objname = mc_zcft_dst_keys
                          as4pos = '000002'
                          mastertype = gc_object_cdat
                          mastername = mc_zcf_dst_config_view
                          tabkey = gc_logo_generic
                          sortflag = '2' ) ).

    CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
      EXPORTING
*       wi_simulation                  = ' '    " Flag, abap_true - no database update
*       wi_suppress_key_check          = ' '    " Flag whether key syntax check is suppressed
        wi_trkorr                      = lv_task    " Task to be added to
*       it_e071k_str                   =     " Table Key with String Field
      TABLES
        wt_e071                        = lt_e071    " Table of objects to be added
        wt_e071k                       = lt_e071k   " Table of keys to be added
      EXCEPTIONS
        key_char_in_non_char_field     = 1
        key_check_keysyntax_error      = 2
        key_inttab_table               = 3
        key_longer_field_but_no_generc = 4
        key_missing_key_master_fields  = 5
        key_missing_key_tablekey       = 6
        key_non_char_but_no_generic    = 7
        key_no_key_fields              = 8
        key_string_longer_char_key     = 9
        key_table_has_no_fields        = 10
        key_table_not_activ            = 11
        key_unallowed_key_function     = 12
        key_unallowed_key_object       = 13
        key_unallowed_key_objname      = 14
        key_unallowed_key_pgmid        = 15
        key_without_header             = 16
        ob_check_obj_error             = 17
        ob_devclass_no_exist           = 18
        ob_empty_key                   = 19
        ob_generic_objectname          = 20
        ob_ill_delivery_transport      = 21
        ob_ill_lock                    = 22
        ob_ill_parts_transport         = 23
        ob_ill_source_system           = 24
        ob_ill_system_object           = 25
        ob_ill_target                  = 26
        ob_inttab_table                = 27
        ob_local_object                = 28
        ob_locked_by_other             = 29
        ob_modif_only_in_modif_order   = 30
        ob_name_too_long               = 31
        ob_no_append_of_corr_entry     = 32
        ob_no_append_of_c_member       = 33
        ob_no_consolidation_transport  = 34
        ob_no_original                 = 35
        ob_no_shared_repairs           = 36
        ob_no_systemname               = 37
        ob_no_systemtype               = 38
        ob_no_tadir                    = 39
        ob_no_tadir_not_lockable       = 40
        ob_privat_object               = 41
        ob_repair_only_in_repair_order = 42
        ob_reserved_name               = 43
        ob_syntax_error                = 44
        ob_table_has_no_fields         = 45
        ob_table_not_activ             = 46
        tr_enqueue_failed              = 47
        tr_errors_in_error_table       = 48
        tr_ill_korrnum                 = 49
        tr_lockmod_failed              = 50
        tr_lock_enqueue_failed         = 51
        tr_not_owner                   = 52
        tr_no_systemname               = 53
        tr_no_systemtype               = 54
        tr_order_not_exist             = 55
        tr_order_released              = 56
        tr_order_update_error          = 57
        tr_wrong_order_type            = 58
        ob_invalid_target_system       = 59
        tr_no_authorization            = 60
        ob_wrong_tabletyp              = 61
        ob_wrong_category              = 62
        ob_system_error                = 63
        ob_unlocal_objekt_in_local_ord = 64
        tr_wrong_client                = 65
        ob_wrong_client                = 66
        key_wrong_client               = 67
        OTHERS                         = 68.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_msg_saved = 'template saved to task &1'(012).
    REPLACE ALL OCCURRENCES OF '&1' IN lv_msg_saved WITH lv_task.
    MESSAGE lv_msg_saved TYPE 'I'.    "Data not changed

    bal_log_msg_add( is_msg = VALUE bal_s_msg( msgty = 'I'
                                               msgno = '006'
                                               msgv1 = lv_msg_saved  ) ).

    bal_log_save( ).

  ENDMETHOD.

  METHOD read_dd_descriptions.

    FIELD-SYMBOLS <fs> TYPE gty_alv.

    LOOP AT mt_fields_int_alv ASSIGNING <fs>.

      SELECT SINGLE ddtext                    "table description
               FROM dd02t
               INTO <fs>-tab_ddtext
              WHERE
                  tabname = <fs>-tabname
              AND ddlanguage = sy-langu.

      SELECT SINGLE ddtext                    "field description
               FROM dd03vt
               INTO <fs>-field_ddtext
              WHERE
                   tabname = <fs>-tabname
               AND fieldname = <fs>-fieldname
               AND ddlanguage = sy-langu.

    ENDLOOP.

    update_key_column(  ).
    SORT mt_fields_int_alv.

  ENDMETHOD.

  METHOD update_key_column.

    FIELD-SYMBOLS <fs> TYPE gty_alv.
    DATA: ls_cell_color TYPE lvc_s_scol.

    LOOP AT mt_fields_int_alv ASSIGNING <fs>.

      REFRESH <fs>-cell_color.

      IF line_exists( mt_keys_int[ tabname = <fs>-tabname ] ).

        <fs>-key_exist = icon_foreign_key.

        ls_cell_color-fname = mc_key_exist.
        ls_cell_color-color-col = cl_gui_resources=>list_col_positive.
        ls_cell_color-color-int = 0.
        ls_cell_color-color-inv = 0.

        APPEND ls_cell_color TO <fs>-cell_color.
        CLEAR ls_cell_color.

      ELSE.

        <fs>-key_exist = icon_add_row.
        ls_cell_color-fname = mc_key_exist.
        ls_cell_color-color-col = cl_gui_resources=>list_col_normal.
        ls_cell_color-color-int = 0.
        ls_cell_color-color-inv = 0.

        APPEND ls_cell_color TO <fs>-cell_color.
        CLEAR ls_cell_color.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed_finished.

    IF e_modified IS NOT INITIAL.
      read_dd_descriptions(  ).
      mo_alv->refresh_table_display( EXPORTING i_soft_refresh = abap_true ).
    ENDIF.

  ENDMETHOD.

  METHOD format_alv_table.

    TRY.
        mo_alv->check_changed_data(
*          IMPORTING
*            e_valid   =     " Entries are Consistent
*          CHANGING
*            c_refresh = 'X'    " Character Field of Length 1
            ).

      CATCH cx_sy_ref_is_initial.

    ENDTRY.

    rt_mt_fields_int = VALUE #( FOR <wa> IN mt_fields_int_alv
                                ( mandt = sy-mandt
                                tabname = <wa>-tabname
                                fieldname = <wa>-fieldname
                                key_exist = <wa>-key_exist ) ).

    SORT: rt_mt_fields_int, mt_fields_db.

  ENDMETHOD.

  METHOD rsds_trange_to_range.


*    rt_field_ranges = VALUE rsds_trange(
*                                            FOR <wa_keys_int> IN mt_keys_int WHERE ( tabname = iv_tabname )    "initial selections
*                                                ( tablename = <wa_keys_int>-tabname
*                                                frange_t = VALUE rsds_frange_t( FOR <wa_keys_int2> IN mt_keys_int WHERE ( tabname = iv_tabname AND field = <wa_keys_int>-field )
*                                                ( fieldname = <wa_keys_int2>-field
*                                                selopt_t = VALUE rsds_selopt_t( FOR <wa_keys_int_options> IN mt_keys_int WHERE ( tabname = iv_tabname AND field = <wa_keys_int>-field )
*                                                ( sign = <wa_keys_int_options>-sign
*                                                  option = <wa_keys_int_options>-zoption
*                                                  low = <wa_keys_int_options>-low
*                                                  high = <wa_keys_int_options>-high
*                                                                                               ) ) ) ) ) ).

    DATA(lt_frange) =  VALUE rsds_frange_t( FOR <wa_keys_int> IN mt_keys_int WHERE ( tabname = iv_tabname )
                                                ( fieldname = <wa_keys_int>-field
                                                selopt_t = VALUE rsds_selopt_t( FOR <wa_keys_int_options> IN mt_keys_int WHERE ( tabname = iv_tabname AND field = <wa_keys_int>-field )
                                                ( sign = <wa_keys_int_options>-sign
                                                  option = <wa_keys_int_options>-zoption
                                                  low = <wa_keys_int_options>-low
                                                  high = <wa_keys_int_options>-high
                                                                                                ) ) ) ).

    DELETE ADJACENT DUPLICATES FROM lt_frange.

    rt_field_ranges = VALUE rsds_trange( ( tablename = iv_tabname
                                           frange_t = lt_frange ) ).

  ENDMETHOD.

  METHOD create_where.

    DATA(lt_rs_t_rscedst) = VALUE rs_t_rscedst(
                                                FOR <wa_keys> IN mt_keys_int WHERE ( tabname = iv_tabname )
                                                    (
                                                      fnam = <wa_keys>-field
                                                      sign = <wa_keys>-sign
                                                      option = <wa_keys>-zoption
                                                      low = <wa_keys>-low
                                                      high = <wa_keys>-high
                                                                                ) ).

    CALL FUNCTION 'RSDS_RANGE_TO_WHERE'
      EXPORTING
        i_t_range      = lt_rs_t_rscedst    " Transfer structure
*       i_th_range     =     " DTP: Selections for Source
*       i_r_renderer   =     " Format Dimension and Values for Output
      IMPORTING
*       e_where        =     " where condition as a string
        e_t_where      = rt_where    " where condition as a table
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD bal_log_create.

    DATA:
          ls_s_log TYPE bal_s_log.

    CHECK mv_bal_log_created IS INITIAL.

    ls_s_log-object = mc_balobj_d_zdst.
    ls_s_log-alchuser = sy-uname.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_s_log    " Log header data
      IMPORTING
        e_log_handle            = mv_log_handle     " Log handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      mv_bal_log_created = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD bal_log_msg_add.

    bal_log_create(  ).

    DATA(ls_bal_s_msg) = CORRESPONDING bal_s_msg( is_msg ).

    IF is_msg-probclass IS INITIAL.
      ls_bal_s_msg-probclass = 4.           "Additional Information
    ENDIF.

    IF ls_bal_s_msg-msgid IS INITIAL.
      ls_bal_s_msg-msgid = mc_balobj_d_zdst.  "message class ZDST
    ENDIF.

    ls_bal_s_msg-msgv1 = condense( CONV string( is_msg-msgv1 ) ).
    ls_bal_s_msg-msgv2 = condense( CONV string( is_msg-msgv2 ) ).
    ls_bal_s_msg-msgv3 = condense( CONV string( is_msg-msgv3 ) ).
    ls_bal_s_msg-msgv4 = condense( CONV string( is_msg-msgv4 ) ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle    " Log handle
        i_s_msg          = ls_bal_s_msg    " Notification data
*      IMPORTING
*       e_s_msg_handle   =     " Message handle
*       e_msg_was_logged =     " Message collected
*       e_msg_was_displayed =     " Message output
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD bal_log_display.

    DATA:
          l_s_display_profile TYPE bal_s_prof.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
*     EXPORTING
*       start_col           =     " Application Log: Dialog box coordinates
*       start_row           =     " Application Log: Dialog box coordinates
*       end_col             =     " Application Log: Dialog box coordinates
*       end_row             =     " Application Log: Dialog box coordinates
      IMPORTING
        e_s_display_profile = l_s_display_profile ##FM_SUBRC_OK.

    l_s_display_profile-use_grid = abap_true.         "grid
    l_s_display_profile-disvariant-report = sy-repid. "set report to allow saving of variants

    l_s_display_profile-disvariant-handle = 'LOG'. "specify a handle to distinguish between the display

    DATA(lt_log_handle) = VALUE bal_t_logh( ( mv_log_handle ) ). "limit to display the last scrambling log

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = l_s_display_profile    " Display Profile
        i_t_log_handle       = lt_log_handle    " Restrict display by log handle
*       i_t_msg_handle       =     " Restrict display by message handle
*       i_s_log_filter       =     " Restrict display by log filter
*       i_s_msg_filter       =     " Restrict display by message filter
*       i_t_log_context_filter        =     " Restrict display by log context filter
*       i_t_msg_context_filter        =     " Restrict display by message context filter
*       i_amodal             = SPACE    " Display amodally in new session
*       i_srt_by_timstmp     = SPACE    " Sort Logs by Timestamp ('X') or Log Number (SPACE)
*       i_msg_context_filter_operator = 'A'    " Operator for message context filter ('A'nd or 'O'r)
*      IMPORTING
*       e_s_exit_command     =     " Application Log: Key confirmed by user at end
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD bal_log_save.

    DATA(lt_log_handle) = VALUE bal_t_logh( ( mv_log_handle ) ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = ''
        text       = 'saving Applicaton Log'(013).

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
*       i_client         = SY-MANDT    " Client in which the new log is to be saved
*       i_in_update_task = SPACE    " Save in UPDATE TASK
*       i_save_all       = SPACE    " Save all logs in memory
        i_t_log_handle   = lt_log_handle    " Table of log handles
*       i_2th_connection = SPACE    " FALSE: No secondary connection
*       i_2th_connect_commit = SPACE    " FALSE: No COMMIT in module
*       i_link2job       = 'X'    " Boolean Variable (X = True, - = False, Space = Unknown)
*      IMPORTING
*       e_new_lognumbers =     " Table of new log numbers
*       e_second_connection  =     " Name of Secondary Connection
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CLEAR mv_bal_log_created.
    ENDIF.

  ENDMETHOD.

  METHOD bal_log_display_all.

    DATA:
    lt_log_header     TYPE balhdr_t.

    DATA(ls_log_filter) = VALUE bal_s_lfil(
                                            object = VALUE bal_r_obj( (
                                            sign = 'I'
                                            option = 'EQ'
                                            low = mc_balobj_d_zdst ) ) ).

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
*       i_client           = SY-MANDT    " Client to be searched
        i_s_log_filter     = ls_log_filter    " Log header data filter
*       i_t_sel_field      =     " Errors to Be Read
*       i_tzone            =     " ABAP System Field: Time Zone
      IMPORTING
        e_t_log_header     = lt_log_header      " Table of log header data read
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = lt_log_header     " Alternative 1: Table of log headers
*       i_t_log_handle     =     " Alternative 2: Table of log handles
*       i_t_lognumber      =     " Alternative 3: Table of log numbers
*       i_client           = SY-MANDT    " Client for I_T_LOGNUMBER
*       i_do_not_load_messages        = SPACE    " Only load log header
*       i_exception_if_already_loaded =     " Raise exception if log already loaded
*       i_lock_handling    = 2    " 0: Ignore locks, 1: Do not read locked items; 3: Wait for un
*        IMPORTING
*       e_t_log_handle     =     " Table of handles of logs read
*       e_t_msg_handle     =     " Table of handles of messages read
*       e_t_locked         =     " Logs locked and not read
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*     EXPORTING
*       i_s_display_profile           =     " Display Profile
*       i_t_log_handle                =     " Restrict display by log handle
*       i_t_msg_handle                =     " Restrict display by message handle
*       i_s_log_filter                =     " Restrict display by log filter
*       i_s_msg_filter                =     " Restrict display by message filter
*       i_t_log_context_filter        =     " Restrict display by log context filter
*       i_t_msg_context_filter        =     " Restrict display by message context filter
*       i_amodal                      = SPACE    " Display amodally in new session
*       i_srt_by_timstmp              = SPACE    " Sort Logs by Timestamp ('X') or Log Number (SPACE)
*       i_msg_context_filter_operator = 'A'    " Operator for message context filter ('A'nd or 'O'r)
*     IMPORTING
*       e_s_exit_command              =     " Application Log: Key confirmed by user at end
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD add_percentage_step.

    IF mv_percentage < 100.                                 "progress indicator add step.

      TRY.
          mv_percentage = mv_percentage + mv_percentage_step.
        CATCH cx_sy_arithmetic_overflow.
          mv_percentage = 99.
      ENDTRY.

      IF mv_percentage >= 100.
        mv_percentage = 99.                                 "reserve 100% for last operation
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD update_db_table.

    add_percentage_step( ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = mv_percentage
        text       = |{ 'updating DB table '(011)  && ` ` && iv_tabname }| ##FM_SUBRC_OK.

    TRY.

        DELETE (iv_tabname) FROM TABLE <gfs_tab_db>.   "delete

        IF sy-subrc <> 0.
          CALL FUNCTION 'DB_ROLLBACK'.
          bal_log_msg_add( is_msg = VALUE bal_s_msg(
                                                          msgty = 'E'
                                                          msgno = '009'
                                                          msgv1 = iv_tabname
                                                          probclass = 1
                                                                 ) ).
          EXIT.
        ELSE.

          MODIFY (iv_tabname) FROM TABLE <gfs_tab> .

          IF sy-subrc <> 0.
            CALL FUNCTION 'DB_ROLLBACK'.
            bal_log_msg_add( is_msg = VALUE bal_s_msg(
                                                            msgty = 'E'
                                                            msgno = '009'
                                                            msgv1 = iv_tabname
                                                                 ) ).
            EXIT.
          ENDIF.
        ENDIF.

      CATCH cx_sy_open_sql_db INTO DATA(lx_ref). "SQL errors

        cl_message_helper=>set_msg_vars_for_clike( text = lx_ref->get_text( )  ).
        bal_log_msg_add( is_msg = VALUE bal_s_msg(
                                                        msgty = 'E'
                                                        msgno = '006'
                                                        msgv1 = sy-msgv1
                                                        msgv2 = sy-msgv2
                                                        msgv3 = sy-msgv3
                                                        msgv4 = sy-msgv4
                                                        probclass = 1
                                                                                ) )."& SQL error
        sy-subrc = '99'.

    ENDTRY.

  ENDMETHOD.

  METHOD check_template_on_keys.

    LOOP AT mt_fields_db INTO DATA(ls_fields_db).

      SELECT COUNT(*)
              FROM dd03l
             WHERE
                  tabname = @ls_fields_db-tabname
              AND fieldname = @ls_fields_db-fieldname
              AND keyflag = @abap_true.

      IF sy-dbcnt IS NOT INITIAL.
        rt_key_exist = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_tables_list.

    rt_tab_data  =  VALUE rsar_t_tabnames(  FOR <fs> IN mt_fields_db  ( <fs>-tabname )    ). " creating table consisting of table data
    DELETE ADJACENT DUPLICATES FROM rt_tab_data.

  ENDMETHOD.

ENDCLASS.
