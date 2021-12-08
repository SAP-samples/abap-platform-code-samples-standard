*&---------------------------------------------------------------------*
*& Report zaf_r_extend_rds_based_service
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaf_r_extend_rds_based_service.

"todo's
"check usage of CL_BSA_SADL_M2M_TRANSFORMER to transform the SADL XML to a structure and vice versa.

CLASS cl_sadl_based_segw_project DEFINITION .

  PUBLIC SECTION.



    METHODS: constructor IMPORTING io_segw_project_name TYPE  /iwbep/i_sbd_ga-project
                         RAISING   cx_parameter_invalid,
      get_sadl_xml RETURNING VALUE(sadl_xml) TYPE string
                   RAISING   cx_sadl_gw_no_service_api,
      get_merged_sadl_xml IMPORTING sadl_xml_source_project   TYPE string
                                    sadl_xml_extended_project TYPE string
                          RETURNING VALUE(sadl_xml_merged)    TYPE string,
      get_dpc_ext_class_name  RETURNING VALUE(class_name) TYPE string,
      get_mpc_ext_class_name  RETURNING VALUE(class_name) TYPE string,
      get_dpc_class_name  RETURNING VALUE(class_name) TYPE string,
      get_mpc_class_name  RETURNING VALUE(class_name) TYPE string,
      get_segw_project_name  RETURNING VALUE(project_name) TYPE string.
    "methods A012345678901234567890123456789.

    METHODS insert_sadl_xml_into_dpc
      IMPORTING
        !iv_sadl_xml                  TYPE string
        iv_src_segw_project_name      TYPE string
      CHANGING
        generated_code_definition     TYPE rswsourcet
        generated_code_implementation TYPE rswsourcet
      RAISING
        /iwbep/cx_sbcm_exception .

    METHODS insert_sadl_xml_into_mpc_ext
      IMPORTING
        !iv_sadl_xml                  TYPE string
        iv_src_segw_project_name      TYPE string
      CHANGING
        generated_code_definition     TYPE rswsourcet
        generated_code_implementation TYPE rswsourcet
      RAISING
        /iwbep/cx_sbcm_exception .


    METHODS get_cds_views_from_sadl_xml
      IMPORTING sadl_xml              TYPE string
      RETURNING VALUE(cds_view_names) TYPE table_of_strings.

    METHODS create_get_entity_set_methods
      IMPORTING
        cds_view_names                TYPE table_of_strings
      CHANGING
        generated_code_definition     TYPE rswsourcet
        generated_code_implementation TYPE rswsourcet
        generated_types_definition    TYPE rswsourcet.

    METHODS create_generic_get_methods
      IMPORTING
        cds_view_names                TYPE table_of_strings
      CHANGING
        generated_code_definition     TYPE rswsourcet
        generated_code_implementation TYPE rswsourcet.

  PROTECTED SECTION. " Visible only in child classes

  PRIVATE SECTION. " Visible only internally

    DATA segw_project_name TYPE  /iwbep/i_sbd_ga-project.
    DATA segw_project      TYPE REF TO /iwbep/if_sbdm_project.

    CONSTANTS suffix_length TYPE i VALUE 3.
    CONSTANTS cds_view_name_length TYPE i VALUE 30.

    DATA dpc_ext_class_name TYPE string.
    DATA mpc_ext_class_name TYPE string.
    DATA dpc_class_name TYPE string.
    DATA mpc_class_name TYPE string.

ENDCLASS.

CLASS cl_sadl_based_segw_project IMPLEMENTATION.

  METHOD constructor.

    DATA:service_builder_dm_factory TYPE REF TO /iwbep/if_sbdm_factory,
         service_builder_dm_project TYPE REF TO /iwbep/if_sbdm_project,
         service_builder_dm_manager TYPE REF TO /iwbep/if_sbdm_manager,
         lt_range                   TYPE /iwbep/if_sbdm_manager=>ty_t_range_prjct_name.

    segw_project_name = io_segw_project_name.

    service_builder_dm_factory = /iwbep/cl_sbdm=>get_factory( ).
    service_builder_dm_manager = /iwbep/cl_sbdm=>get_manager( ).
    lt_range  = VALUE #( (
                           sign = 'I'
                           option = 'EQ'
                           low =  segw_project_name
                        ) ).
    DATA error_message_text TYPE string.

    READ TABLE service_builder_dm_manager->find_projects( lt_range ) INTO segw_project INDEX 1.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE cx_demo_dyn_t100
        MESSAGE ID '/IWBEP/SBUIP'
        TYPE 'E'
        NUMBER '001'
        WITH segw_project_name.
    ENDIF.



    LOOP AT segw_project->get_generated_artifacts(  ) INTO DATA(generated_artifact).
      IF generated_artifact->get_tadir_data(  )-trobj_type = 'CLAS'.
        DATA(class_name) = generated_artifact->get_tadir_data(  )-trobj_name.
        DATA(mpc_ext_suffix) =  substring( val = class_name len = strlen( '_MPC_EXT' )  off = strlen( class_name ) - strlen( '_MPC_EXT' ) ).
        DATA(dpc_ext_suffix) =  substring( val = class_name len = strlen( '_DPC_EXT' )  off = strlen( class_name ) - strlen( '_DPC_EXT' ) ).
        DATA(mpc_suffix) =  substring( val = class_name len = strlen( '_MPC' )  off = strlen( class_name ) - strlen( '_MPC' ) ).
        DATA(dpc_suffix) =  substring( val = class_name len = strlen( '_DPC' )  off = strlen( class_name ) - strlen( '_DPC' ) ).

        IF mpc_ext_suffix = '_MPC_EXT'.
          mpc_ext_class_name = generated_artifact->get_tadir_data(  )-trobj_name.
        ELSEIF dpc_ext_suffix = '_DPC_EXT'.
          dpc_ext_class_name = generated_artifact->get_tadir_data(  )-trobj_name.
        ELSEIF dpc_suffix = '_DPC'.
          dpc_class_name = generated_artifact->get_tadir_data(  )-trobj_name.
        ELSEIF dpc_suffix = '_MPC'.
          mpc_class_name = generated_artifact->get_tadir_data(  )-trobj_name.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_sadl_xml.
    DATA   lo_service_api TYPE REF TO cl_sadl_gw_service_api  .
    TRY.
        lo_service_api = cl_sadl_gw_service_api=>get_service_api( io_project = segw_project ).
      CATCH /iwbep/cx_sbcm_exception.
        "handle exception
    ENDTRY.
    TRY.
        lo_service_api->get_and_update_sadl_xml( IMPORTING ev_sadl_xml = sadl_xml ).
      CATCH /iwbep/cx_sbcm_exception.
        "handle exception
    ENDTRY.
    "lo_service_api->get_sadl( IMPORTING es_sadl = DATA(sadl_definition) ).
  ENDMETHOD.

  METHOD get_merged_sadl_xml.

    DATA string_1 TYPE string.
    DATA remaining_string TYPE string.
    DATA data_sources_string TYPE string.
    DATA data_sources_target_string TYPE string.
    DATA structures_string TYPE string.
    DATA structures_target_string TYPE string.
    DATA mv_sadl_xml_merged TYPE string.


    SPLIT sadl_xml_source_project  AT '<sadl:dataSource' INTO string_1 remaining_string.
    SPLIT remaining_string AT '<sadl:resultSet>' INTO data_sources_string remaining_string.
    data_sources_string = '<sadl:dataSource' && data_sources_string.
    SPLIT remaining_string AT '</sadl:resultSet>' INTO structures_string remaining_string.


    SPLIT sadl_xml_extended_project AT '<sadl:dataSource' INTO string_1 remaining_string.
    SPLIT remaining_string AT '<sadl:resultSet>' INTO data_sources_target_string remaining_string.
    data_sources_target_string = '<sadl:dataSource' && data_sources_target_string.
    SPLIT remaining_string AT '</sadl:resultSet>' INTO structures_target_string remaining_string.


    sadl_xml_merged   = string_1 &&
                         data_sources_string &&
                         data_sources_target_string &&
                         '<sadl:resultSet>' &&
                         structures_string &&
                         structures_target_string &&
                         '</sadl:resultSet>' &&
                         remaining_string.

    WRITE : / mv_sadl_xml_merged.

  ENDMETHOD.

  METHOD insert_sadl_xml_into_dpc.

    DATA(lv_timestamp)  = cl_sadl_gw_model_sb_gen_mpc_pr=>get_generation_timestamp( segw_project ).


    APPEND |    METHODS if_sadl_gw_dpc_util~get_dpc REDEFINITION .| TO  generated_code_definition  ##NO_TEXT.

    APPEND | METHOD if_sadl_gw_dpc_util~get_dpc. | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |    CONSTANTS: co_gen_timestamp TYPE timestamp VALUE '{ lv_timestamp }'.  | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |    DATA(lv_sadl_xml) =                                                   | TO  generated_code_implementation  ##NO_TEXT.

* === Large SADL must be split, we split it after 50 rows
    SPLIT iv_sadl_xml AT '>'  INTO TABLE DATA(lt_string).
    DATA(lv_lines) = lines( lt_string ).
    LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<s_line>).
      <s_line> = |               \|{ <s_line> }>\| |.
      IF sy-tabix = lv_lines.         " Last line
        <s_line> = |{ <s_line> }.|.
      ELSE.
        IF sy-tabix MOD 50 = 0.
          <s_line> = |{ <s_line> }.|.
          APPEND <s_line> TO  generated_code_implementation .
          <s_line> = |      lv_sadl_xml = \|\{ lv_sadl_xml \}\| &| ##no_text.
        ELSE.
          <s_line> = |{ <s_line> } &|.
        ENDIF.
      ENDIF.
      APPEND <s_line> TO  generated_code_implementation .
    ENDLOOP.


    APPEND |   ro_dpc = cl_sadl_gw_dpc_factory=>create_for_sadl( iv_sadl_xml   = lv_sadl_xml | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |          iv_timestamp         = co_gen_timestamp                                | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |          iv_uuid              = CONV #( '{ iv_src_segw_project_name }' )        | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |          io_context           = me->mo_context ).                               | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |ENDMETHOD.| TO  generated_code_implementation  ##NO_TEXT.

  ENDMETHOD.


  METHOD get_cds_views_from_sadl_xml.

    DATA(ls_sadl_definition) = cl_bsa_sadl_m2m_transformer=>get_instance( )->xml_to_abap_structure( iv_sadl_xml = sadl_xml ).

    LOOP AT ls_sadl_definition-structures REFERENCE INTO DATA(lr_structure).
      " Entry in structures correlates to entry in data sources
      READ TABLE ls_sadl_definition-data_sources WITH KEY name = lr_structure->data_source
                                                          type = 'CDS' TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 AND lr_structure->exposure = 'TRUE'.
        "INSERT CONV #( lr_structure->data_source ) INTO TABLE cds_view_names.
        INSERT CONV #( lr_structure->name ) INTO TABLE cds_view_names.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD create_get_entity_set_methods.

*    DATA generated_code_definition TYPE rswsourcet .
*    DATA generated_code_implementation TYPE rswsourcet .
    DATA method_name_get_entityset TYPE string.
    DATA method_name_get_entity TYPE string.
    DATA type_name_entity TYPE string.
    DATA type_name_entityset TYPE string.
    DATA tab_name_entity TYPE string.
    DATA tab_name_entityset TYPE string.
    "DATA generated_types_definition TYPE rswsourcet  .


    LOOP AT  cds_view_names  INTO DATA(cds_view_name).
      "leave 2 characters to generate a unique name '<...>_get_entityset'

      DATA(get_entityset_suffix) = '_get_entityset'.
      DATA(get_entity_suffix) = '_get_entity'.
      DATA(tab_entityset_suffix) = '_entityset'.
      DATA(tab_entity_suffix) = '_entity'.
      DATA(mandatory_name_components) = get_entityset_suffix.

      DATA(length_mandatory_name_comp) = strlen( mandatory_name_components ).
      DATA(remaining_num_characters) =  cds_view_name_length - length_mandatory_name_comp.

      IF strlen( cds_view_name ) > remaining_num_characters - suffix_length.
        method_name_get_entityset = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && get_entityset_suffix.
        method_name_get_entity = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && get_entity_suffix.
        type_name_entity = 'TS_' && substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) .
        type_name_entityset = 'TT_' && substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) .
        tab_name_entityset = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && tab_entityset_suffix.
        tab_name_entity = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && tab_entity_suffix.
      ELSE.
        method_name_get_entityset = cds_view_name && get_entityset_suffix.
        method_name_get_entity = cds_view_name && get_entity_suffix.
        type_name_entity = 'TS_' && cds_view_name.
        type_name_entityset = 'TT_' && cds_view_name .
        tab_name_entityset = cds_view_name && tab_entityset_suffix.
        tab_name_entity = cds_view_name && tab_entity_suffix.
      ENDIF.

      APPEND |  METHODS { method_name_get_entity } | TO generated_code_definition ##NO_TEXT.
      APPEND |      IMPORTING | TO generated_code_definition ##NO_TEXT.
      APPEND |        !io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entity OPTIONAL | TO generated_code_definition ##NO_TEXT.
      APPEND |      EXPORTING | TO generated_code_definition ##NO_TEXT.
      APPEND |        !er_entity               TYPE { mpc_class_name }=>TS_  "use code completion to select correct type from MPC . | TO generated_code_definition ##NO_TEXT.
      APPEND |        !es_response_context     TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_entity_cntxt | TO generated_code_definition ##NO_TEXT.
      APPEND |      RAISING | TO generated_code_definition ##NO_TEXT.
      APPEND |        /iwbep/cx_mgw_busi_exception | TO generated_code_definition ##NO_TEXT.
      APPEND |        /iwbep/cx_mgw_tech_exception . | TO generated_code_definition ##NO_TEXT.

      APPEND |     METHODS { method_name_get_entityset }  | TO generated_code_definition ##NO_TEXT.
      APPEND |       IMPORTING | TO generated_code_definition ##NO_TEXT.
      APPEND |         !io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL | TO generated_code_definition ##NO_TEXT.
      APPEND |       EXPORTING | TO generated_code_definition ##NO_TEXT.
      APPEND |         !et_entityset            TYPE  { mpc_class_name }=>TT_  "use code completion to select correct type from MPC .  | TO generated_code_definition ##NO_TEXT.
      APPEND |         !es_response_context     TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context | TO generated_code_definition ##NO_TEXT.
      APPEND |       RAISING | TO generated_code_definition ##NO_TEXT.
      APPEND |         /iwbep/cx_mgw_busi_exception | TO generated_code_definition ##NO_TEXT.
      APPEND |         /iwbep/cx_mgw_tech_exception . | TO generated_code_definition ##NO_TEXT.

      " create code for implementation part

      APPEND | method { method_name_get_entity }. | TO generated_code_implementation ##NO_TEXT.
      APPEND |   if_sadl_gw_dpc_util~get_dpc( )->get_entity( EXPORTING io_tech_request_context = io_tech_request_context | TO generated_code_implementation ##NO_TEXT.
      APPEND |                                               IMPORTING es_data                 = er_entity ). | TO generated_code_implementation ##NO_TEXT.
      APPEND | endmethod. | TO generated_code_implementation ##NO_TEXT.

      APPEND | method { method_name_get_entityset }. | TO generated_code_implementation ##NO_TEXT.
      APPEND |    if_sadl_gw_dpc_util~get_dpc( )->get_entityset( EXPORTING io_tech_request_context = io_tech_request_context  | TO generated_code_implementation ##NO_TEXT.
      APPEND |                                                   IMPORTING et_data                 = et_entityset  | TO generated_code_implementation ##NO_TEXT.
      APPEND |                                                             es_response_context     = es_response_context ). | TO generated_code_implementation ##NO_TEXT.
      APPEND |  endmethod. | TO generated_code_implementation ##NO_TEXT.



    ENDLOOP.


  ENDMETHOD.

  METHOD create_generic_get_methods.

    DATA method_name_get_entityset TYPE string.
    DATA method_name_get_entity TYPE string.
    DATA type_name_entity TYPE string.
    DATA type_name_entityset TYPE string.
    DATA tab_name_entity TYPE string.
    DATA tab_name_entityset TYPE string.

    DATA generated_code_get_entity  TYPE rswsourcet.
    DATA generated_code_get_entityset  TYPE rswsourcet.

    DATA(tab_entityset_suffix) = '_entityset'.
    DATA(tab_entity_suffix) = '_entity'.


* APPEND | CLASS  dpc_ext_class  DEFINITION    | TO generated_code_definition ##NO_TEXT.
    APPEND | METHODS /iwbep/if_mgw_appl_srv_runtime~get_entityset REDEFINITION . | TO generated_code_definition ##NO_TEXT.
    APPEND | METHODS /iwbep/if_mgw_appl_srv_runtime~get_entity REDEFINITION . | TO generated_code_definition ##NO_TEXT.
*
*    APPEND | CLASS dpc_ext_class IMPLEMENTATION.| TO generated_code_implementation ##NO_TEXT.

    CLEAR generated_code_get_entityset.
    APPEND | METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset. | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |    DATA(my_entity_name) = io_tech_request_context->get_entity_set_name( ). | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |    case io_tech_request_context->get_entity_set_name( ). | TO generated_code_get_entityset ##NO_TEXT.

    CLEAR generated_code_get_entity .
    APPEND | METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity. | TO generated_code_get_entity ##NO_TEXT.
    APPEND |    DATA(my_entity_name) = io_tech_request_context->get_entity_set_name( ). | TO generated_code_get_entity ##NO_TEXT.
    APPEND |    case io_tech_request_context->get_entity_set_name( ). | TO generated_code_get_entity ##NO_TEXT.


    LOOP AT  cds_view_names  INTO DATA(cds_view_name).
      "leave 2 characters to generate a unique name '<...>_get_entityset'

      DATA(get_entityset_suffix) = '_get_entityset'.
      DATA(get_entity_suffix) = '_get_entity'.
      DATA(mandatory_name_components) = get_entityset_suffix.

      DATA(length_mandatory_name_comp) = strlen( mandatory_name_components ).
      DATA(remaining_num_characters) =  cds_view_name_length - length_mandatory_name_comp.


      IF strlen( cds_view_name ) > remaining_num_characters - suffix_length.
        method_name_get_entityset = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && get_entityset_suffix.
        method_name_get_entity = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && get_entity_suffix.
        type_name_entity = 'TS_' && substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) .
        type_name_entityset = 'TT_' && substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) .
        tab_name_entityset = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && tab_entityset_suffix.
        tab_name_entity = substring( val = cds_view_name len =  remaining_num_characters - suffix_length ) && tab_entity_suffix.
      ELSE.
        method_name_get_entityset = cds_view_name && get_entityset_suffix.
        method_name_get_entity = cds_view_name && get_entity_suffix.
        type_name_entity = 'TS_' && cds_view_name.
        type_name_entityset = 'TT_' && cds_view_name .
        tab_name_entityset = cds_view_name && tab_entityset_suffix.
        tab_name_entity = cds_view_name && tab_entity_suffix.
      ENDIF.



      APPEND |WHEN '{ cds_view_name }'.  |  TO generated_code_get_entityset ##NO_TEXT.

      "APPEND |data { tab_name_entityset } type { type_name_entityset }.| TO generated_code_get_entityset ##NO_TEXT.

      APPEND | { method_name_get_entityset }( EXPORTING io_tech_request_context = io_tech_request_context | TO generated_code_get_entityset ##NO_TEXT.
      APPEND |                                IMPORTING et_entityset               = data({ tab_name_entityset }) | TO generated_code_get_entityset ##NO_TEXT.
      APPEND |                                  es_response_context     = es_response_context ). | TO generated_code_get_entityset ##NO_TEXT.

      APPEND |   IF { tab_name_entityset } IS NOT INITIAL.  | TO generated_code_get_entityset ##NO_TEXT.
*     Send specific entity data to the caller interface
      APPEND |      copy_data_to_ref( EXPORTING is_data = { tab_name_entityset } |  TO generated_code_get_entityset ##NO_TEXT.
      APPEND |                       CHANGING  cr_data = er_entityset ).  | TO generated_code_get_entityset ##NO_TEXT.
      APPEND |   ENDIF. | TO generated_code_get_entityset ##NO_TEXT.


      APPEND |WHEN '{ cds_view_name }'.  |  TO generated_code_get_entity ##NO_TEXT.

      "APPEND |data { tab_name_entity } type { type_name_entity }.| TO generated_code_get_entity ##NO_TEXT.

      APPEND | { method_name_get_entity }( EXPORTING io_tech_request_context = io_tech_request_context | TO generated_code_get_entity ##NO_TEXT.
      APPEND |                                IMPORTING er_entity               = data({ tab_name_entity }) | TO generated_code_get_entity ##NO_TEXT.
      APPEND |                                  es_response_context     = es_response_context ). | TO generated_code_get_entity ##NO_TEXT.

      APPEND |   IF { tab_name_entity } IS NOT INITIAL.  | TO generated_code_get_entity ##NO_TEXT.
*     Send specific entity data to the caller interface
      APPEND |      copy_data_to_ref( EXPORTING is_data = { tab_name_entity } |  TO generated_code_get_entity ##NO_TEXT.
      APPEND |                       CHANGING  cr_data = er_entity ).  | TO generated_code_get_entity ##NO_TEXT.
      APPEND |   ENDIF. | TO generated_code_get_entity ##NO_TEXT.

    ENDLOOP.

    APPEND | WHEN OTHERS.  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |    super->/iwbep/if_mgw_appl_srv_runtime~get_entity( EXPORTING iv_entity_name          = iv_entity_name  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                               iv_entity_set_name      = iv_entity_set_name  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                               iv_source_name          = iv_source_name  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                               it_key_tab              = it_key_tab  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                               it_navigation_path      = it_navigation_path  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                               io_tech_request_context = io_tech_request_context  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                      IMPORTING er_entity              = er_entity  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |                                                                es_response_context    = es_response_context ).  | TO generated_code_get_entity ##NO_TEXT.
    APPEND |  ENDCASE. | TO generated_code_get_entity ##NO_TEXT.
    APPEND |  ENDMETHOD. | TO generated_code_get_entity ##NO_TEXT.


    APPEND |  WHEN OTHERS. | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |   super->/iwbep/if_mgw_appl_srv_runtime~get_entityset( EXPORTING iv_entity_name           = iv_entity_name | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             iv_entity_set_name       = iv_entity_set_name | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             iv_source_name           = iv_source_name | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             it_filter_select_options = it_filter_select_options | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             it_order                 = it_order | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             is_paging                = is_paging | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             it_key_tab               = it_key_tab | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             it_navigation_path       = it_navigation_path | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             iv_filter_string         = iv_filter_string | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             iv_search_string         = iv_search_string | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             io_tech_request_context  = io_tech_request_context | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                   IMPORTING er_entityset             = er_entityset | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |                                                             es_response_context      = es_response_context ). | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |  ENDCASE. | TO generated_code_get_entityset ##NO_TEXT.
    APPEND |  ENDMETHOD. | TO generated_code_get_entityset ##NO_TEXT.

    APPEND LINES OF generated_code_get_entity TO generated_code_implementation.
    APPEND LINES OF generated_code_get_entityset TO generated_code_implementation.


  ENDMETHOD.

  METHOD get_dpc_ext_class_name.
    class_name = dpc_ext_class_name.
  ENDMETHOD.

  METHOD get_mpc_ext_class_name.
    class_name = mpc_ext_class_name.
  ENDMETHOD.

  METHOD get_dpc_class_name.
    class_name = dpc_class_name.
  ENDMETHOD.

  METHOD get_mpc_class_name.
    class_name = mpc_class_name.
  ENDMETHOD.

  METHOD insert_sadl_xml_into_mpc_ext.

    DATA(lv_timestamp)  = cl_sadl_gw_model_sb_gen_mpc_pr=>get_generation_timestamp( segw_project ).


    APPEND |    METHODS define REDEFINITION. | TO  generated_code_definition  ##NO_TEXT.
    APPEND |    METHODS if_sadl_gw_model_exposure_data~get_model_exposure REDEFINITION. | TO  generated_code_definition  ##NO_TEXT.



    APPEND |  METHOD define.| TO  generated_code_implementation.
    APPEND |    super->define( ).| TO  generated_code_implementation.
    APPEND |  ENDMETHOD.| TO  generated_code_implementation.

    APPEND |  METHOD if_sadl_gw_model_exposure_data~get_model_exposure.| TO  generated_code_implementation.
    APPEND |    CONSTANTS: co_gen_timestamp TYPE timestamp VALUE '{ lv_timestamp }'.  | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |    DATA(lv_sadl_xml) =                                                   | TO  generated_code_implementation  ##NO_TEXT.

* === Large SADL must be split, we split it after 50 rows
    SPLIT iv_sadl_xml AT '>'  INTO TABLE DATA(lt_string).
    DATA(lv_lines) = lines( lt_string ).
    LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<s_line>).
      <s_line> = |               \|{ <s_line> }>\| |.
      IF sy-tabix = lv_lines.         " Last line
        <s_line> = |{ <s_line> }.|.
      ELSE.
        IF sy-tabix MOD 50 = 0.
          <s_line> = |{ <s_line> }.|.
          APPEND <s_line> TO  generated_code_implementation .
          <s_line> = |      lv_sadl_xml = \|\{ lv_sadl_xml \}\| &| ##no_text.
        ELSE.
          <s_line> = |{ <s_line> } &|.
        ENDIF.
      ENDIF.
      APPEND <s_line> TO  generated_code_implementation .
    ENDLOOP.


    APPEND |    ro_model_exposure = cl_sadl_gw_model_exposure=>get_exposure_xml( iv_uuid      = CONV #( '{ iv_src_segw_project_name }' ) | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |                                                                     iv_timestamp = co_gen_timestamp | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |                                                                     iv_sadl_xml  = lv_sadl_xml ). | TO  generated_code_implementation  ##NO_TEXT.
    APPEND |  ENDMETHOD. | TO  generated_code_implementation  ##NO_TEXT.






  ENDMETHOD.

  METHOD get_segw_project_name.
    project_name = segw_project_name.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  PARAMETERS: src_proj TYPE /iwbep/i_sbd_ga-project,
              ext_proj TYPE /iwbep/i_sbd_ga-project.



*data source_segw_project type REF TO cl_sadl_based_segw_project.
*data extended_segw_project type REF TO cl_sadl_based_segw_project.

  "todo zeitstempel von quell und zielsystem merken




  DATA filename TYPE string.
  DATA path_to_file TYPE string.
  DATA path_and_filename TYPE string.
  DATA useraction TYPE i.
  DATA gui_download_data TYPE TABLE OF string.
  DATA generated_code TYPE rswsourcet .
  DATA generated_dpc_code_definition TYPE rswsourcet .
  DATA get_entity_and_set_definition TYPE rswsourcet .
  DATA generated_dpc_code_implement TYPE rswsourcet .
  DATA generated_types_definition TYPE rswsourcet .

  DATA generated_mpc_code_definition TYPE rswsourcet .
  DATA generated_mpc_code_implement TYPE rswsourcet .

  TRY.

      DATA(source_segw_project) = NEW cl_sadl_based_segw_project( src_proj ).
      DATA(extended_segw_project) = NEW cl_sadl_based_segw_project( ext_proj ).

      "@todo: get name of DPC EXT Class from extended project
      DATA(extended_dpc_ext_class_name) = 'EXTENDED_DPC_EXT'.

      TRY.
          DATA(sadl_xml_source_project) = source_segw_project->get_sadl_xml( ).
          DATA(sadl_xml_extended_project) = extended_segw_project->get_sadl_xml( ).
        CATCH /iwbep/cx_sbcm_exception INTO DATA(get_sadl_xml_exception).
          WRITE: / |Exception occured: { get_sadl_xml_exception->get_text(  ) }|.
          EXIT.
      ENDTRY.

      DATA(merged_sadl_xml) = extended_segw_project->get_merged_sadl_xml(
                                sadl_xml_source_project   = sadl_xml_source_project
                                sadl_xml_extended_project = sadl_xml_extended_project
                              ).

      DATA(new_cds_view_names) = extended_segw_project->get_cds_views_from_sadl_xml( sadl_xml_extended_project  ).

      LOOP AT new_cds_view_names INTO DATA(new_cds_view_name).
        WRITE : / new_cds_view_name.
      ENDLOOP.

      WRITE : / |extended dpc_ext class name { extended_segw_project->get_dpc_ext_class_name(  ) }|.
      WRITE : / |extended mpc_ext class name { extended_segw_project->get_mpc_ext_class_name(  ) }|.
      WRITE : / |extended dpc class name { extended_segw_project->get_dpc_class_name(  ) }|.
      WRITE : / |extended mpc class name { extended_segw_project->get_mpc_class_name(  ) }|.
      "generated_dpc_code_definition

      APPEND | CLASS { extended_segw_project->get_dpc_ext_class_name(  ) } DEFINITION    | TO generated_dpc_code_definition ##NO_TEXT.
      APPEND | CLASS { extended_segw_project->get_dpc_ext_class_name(  ) } IMPLEMENTATION.| TO generated_dpc_code_implement ##NO_TEXT.


      APPEND | PUBLIC | TO generated_dpc_code_definition ##NO_TEXT.
      APPEND | INHERITING FROM { extended_segw_project->get_dpc_class_name(  ) } | TO generated_dpc_code_definition ##NO_TEXT.
      APPEND | CREATE PUBLIC . | TO generated_dpc_code_definition ##NO_TEXT.
      APPEND | PUBLIC SECTION. | TO generated_dpc_code_definition ##NO_TEXT.

      "get timestamp from extended project
      extended_segw_project->insert_sadl_xml_into_dpc(
        EXPORTING
          iv_sadl_xml       = merged_sadl_xml
          iv_src_segw_project_name = source_segw_project->get_segw_project_name(  )
        CHANGING
          generated_code_definition = generated_dpc_code_definition
          generated_code_implementation = generated_dpc_code_implement
      ).
*      CATCH /iwbep/cx_sbcm_exception.

      extended_segw_project->create_generic_get_methods(
        EXPORTING
          cds_view_names                = new_cds_view_names
        CHANGING
          generated_code_definition     = generated_dpc_code_definition
          generated_code_implementation = generated_dpc_code_implement
      ).

      APPEND | PROTECTED SECTION. | TO generated_dpc_code_definition ##NO_TEXT.

      extended_segw_project->create_get_entity_set_methods(
        EXPORTING
          cds_view_names    = new_cds_view_names
        CHANGING
          generated_types_definition = generated_types_definition
          generated_code_definition = get_entity_and_set_definition
          "generated_code_definition = generated_dpc_code_definition
          generated_code_implementation = generated_dpc_code_implement
      ).


      APPEND LINES OF generated_types_definition TO generated_dpc_code_definition.
      APPEND LINES OF get_entity_and_set_definition TO generated_dpc_code_definition.


      APPEND |         ENDCLASS . | TO generated_dpc_code_definition ##NO_TEXT.
      APPEND |         ENDCLASS . | TO generated_dpc_code_implement ##NO_TEXT.

      APPEND |CLASS { extended_segw_project->get_mpc_ext_class_name(  ) } DEFINITION | TO  generated_mpc_code_definition  ##NO_TEXT.
      APPEND |PUBLIC | TO  generated_mpc_code_definition  ##NO_TEXT.
      APPEND |INHERITING FROM { extended_segw_project->get_mpc_class_name(  ) }  | TO  generated_mpc_code_definition  ##NO_TEXT.
      APPEND |CREATE PUBLIC .  | TO  generated_mpc_code_definition  ##NO_TEXT.
      APPEND |  PUBLIC SECTION. | TO  generated_mpc_code_definition  ##NO_TEXT.

      APPEND |CLASS { extended_segw_project->get_mpc_ext_class_name(  ) } IMPLEMENTATION. | TO  generated_mpc_code_implement  ##NO_TEXT.

      extended_segw_project->insert_sadl_xml_into_mpc_ext(
             EXPORTING
               iv_sadl_xml       = merged_sadl_xml
               iv_src_segw_project_name = source_segw_project->get_segw_project_name(  )
             CHANGING
               generated_code_definition = generated_mpc_code_definition
               generated_code_implementation = generated_mpc_code_implement
           ).

      APPEND |  PROTECTED SECTION. | TO  generated_mpc_code_definition ##NO_TEXT.
      APPEND |  PRIVATE SECTION. | TO  generated_mpc_code_definition  ##NO_TEXT.
      APPEND |ENDCLASS. | TO  generated_mpc_code_definition  ##NO_TEXT.

      APPEND |ENDCLASS. | TO  generated_mpc_code_implement  ##NO_TEXT.

      LOOP AT generated_mpc_code_definition INTO DATA(generated_code_line).
        APPEND generated_code_line TO gui_download_data.
      ENDLOOP.

      LOOP AT generated_mpc_code_implement INTO generated_code_line.
        APPEND generated_code_line TO gui_download_data.
      ENDLOOP.

      APPEND '**************************'  TO gui_download_data.
      APPEND '* DPC_EXT Code goes here *'  TO gui_download_data.
      APPEND '**************************'  TO gui_download_data.

      LOOP AT generated_dpc_code_definition INTO generated_code_line.
        APPEND generated_code_line TO gui_download_data.
      ENDLOOP.

      LOOP AT generated_dpc_code_implement INTO generated_code_line.
        APPEND generated_code_line TO gui_download_data.
      ENDLOOP.

* Show the file download dialog
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title         = | ABAP sourcecode for { ext_proj } |
          default_file_name    = |{ extended_segw_project->get_segw_project_name(  ) }_MPC_EXT_and_DPC_EXT|
          default_extension    = 'txt'
        CHANGING
          filename             = filename
          path                 = path_to_file
          fullpath             = path_and_filename
          user_action          = useraction
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.

      IF useraction <> cl_gui_frontend_services=>action_ok.
        EXIT.
      ENDIF.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename = filename
        CHANGING
          data_tab = gui_download_data
        EXCEPTIONS
          OTHERS   = 24.

    CATCH cx_root INTO DATA(sadl_merge_exception).

      WRITE: / |Exception occured: { sadl_merge_exception->get_text(  ) }|.

  ENDTRY.
