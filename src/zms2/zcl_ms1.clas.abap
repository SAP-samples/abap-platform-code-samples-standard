CLASS zcl_ms1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  class-METHODS abap_sdk_read IMPORTING orderid type numc08
                                        createdby type zaonlineshop_ms1-created_by.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_ms1 IMPLEMENTATION.
  METHOD abap_sdk_read.
*  data lv_orderid type char10.
*CONSTANTS: gc_interface  type zinterface_id VALUE 'AZSBQ_P00',
*           gc_busobjtype type sbo_bo_type   value 'BUS2015' , "Purchase Requistion
*           gc_busobjcat  type sbo_type_cat  value 'BO',
*           gc_busobjid   type char10    value '0040099',
*           gc_eventtype  type sibfevent     value 'CREATED'.
*
*"Type Definitions
*TYPES: BEGIN OF lty_event,
*  busobj        TYPE sbo_bo_type,
*  busobjname    TYPE SBEH_BOTYP_TEXT,
*  objkey        TYPE SIBFBORIID,
*  event         TYPE SIBFEVENT,
*  date          TYPE dats,
*  time          TYPE tims,
*END OF lty_event.
*
*
*
*DATA: lv_event              type lty_event,
*      it_headers            TYPE tihttpnvp,
*      wa_headers            TYPE LINE OF tihttpnvp,
*      lv_busobj_type_name   type SBEH_BOTYP_TEXT,
*      lv_json_output        type string,
*      lv_string             TYPE string,
*      lv_response           TYPE string,
*      cx_interface          TYPE REF TO zcx_interace_config_missing,
*      cx_http               TYPE REF TO zcx_http_client_failed,
*      cx_adf_service        TYPE REF TO zcx_adf_service,
*      oref_servicebus       TYPE REF TO zcl_adf_service_servicebus,
*      oref                  TYPE REF TO zcl_adf_service,
*      filter                TYPE zbusinessid,
*      lv_http_status        TYPE i,
*      lo_json               TYPE REF TO cl_trex_json_serializer,
*      lv1_string            TYPE string,
*      lv_xstring            TYPE xstring.
*
*data: p_asdkid   type zinterface_id VALUE gc_interface,
*            p_botype   type sbo_bo_type value gc_busobjtype,
*            p_bocat    type sbo_type_cat value gc_busobjcat,
*            p_bokey    type char10 ,
*            p_boevn    type sibfevent value gc_eventtype.
**SELECTION-SCREEN end of BLOCK bl1.
**
**START-OF-SELECTION.
*
*SELECT SINGLE bo_type INTO lv_busobj_type_name
*    FROM sbo_i_bodef
*    WHERE object_name = p_botype
*    AND   object_type_category = p_bocat.
*p_bokey = orderid.
*IF sy-subrc EQ 0.
*
*  "Create the Event
*  lv_event-busobj     = p_botype.
*  lv_event-busobjname = lv_busobj_type_name.
*  lv_event-event      = p_boevn.
*  lv_event-objkey     = p_bokey.
*  lv_event-date       = sy-datlo.
*  lv_event-time       = sy-timlo.
*
*  "Convert to JSON
*  lv_json_output = /ui2/cl_json=>serialize( data     = lv_event
*                                            compress = abap_true
*                                            pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
*
*  TRY.
***Calling Factory method to instantiate eventhub client
*
*      oref = zcl_adf_service_factory=>create( iv_interface_id        = p_asdkid
*                                              iv_business_identifier = filter ).
*      oref_servicebus ?= oref.
*
***Setting Expiry time
*      CALL METHOD oref_servicebus->add_expiry_time
*        EXPORTING
*          iv_expiry_hour = 0
*          iv_expiry_min  = 15
*          iv_expiry_sec  = 0.
*
*
***Convert input string data to Xstring format
*      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*        EXPORTING
*          text   = lv_json_output
*        IMPORTING
*          buffer = lv_xstring
*        EXCEPTIONS
*          failed = 1
*          OTHERS = 2.
*      IF sy-subrc <> 0.
*      ENDIF.
*
** Add message headers
*    CLEAR it_headers.
*    wa_headers-name = 'BrokerProperties'.
*    wa_headers-value = '{"Label":"OnlineShopEvent"}'.
*    APPEND wa_headers TO it_headers.
*    CLEAR  wa_headers.
*
***Sending Converted SAP data to Azure Service Bus
*     oref_servicebus->send( EXPORTING request        = lv_xstring   "Input XSTRING of SAP Business data
*                                      it_headers     = it_headers        "Header attributes
*                            IMPORTING response       = lv_response       "Response from Service Bus
*                                     ev_http_status = lv_http_status ). "Status
*
*    CATCH zcx_interace_config_missing INTO cx_interface.
*      lv_string = cx_interface->get_text( ).
*      MESSAGE lv_string TYPE 'E'.
*    CATCH zcx_http_client_failed INTO cx_http .
*      lv_string = cx_http->get_text( ).
*      MESSAGE lv_string TYPE 'E'.
*    CATCH zcx_adf_service INTO cx_adf_service.
*      lv_string = cx_adf_service->get_text( ).
*      MESSAGE lv_string TYPE 'E'.
*  ENDTRY.
*
*  IF lv_http_status NE '201' AND
*     lv_http_status NE '200'.
*    MESSAGE 'SAP data not sent to Azure ServiceBus' TYPE 'E'.
*  ELSE.
*    MESSAGE 'SAP data sent to Azure ServiceBus' TYPE 'I'.
*  ENDIF.
*else.
*   MESSAGE 'Event Data nof found' type 'E'.
*endif.
call FUNCTION 'Z_ONLINESHOP_SDK_BUS' IN BACKGROUND TASK
  EXPORTING
    im_order = orderid
    im_createdby = createdby

  .
  ENDMETHOD.

ENDCLASS.
