function z_onlineshop_sdk_bus.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_ORDER) TYPE  ZAONLINESHOP_MSF-ORDERID
*"     VALUE(IM_CREATEDBY) TYPE  ZAONLINESHOP_MS1-CREATED_BY
*"----------------------------------------------------------------------
data lv_orderid type char10.
constants : gc_interface     type zinterface_id   VALUE 'AZEGT_P00',
            gc_event_type    type ZADF_EGRID_TYP  value 'onlineshop.order.created',
            gc_event_subject type ZADF_EGRID_SUBJ value 'OnlineShop/Order/Created',
            gc_event         type sibfevent value 'created',
            gc_utc_zone      type TZNZONE value 'UTC',
            gc_sep_hyphen    type char1 value '-',
            gc_sep_colon     type char1 value ':'.


"Type Definitions
TYPES: BEGIN OF lty_event_data,
  ordernr       type ZAONLINESHOP_MSF-ORDERID,
  createdby     type zr_onlineshop_ms1-CreatedBy,
  event         TYPE SIBFEVENT,
  date          TYPE dats,
  time          TYPE tims,
END OF lty_event_data.

Types: begin of lty_schema_data,
  topic             type ZADF_EGRID_TOPIC,
  SUBJECT           type ZADF_EGRID_SUBJ,
  EVENTTYPE         type ZADF_EGRID_TYP,
  EVENTTIME         type ZADF_EGRID_ETIME,
  ID                type ZADF_EGRID_ID,
  DATAVERSION       type ZADF_EGRID_DVERSION,
  METADATAVERSION   type ZADF_EGRID_MDVERSION,
  DATA              type lty_event_data,
end of lty_schema_data.


DATA :
       lv_event_data  type lty_event_data,
       lv_schema_data type lty_schema_data,
       lv_payload     type lty_schema_data,
       lt_payload     type table of lty_schema_data.

DATA:
  lt_headers     TYPE tihttpnvp,
  wa_headers     TYPE LINE OF tihttpnvp,
  lv_string      TYPE string,
  lv_response    TYPE string,
  cx_interface   TYPE REF TO zcx_interace_config_missing,
  cx_http        TYPE REF TO zcx_http_client_failed,
  cx_adf_service TYPE REF TO zcx_adf_service,
  oref_eventgrid TYPE REF TO zcl_adf_service_eventgrid,
  oref           TYPE REF TO zcl_adf_service,
  filter         TYPE zbusinessid,
  lv_http_status TYPE i,
  lo_json        TYPE REF TO cl_trex_json_serializer,
  lv_json_string TYPE string,
  lv_xstring     TYPE xstring,
  utctime        type char100.

TRY.

**Calling Factory method to instantiate eventgrid client
    oref = zcl_adf_service_factory=>create( iv_interface_id = gc_interface
                                            iv_business_identifier = filter ).
    oref_eventgrid ?= oref.

**Setting Expiry time
    CALL METHOD oref_eventgrid->add_expiry_time
      EXPORTING
        iv_expiry_hour = 0
        iv_expiry_min  = 120
        iv_expiry_sec  = 0.

**Prepate the event data
    lv_event_data-ordernr    = im_order.
*    lv_event_data-createdby = sy-uname.
    lv_event_data-createdby  = im_createdby.
    lv_event_data-event      = gc_event.
    lv_event_data-date       = sy-datlo.
    lv_event_data-time       = sy-timlo.

    lv_payload-topic     = ''.
    lv_payload-id        = im_order. "OrderId
    lv_payload-subject   = gc_event_subject.
    lv_payload-eventtype = gc_event_type.
    lv_payload-data      = lv_event_data.
    lv_payload-dataversion    = '2.0'.
    lv_payload-metadataversion = '1'.

    GET TIME STAMP FIELD  DATA(lv_current_timestamp) .
    CONVERT TIME STAMP lv_current_timestamp TIME ZONE gc_utc_zone INTO DATE DATA(lv_date) TIME DATA(lv_time).
    CONCATENATE lv_date+0(4) gc_sep_hyphen lv_date+4(2) gc_sep_hyphen lv_date+6(2)
                'T' lv_time+0(2) gc_sep_colon lv_time+2(2) gc_sep_colon lv_time+4(2) 'Z'
                INTO utctime.
    lv_payload-eventtime = utctime.
    append lv_payload to lt_payload.

** Convert Data into json format
      /ui2/cl_json=>serialize(
         EXPORTING
           data             = lt_payload
           "compress         = abap_true
           "pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
           pretty_name = 'X'
         RECEIVING
           r_json           = lv_json_string ).


**Convert input string data to Xstring format
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_json_string
        IMPORTING
          buffer = lv_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
      ENDIF.

**Sending Converted SAP data to Azure eventgrid
      CALL METHOD oref_eventgrid->send
        EXPORTING
          request        = lv_xstring       "Input XSTRING of SAP Business Event data
          it_headers     = lt_headers        "Header attributes
        IMPORTING
          response       = lv_response       "Response from eventgrid
          ev_http_status = lv_http_status.   "Status


      IF lv_http_status NE '201' AND
         lv_http_status NE '200'.
        MESSAGE 'SAP data not sent to Azure eventgrid' TYPE 'E'.
      ELSE.
        MESSAGE 'SAP data sent to Azure eventgrid' TYPE 'I'.
      ENDIF.

  CATCH zcx_interace_config_missing INTO cx_interface.
    lv_string = cx_interface->get_text( ).
    MESSAGE lv_string TYPE 'E'.
  CATCH zcx_http_client_failed INTO cx_http .
    lv_string = cx_http->get_text( ).
    MESSAGE lv_string TYPE 'E'.
  CATCH zcx_adf_service INTO cx_adf_service.
    lv_string = cx_adf_service->get_text( ).
    MESSAGE lv_string TYPE 'E'.

ENDTRY.

ENDFUNCTION.
