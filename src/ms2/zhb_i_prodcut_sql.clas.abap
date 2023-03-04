CLASS zhb_i_prodcut_sql DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zhb_i_prodcut_sql IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
  select * from zhb_i_product INTO table @data(lt_product)  .
*  value #( PRODUCT = '000000000000002097' PRODUCTGROUP = 'L003' )
  data(a) = 1.
  ENDMETHOD.
ENDCLASS.
